-- | Punto de entrada y raíz de composición del pipeline.
--
-- Uso: `proyecto-integrador [problema.txt] [directorio_de_salida]`
--
-- Lee el archivo de entrada, ejecuta el pipeline puro conectando los
-- generadores de las capas externas y escribe `conocimiento.pl`,
-- `consultas.pl`, `index.html` y `reporte_validacion.txt`.
--
-- @ingroup capa_presentation
module Main (main) where

import Application.Pipeline
import Control.Exception (IOException, try)
import Domain.Types
import Infrastructure.Archivos
import Infrastructure.PrologGen
import Infrastructure.Reporte
import Presentation.HtmlGen
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO

-- | Generadores concretos que se inyectan al caso de uso.
generadores :: Generadores
generadores =
  Generadores
    { genConocimiento = generarConocimiento
    , genConsultasPl = generarConsultasPl
    , genReporte = generarReporte
    , genHtml = generarHtml
    }

-- | Programa principal.
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    ["-h"] -> ayuda
    ["--help"] -> ayuda
    [] -> ejecutar "problema.txt" "salida"
    [entrada] -> ejecutar entrada "salida"
    [entrada, salida] -> ejecutar entrada salida
    _ -> ayuda >> exitWith (ExitFailure 2)

-- | Muestra el modo de uso.
ayuda :: IO ()
ayuda =
  putStrLn "Uso: proyecto-integrador [problema.txt] [directorio_de_salida]\n\
           \Valores por defecto: problema.txt  y  salida/"

-- | Lee la entrada, ejecuta el pipeline y escribe los cuatro archivos generados.
ejecutar :: FilePath -> FilePath -> IO ()
ejecutar entrada salida = do
  leido <- try (leerUtf8 entrada) :: IO (Either IOException String)
  case leido of
    Left e -> do
      hPutStrLn stderr ("No se pudo leer \"" ++ entrada ++ "\" como UTF-8: " ++ show e)
      exitWith (ExitFailure 1)
    Right texto -> do
      let r = ejecutarPipeline generadores texto
          p = resProblema r
      createDirectoryIfMissing True salida
      escribirUtf8 (salida </> "conocimiento.pl") (resConocimientoPl r)
      escribirUtf8 (salida </> "consultas.pl") (resConsultasPl r)
      escribirUtf8 (salida </> "reporte_validacion.txt") (resReporte r)
      escribirUtf8 (salida </> "index.html") (resIndexHtml r)
      putStrLn ("Entrada:        " ++ entrada)
      putStrLn
        ( "Entidades: " ++ show (length (entidades p))
            ++ " | Relaciones: " ++ show (length (relaciones p))
            ++ " | Reglas: " ++ show (length (reglasSolicitadas p))
            ++ " | Errores: " ++ show (length (soloErrores p))
            ++ " | Advertencias: " ++ show (length (soloAdvertencias p))
        )
      mapM_ (putStrLn . ("  " ++) . resumenDiag) (advertencias p)
      putStrLn ("Archivos generados en " ++ salida ++ "/:")
      mapM_ (putStrLn . ("  - " ++)) ["conocimiento.pl", "consultas.pl", "index.html", "reporte_validacion.txt"]

-- | Línea de consola para un diagnóstico.
resumenDiag :: Diagnostico -> String
resumenDiag d =
  (if esError d then "[ERROR] " else "[AVISO] ")
    ++ maybe "" (\n -> "línea " ++ show n ++ ": ") (diagLinea d)
    ++ diagMensaje d
