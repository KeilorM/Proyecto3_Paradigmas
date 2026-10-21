-- | Pruebas automáticas del pipeline (Haskell).
--
-- Mini marco de pruebas sin dependencias externas: cada caso es una
-- descripción y un booleano. Se ejecuta con `make test` o con
-- `cabal test` y termina con código de salida distinto de cero si falla alguno.
--
-- @ingroup capa_pruebas
module Main (main) where

import Application.Consultas
import Application.Parser (parsearProblema)
import Application.Pipeline
import Data.List (isInfixOf)
import Domain.Atomo
import Domain.Hechos
import Domain.Types
import Domain.Validacion (construirProblema)
import Infrastructure.PrologGen
import Infrastructure.Reporte
import Presentation.HtmlGen
import System.Exit (exitFailure, exitSuccess)

-- | Una prueba: descripción y resultado.
type Caso = (String, Bool)

-- | Generadores reales, igual que en `Main`.
generadores :: Generadores
generadores = Generadores generarConocimiento generarConsultasPl generarReporte generarHtml

-- | Ejecuta el pipeline completo sobre un texto.
correr :: String -> Resultado
correr = ejecutarPipeline generadores

-- | Construye el problema validado sobre un texto.
modelo :: String -> Problema
modelo = construirProblema . parsearProblema

-- | Entrada valida de referencia (líneas numeradas en comentarios).
entradaValida :: String
entradaValida =
  unlines
    [ "# comentario"            -- 1
    , "[ENTIDADES]"             -- 2
    , "persona: Ana"            -- 3
    , "persona: Luis"           -- 4
    , ""                        -- 5
    , "curso: Paradigmas"       -- 6
    , "docente: Maria"          -- 7
    , "[RELACIONES]"            -- 8
    , "lleva: Ana, Paradigmas"  -- 9
    , "lleva: Luis, Paradigmas" -- 10
    , "imparte: Maria, Paradigmas" -- 11
    , "[REGLAS_SOLICITADAS]"    -- 12
    , "companeros"              -- 13
    , "curso_con_docente"       -- 14
    ]

-- | Mensajes de un problema que contienen un texto.
hayMensaje :: String -> Problema -> Bool
hayMensaje t = any ((t `isInfixOf`) . diagMensaje) . advertencias

-- | Todos los casos de prueba.
casos :: [Caso]
casos =
  [ ( "entrada valida: cuenta entidades, relaciones y reglas"
    , let p = modelo entradaValida
       in (length (entidades p), length (relaciones p), length (reglasSolicitadas p)) == (4, 3, 2)
    )
  , ( "entrada valida: no produce diagnosticos"
    , null (advertencias (modelo entradaValida))
    )
  , ( "atomo: minusculas y sin acentos"
    , aAtomo "María José" == "maria_jose"
    )
  , ( "atomo: espacios, signos y guiones colapsados"
    , aAtomo "  Bases   de-Datos! " == "bases_de_datos"
    )
  , ( "atomo: empieza con digito se entrecomilla"
    , aAtomo "2024" == "'2024'"
    )
  , ( "atomo: nombre vacio"
    , aAtomo "???" == "sin_nombre"
    )
  , ( "escaparAtomo escapa comillas y barras"
    , escaparAtomo "O'Brien\\x" == "O\\'Brien\\\\x"
    )
  , ( "entidad duplicada: advertencia y se conserva una"
    , let p = modelo "[ENTIDADES]\npersona: Ana\npersona: ana\n[RELACIONES]\nlleva: x, y\n[REGLAS_SOLICITADAS]\ncompaneros\n"
       in length (entidades p) == 1 && hayMensaje "duplicada" p
    )
  , ( "relacion con entidad no declarada: error y se excluye"
    , let p = modelo "[ENTIDADES]\npersona: Ana\ncurso: C\n[RELACIONES]\nlleva: Pedro, C\n[REGLAS_SOLICITADAS]\ncompaneros\n"
       in null (relaciones p) && hayMensaje "no declarada" p && not (null (soloErrores p))
    )
  , ( "relacion con entidad de tipo incorrecto: error"
    , let p = modelo "[ENTIDADES]\ndocente: Maria\ncurso: C\n[RELACIONES]\nlleva: Maria, C\n[REGLAS_SOLICITADAS]\ncompaneros\n"
       in null (relaciones p) && hayMensaje "espera una entidad de tipo persona" p
    )
  , ( "relacion duplicada: advertencia"
    , let p = modelo "[ENTIDADES]\npersona: A\ncurso: C\n[RELACIONES]\nlleva: A, C\nlleva: A, C\n[REGLAS_SOLICITADAS]\ncompaneros\n"
       in length (relaciones p) == 1 && hayMensaje "Relación duplicada" p
    )
  , ( "linea con formato invalido: error con numero de linea"
    , let p = modelo "[ENTIDADES]\npersona Ana\n[RELACIONES]\nx\n[REGLAS_SOLICITADAS]\ncompaneros\n"
       in any (\d -> diagLinea d == Just 2 && esError d) (advertencias p)
    )
  , ( "seccion obligatoria ausente: error"
    , let p = modelo "[ENTIDADES]\npersona: Ana\n"
       in hayMensaje "[RELACIONES]" p && hayMensaje "[REGLAS_SOLICITADAS]" p
    )
  , ( "linea fuera de seccion: error"
    , hayMensaje "fuera de toda sección" (modelo "persona: Ana\n[ENTIDADES]\npersona: B\n[RELACIONES]\n[REGLAS_SOLICITADAS]\n")
    )
  , ( "regla de otro dominio: error y no se acepta"
    , let p = modelo "[ENTIDADES]\npersona: A\n[RELACIONES]\n[REGLAS_SOLICITADAS]\nhermanos\n"
       in null (reglasSolicitadas p) && hayMensaje "dominio familiar" p
    )
  , ( "regla desconocida: error"
    , hayMensaje "desconocida" (modelo "[ENTIDADES]\npersona: A\n[RELACIONES]\n[REGLAS_SOLICITADAS]\nmagia\n")
    )
  , ( "regla sin hechos de apoyo: advertencia"
    , hayMensaje "necesita hechos" (modelo "[ENTIDADES]\npersona: A\n[RELACIONES]\n[REGLAS_SOLICITADAS]\ncompaneros\n")
    )
  , ( "acepta CRLF y BOM"
    , let p = modelo ('\xFEFF' : concatMap (++ "\r\n") (lines entradaValida))
       in length (entidades p) == 4 && null (advertencias p)
    )
  , ( "encabezados en minusculas son validos"
    , length (entidades (modelo "[entidades]\npersona: A\n[relaciones]\n[reglas_solicitadas]\n")) == 1
    )
  , ( "trazabilidad: lineas de origen correctas"
    , map (fuenteLinea . origen) (entidades (modelo entradaValida)) == [3, 4, 6, 7]
    )
  , ( "hechos: formato Prolog"
    , map mostrarHecho (hechosDe (modelo entradaValida))
        == [ "persona(ana)."
           , "persona(luis)."
           , "curso(paradigmas)."
           , "docente(maria)."
           , "lleva(ana, paradigmas)."
           , "lleva(luis, paradigmas)."
           , "imparte(maria, paradigmas)."
           ]
    )
  , ( "conocimiento.pl: comentario de trazabilidad y hecho"
    , let k = resConocimientoPl (correr entradaValida)
       in "% linea 9: lleva: Ana, Paradigmas\nlleva(ana, paradigmas)." `isInfixOf` k
    )
  , ( "conocimiento.pl: declara predicados dinamicos y codificacion"
    , let k = resConocimientoPl (correr entradaValida)
       in ":- encoding(utf8)." `isInfixOf` k && ":- dynamic persona/1" `isInfixOf` k
    )
  , ( "conocimiento.pl: incluye reglas solicitadas"
    , let k = resConocimientoPl (correr entradaValida)
       in "companero(X, Y) :-" `isInfixOf` k && "curso_con_docente(Curso, Docente) :-" `isInfixOf` k
    )
  , ( "conocimiento.pl: no incluye reglas no solicitadas"
    , not ("aprobado(" `isInfixOf` resConocimientoPl (correr entradaValida))
    )
  , ( "conocimiento.pl: elementos invalidos no aparecen"
    , not ("pedro" `isInfixOf` resConocimientoPl (correr "[ENTIDADES]\npersona: Ana\ncurso: C\n[RELACIONES]\nlleva: Pedro, C\n[REGLAS_SOLICITADAS]\ncompaneros\n"))
    )
  , ( "consultas: incluye general y concreta por regla"
    , let ms = map consultaMeta (resConsultas (correr entradaValida))
       in "companero(X, Y)" `elem` ms && "companero(ana, X)" `elem` ms
    )
  , ( "consultas.pl: formato ejecutable"
    , let q = resConsultasPl (correr entradaValida)
       in "consulta('Todos los pares de compañeros', companero(X, Y))." `isInfixOf` q
            && "ejecutar_consultas :-" `isInfixOf` q
    )
  , ( "reporte: resume errores"
    , let r = resReporte (correr "[ENTIDADES]\npersona: A\n")
       in "ERRORES (2)" `isInfixOf` r && "se detectaron errores" `isInfixOf` r
    )
  , ( "reporte: entrada limpia"
    , "sin errores ni advertencias" `isInfixOf` resReporte (correr entradaValida)
    )
  , ( "html: contiene secciones obligatorias"
    , let h = resIndexHtml (correr entradaValida)
       in all (`isInfixOf` h) ["id='entidades'", "id='relaciones'", "id='hechos'", "id='reglas'", "id='consultas'", "id='diagnosticos'", "id='conocimiento'"]
    )
  , ( "html: refleja el contenido de la entrada"
    , let h = resIndexHtml (correr entradaValida)
       in "lleva(ana, paradigmas)." `isInfixOf` h && "companero(X, Y) :-" `isInfixOf` h
    )
  , ( "html: escapa caracteres especiales"
    , let h = resIndexHtml (correr "[ENTIDADES]\npersona: <b>X</b>\n[RELACIONES]\n[REGLAS_SOLICITADAS]\n")
       in "&lt;b&gt;X&lt;/b&gt;" `isInfixOf` h && not ("<b>X</b>" `isInfixOf` h)
    )
  , ( "regeneracion: otro problema.txt cambia todos los artefactos"
    , let a = correr entradaValida
          b = correr "[ENTIDADES]\npersona: Zoe\n[RELACIONES]\n[REGLAS_SOLICITADAS]\n"
       in "zoe" `isInfixOf` resConocimientoPl b
            && not ("ana" `isInfixOf` resConocimientoPl b)
            && "Zoe" `isInfixOf` resIndexHtml b
            && not ("Luis" `isInfixOf` resIndexHtml b)
            && resConsultasPl a /= resConsultasPl b
    )
  , ( "archivo vacio: no falla y reporta tres secciones ausentes"
    , length (soloErrores (modelo "")) == 3
    )
  ]

-- | Ejecuta todos los casos e imprime el resultado.
main :: IO ()
main = do
  let resultados = [(d, ok) | (d, ok) <- casos]
  mapM_ (\(d, ok) -> putStrLn ((if ok then "  [OK]    " else "  [FALLA] ") ++ d)) resultados
  let fallas = length (filter (not . snd) resultados)
  putStrLn ("\n" ++ show (length resultados - fallas) ++ "/" ++ show (length resultados) ++ " pruebas correctas")
  if fallas == 0 then exitSuccess else exitFailure
