-- | Lectura y parseo sintáctico de `problema.txt` (capa Application).
--
-- El archivo se procesa en tres pasos, todos con funciones puras:
--
-- 1. se numeran las líneas y se divide el texto en secciones `[NOMBRE]`;
-- 2. cada línea de una seccion conocida se interpreta con su formato
--    (`tipo: Nombre`, `relacion: A, B` o `nombre_de_regla`);
-- 3. se arma un `ProblemaCrudo` con los datos correctos y los diagnósticos
--    de formato encontrados.
--
-- Este módulo solo valida la sintaxis; el significado (tipos, entidades
-- declaradas, reglas del dominio) lo valida `Domain.Validacion`.
--
-- @ingroup capa_application
module Application.Parser
  ( Seccion (..)
  , parsearProblema
  , dividirSecciones
  , trim
  ) where

import Data.Char (isAlphaNum, isSpace, toLower, toUpper)
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, isPrefixOf, isSuffixOf)
import Domain.Dominio (seccionesObligatorias)
import Domain.Types

-- | Una seccion del archivo: nombre en mayúsculas, línea del encabezado y
-- líneas de contenido (sin vacias ni comentarios).
data Seccion = Seccion
  { secNombre :: String
  , secLinea :: Int
  , secContenido :: [Fuente]
  } deriving (Show, Eq)

-- | Parsea el contenido completo del archivo de entrada.
--
-- Acepta saltos de línea de Windows y una marca BOM inicial.
parsearProblema :: String -> ProblemaCrudo
parsearProblema texto =
  ProblemaCrudo
    { crudoEntidades = entidadesOk
    , crudoRelaciones = relacionesOk
    , crudoReglas = reglasOk
    , crudoDiagnosticos = dSecciones ++ dFaltantes ++ dVacias ++ dEnt ++ dRel ++ dReg
    }
  where
    lineas = zip [1 ..] (lines (quitarBom texto))
    (secciones, dSecciones) = dividirSecciones lineas
    presentes = map secNombre secciones
    dFaltantes =
      [ mkError Nothing ("Falta la sección obligatoria [" ++ s ++ "]")
      | s <- seccionesObligatorias
      , s `notElem` presentes
      ]
    dVacias =
      [ mkAdvertencia (Just (secLinea s)) ("La sección [" ++ secNombre s ++ "] está vacía")
      | s <- secciones
      , secNombre s `elem` seccionesObligatorias
      , null (secContenido s)
      ]
    contenidoDe nombre = concatMap secContenido (filter ((== nombre) . secNombre) secciones)
    (dEnt, entidadesOk) = partitionEithers (map parsearEntidad (contenidoDe "ENTIDADES"))
    (dRel, relacionesOk) = partitionEithers (map parsearRelacion (contenidoDe "RELACIONES"))
    (dReg, reglasOk) = partitionEithers (map parsearRegla (contenidoDe "REGLAS_SOLICITADAS"))

-- | Divide las líneas numeradas en secciones.
--
-- Ignora líneas vacias y comentarios (que empiezan con `#`). Reporta
-- encabezados mal formados, líneas fuera de seccion, secciones repetidas y
-- secciones desconocidas (cuyo contenido se ignora).
dividirSecciones :: [(Int, String)] -> ([Seccion], [Diagnostico])
dividirSecciones = go Nothing [] []
  where
    go actual acc ds [] = (reverse (cerrar actual acc), reverse ds)
    go actual acc ds ((n, crudo) : resto)
      | null t || "#" `isPrefixOf` t = go actual acc ds resto
      | "[" `isPrefixOf` t = encabezado
      | otherwise =
          case actual of
            Nothing ->
              go actual acc (mkError (Just n) ("Línea fuera de toda sección: \"" ++ t ++ "\"") : ds) resto
            Just (nom, l, cont) ->
              go (Just (nom, l, Fuente n t : cont)) acc ds resto
      where
        t = trim crudo
        encabezado
          | "]" `isSuffixOf` t && not (null nombre) =
              go (Just (nombre, n, [])) (cerrar actual acc) (avisos ++ ds) resto
          | otherwise =
              go actual acc (mkError (Just n) ("Encabezado de sección mal formado: \"" ++ t ++ "\"") : ds) resto
          where
            nombre = map toUpper (trim (init (tail t)))
            vistos = map secNombre acc ++ [nom | Just (nom, _, _) <- [actual]]
            avisos =
              [ mkAdvertencia (Just n) ("Sección desconocida [" ++ nombre ++ "]; se ignora su contenido")
              | nombre `notElem` seccionesObligatorias
              ]
                ++ [ mkAdvertencia (Just n) ("Sección repetida [" ++ nombre ++ "]; se unen sus contenidos")
                   | nombre `elem` vistos
                   ]
    cerrar Nothing acc = acc
    cerrar (Just (nom, l, cont)) acc = Seccion nom l (reverse cont) : acc

-- | Interpreta una línea de `[ENTIDADES]`: `tipo: Nombre`.
parsearEntidad :: Fuente -> Either Diagnostico (Localizado (String, String))
parsearEntidad f@(Fuente n t) =
  case partirDosPuntos t of
    Nothing -> Left (formato n "se esperaba 'tipo: Nombre'" t)
    Just (k, v)
      | null k || null v -> Left (formato n "se esperaba 'tipo: Nombre'" t)
      | ',' `elem` v -> Left (formato n "una entidad lleva un solo nombre, sin comas" t)
      | otherwise -> Right (Localizado f (map toLower k, v))

-- | Interpreta una línea de `[RELACIONES]`: `relacion: Origen, Destino`.
parsearRelacion :: Fuente -> Either Diagnostico (Localizado (String, String, String))
parsearRelacion f@(Fuente n t) =
  case partirDosPuntos t of
    Just (k, v) | not (null k) ->
      case map trim (separar ',' v) of
        [a, b] | not (null a) && not (null b) -> Right (Localizado f (map toLower k, a, b))
        _ -> Left (formato n "se esperaba 'relacion: Origen, Destino'" t)
    _ -> Left (formato n "se esperaba 'relacion: Origen, Destino'" t)

-- | Interpreta una línea de `[REGLAS_SOLICITADAS]`: un identificador.
parsearRegla :: Fuente -> Either Diagnostico (Localizado String)
parsearRegla f@(Fuente n t)
  | not (null t) && all (\c -> isAlphaNum c || c == '_') t = Right (Localizado f (map toLower t))
  | otherwise = Left (formato n "el nombre de la regla solo admite letras, digitos y guion bajo" t)

-- | Error de formato con línea, detalle y texto original.
formato :: Int -> String -> String -> Diagnostico
formato n detalle t =
  mkError (Just n) ("Línea con formato inválido (" ++ detalle ++ "): \"" ++ t ++ "\"")

-- | Parte el texto en el primer `:` y recorta ambos lados.
partirDosPuntos :: String -> Maybe (String, String)
partirDosPuntos s =
  case break (== ':') s of
    (a, ':' : b) -> Just (trim a, trim b)
    _ -> Nothing

-- | Separa una cadena por un carácter delimitador.
separar :: Char -> String -> [String]
separar c s =
  case break (== c) s of
    (a, []) -> [a]
    (a, _ : r) -> a : separar c r

-- | Quita espacios (y retornos de carro) al inicio y al final.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Elimina la marca de orden de bytes UTF-8 si el archivo la trae.
quitarBom :: String -> String
quitarBom ('\xFEFF' : r) = r
quitarBom s = s
