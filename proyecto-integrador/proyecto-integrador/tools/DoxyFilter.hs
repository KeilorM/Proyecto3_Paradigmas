-- Filtro de entrada de Doxygen para codigo Haskell.
--
-- Doxygen no tiene analizador para Haskell. Este programa lee un archivo
-- .hs y escribe en la salida estandar un "esqueleto" tipo C++ que Doxygen
-- si entiende:
--
--   * un bloque de comentarios `-- |` antes de `module` se convierte en la
--     documentacion del archivo (\file);
--   * un bloque `-- |` antes de una firma `nombre :: Tipo` se convierte en
--     un comentario /** */ seguido de `void nombre();`, y la firma Haskell
--     original se incluye en la documentacion;
--   * lo mismo para `data`, `newtype` y `type` (se emiten como `struct`).
--
-- Se configura en el Doxyfile con FILTER_PATTERNS y se ejecuta con runghc.
module Main (main) where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.List (dropWhileEnd, isPrefixOf)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ruta] -> do
      h <- openFile ruta ReadMode
      hSetEncoding h utf8
      s <- hGetContents h
      hSetEncoding stdout utf8
      putStr (unlines (filtrar (map (dropWhileEnd (== '\r')) (lines s))))
    _ -> hPutStrLn stderr "uso: DoxyFilter archivo.hs"

filtrar :: [String] -> [String]
filtrar = go []
  where
    go _ [] = []
    go doc (l : ls)
      | Just d <- inicioDoc l =
          let (cont, resto) = span continuaDoc ls
           in go (d : map quitar cont) resto
      | all isSpace l = go [] ls
      | "module " `isPrefixOf` l = archivo doc ++ go [] ls
      | Just n <- nombreFirma l ls =
          let (cont, resto) = span indentada ls
           in bloque doc (l : cont) ++ ["void " ++ sanear n ++ "();"] ++ go [] resto
      | Just n <- nombreTipo l =
          let (cont, resto) = span indentada ls
           in bloque doc (l : cont) ++ ["struct " ++ n ++ " {};"] ++ go [] resto
      | otherwise = go [] ls

inicioDoc :: String -> Maybe String
inicioDoc l
  | "-- |" `isPrefixOf` l = Just (dropWhile (== ' ') (drop 4 l))
  | otherwise = Nothing

continuaDoc :: String -> Bool
continuaDoc l = "--" `isPrefixOf` l && not ("-- |" `isPrefixOf` l)

quitar :: String -> String
quitar l = case drop 2 l of
  (' ' : r) -> r
  r -> r

indentada :: String -> Bool
indentada (c : _) = c == ' ' || c == '\t'
indentada [] = False

identChar :: Char -> Bool
identChar c = isAlphaNum c || c == '_' || c == '\''

-- Reconoce `nombre :: Tipo` y tambien la forma con la firma en la linea
-- siguiente (`nombre` y luego `  :: Tipo`).
nombreFirma :: String -> [String] -> Maybe String
nombreFirma l siguientes =
  case span identChar l of
    (n@(c : _), resto)
      | esNombre c && "::" `isPrefixOf` dropWhile (== ' ') resto -> Just n
      | esNombre c && all isSpace resto, (s : _) <- siguientes, "::" `isPrefixOf` dropWhile (== ' ') s -> Just n
    _ -> Nothing
  where
    esNombre c = isLower c || c == '_'

nombreTipo :: String -> Maybe String
nombreTipo l =
  case words l of
    (kw : n : _)
      | kw `elem` ["data", "newtype", "type"], isUpper (head n) -> Just (takeWhile identChar n)
    _ -> Nothing

sanear :: String -> String
sanear = concatMap (\c -> if c == '\'' then "_prime" else [c])

seguro :: String -> String
seguro [] = []
seguro ('*' : '/' : r) = "* /" ++ seguro r
seguro (c : r) = c : seguro r

archivo :: [String] -> [String]
archivo doc = ["/**", " * \\file"] ++ map ((" * " ++) . seguro) doc ++ [" */"]

bloque :: [String] -> [String] -> [String]
bloque doc codigo =
  ["/**"]
    ++ map ((" * " ++) . seguro) doc
    ++ [" *", " * Declaración Haskell:", " * \\verbatim"]
    ++ map ((" * " ++) . seguro) codigo
    ++ [" * \\endverbatim", " */"]
