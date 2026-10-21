-- | Acceso a archivos con codificación UTF-8 explícita (capa Infrastructure).
--
-- Es el único módulo, junto con `Main`, que realiza entrada y salida. La
-- codificación se fija para que los acentos funcionen igual en Windows,
-- Linux y macOS sin depender de la configuración regional.
--
-- @ingroup capa_infrastructure
module Infrastructure.Archivos
  ( leerUtf8
  , escribirUtf8
  ) where

import System.IO

-- | Lee un archivo completo como UTF-8 (lectura estricta).
leerUtf8 :: FilePath -> IO String
leerUtf8 ruta = do
  h <- openFile ruta ReadMode
  hSetEncoding h utf8
  s <- hGetContents h
  length s `seq` hClose h
  return s

-- | Escribe un archivo en UTF-8, reemplazando su contenido.
escribirUtf8 :: FilePath -> String -> IO ()
escribirUtf8 ruta contenido =
  withFile ruta WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h contenido
