-- | Conversión de nombres libres a átomos Prolog válidos.
--
-- Funciones puras: pasan a minúsculas, quitan acentos, reemplazan todo
-- carácter no alfanumérico por guion bajo y, si el átomo empieza con dígito,
-- lo encierran entre comillas simples.
--
-- @ingroup capa_domain
module Domain.Atomo
  ( aAtomo
  , escaparAtomo
  ) where

import Data.Char (isAlphaNum, isAscii, isDigit, toLower)
import Data.List (dropWhileEnd)

-- | Convierte un nombre cualquiera en un átomo Prolog.
--
-- Ejemplos: `Ana` produce `ana`, `Maria Jose` produce `maria_jose`
-- y `2024` produce `'2024'`.
aAtomo :: String -> String
aAtomo s
  | null base = "sin_nombre"
  | isDigit (head base) = "'" ++ base ++ "'"
  | otherwise = base
  where
    base = recortar (colapsar (map normalizar s))
    normalizar c =
      let d = desacentuar (toLower c)
       in if isAscii d && isAlphaNum d then d else '_'
    colapsar ('_' : '_' : xs) = colapsar ('_' : xs)
    colapsar (x : xs) = x : colapsar xs
    colapsar [] = []
    recortar = dropWhileEnd (== '_') . dropWhile (== '_')

-- | Reemplaza vocales acentuadas, eñe y cedilla por su letra base.
desacentuar :: Char -> Char
desacentuar c = maybe c id (lookup c tabla)
  where
    tabla = zip "áàäâãéèëêíìïîóòöôõúùüûñç" "aaaaaeeeeiiiiooooouuuunc"

-- | Escapa barra invertida y comilla simple para usar texto dentro de un
-- átomo Prolog entre comillas simples.
escaparAtomo :: String -> String
escaparAtomo = concatMap f
  where
    f '\\' = "\\\\"
    f '\'' = "\\'"
    f c = [c]
