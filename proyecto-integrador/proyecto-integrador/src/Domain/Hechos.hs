-- | Hechos logicos derivados del modelo.
--
-- Una única fuente de verdad: tanto el archivo `conocimiento.pl` como la
-- interfaz `index.html` muestran los hechos calculados aqui.
--
-- @ingroup capa_domain
module Domain.Hechos
  ( Hecho (..)
  , hechosDe
  , mostrarHecho
  ) where

import Data.List (intercalate)
import Domain.Atomo
import Domain.Dominio
import Domain.Types

-- | Un hecho Prolog: predicado, argumentos (ya como átomos) y línea de origen.
data Hecho = Hecho
  { hechoPredicado :: String
  , hechoArgumentos :: [String]
  , hechoOrigen :: Fuente
  } deriving (Show, Eq)

-- | Calcula los hechos del problema, agrupados por predicado para que el
-- archivo Prolog no tenga clausulas discontiguas.
hechosDe :: Problema -> [Hecho]
hechosDe p = deEntidades ++ deRelaciones
  where
    deEntidades =
      [ Hecho (etiquetaTipo t) [aAtomo (nombreEntidad e)] f
      | t <- todosLosTipos
      , Localizado f e <- entidades p
      , tipoDeEntidad e == t
      ]
    deRelaciones =
      [ Hecho n [aAtomo a, aAtomo b] f
      | (n, _) <- relacionesConocidas
      , Localizado f r <- relaciones p
      , nombreRelacion r == n
      , let (a, b) = extremosRelacion r
      ]

-- | Escribe el hecho en sintaxis Prolog, con punto final.
--
-- Ejemplo: `lleva(ana, paradigmas)` seguido de un punto.
mostrarHecho :: Hecho -> String
mostrarHecho (Hecho p args _) = p ++ "(" ++ intercalate ", " args ++ ")."
