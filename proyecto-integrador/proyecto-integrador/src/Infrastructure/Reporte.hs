-- | Generación de `reporte_validacion.txt` (capa Infrastructure).
--
-- Texto plano con el resumen del problema y la lista de errores y
-- advertencias ordenados por línea.
--
-- @ingroup capa_infrastructure
module Infrastructure.Reporte
  ( generarReporte
  ) where

import Data.List (sortOn)
import Domain.Dominio (nombreDominio)
import Domain.Types

-- | Genera el contenido del reporte de validación.
generarReporte :: Problema -> String
generarReporte p =
  unlines $
    [ "REPORTE DE VALIDACIÓN"
    , "====================="
    , ""
    , "Dominio:             " ++ nombreDominio
    , "Entidades válidas:   " ++ show (length (entidades p))
    , "Relaciones válidas:  " ++ show (length (relaciones p))
    , "Reglas válidas:      " ++ show (length (reglasSolicitadas p))
    , "Errores:             " ++ show (length errs)
    , "Advertencias:        " ++ show (length avisos)
    , ""
    ]
      ++ bloque "ERRORES" errs
      ++ bloque "ADVERTENCIAS" avisos
      ++ [veredicto]
  where
    errs = ordenar (soloErrores p)
    avisos = ordenar (soloAdvertencias p)
    ordenar = sortOn diagLinea
    bloque titulo ds =
      [encabezado, replicate (length encabezado) '-']
        ++ (if null ds then ["  (ninguno)"] else map linea ds)
        ++ [""]
      where
        encabezado = titulo ++ " (" ++ show (length ds) ++ ")"
    linea d = "  [" ++ maybe "global" (\n -> "línea " ++ show n) (diagLinea d) ++ "] " ++ diagMensaje d
    veredicto
      | not (null errs) =
          "RESULTADO: se detectaron errores. Los elementos inválidos se excluyeron de conocimiento.pl."
      | not (null avisos) = "RESULTADO: entrada válida, con advertencias."
      | otherwise = "RESULTADO: entrada válida, sin errores ni advertencias."
