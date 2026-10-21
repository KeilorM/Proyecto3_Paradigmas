-- | Caso de uso principal: el pipeline completo (capa Application).
--
-- `ejecutarPipeline` es una función pura: recibe el texto de
-- `problema.txt` y devuelve todos los artefactos como cadenas.
--
-- La capa Application no conoce la sintaxis de Prolog ni de HTML: depende
-- de una interfaz (`Generadores`) que las capas externas implementan y que
-- se conecta en `Main` (inversión de dependencias).
--
-- @ingroup capa_application
module Application.Pipeline
  ( Generadores (..)
  , Resultado (..)
  , ejecutarPipeline
  ) where

import Application.Consultas
import Application.Parser (parsearProblema)
import Domain.Types
import Domain.Validacion (construirProblema)

-- | Puerto de salida: funciones que transforman el modelo en texto.
data Generadores = Generadores
  { genConocimiento :: Problema -> String
    -- ^ Produce el contenido de `conocimiento.pl`.
  , genConsultasPl :: [Consulta] -> String
    -- ^ Produce el contenido de `consultas.pl`.
  , genReporte :: Problema -> String
    -- ^ Produce el contenido de `reporte_validacion.txt`.
  , genHtml :: Problema -> [Consulta] -> String -> String
    -- ^ Produce `index.html` a partir del modelo, las consultas y el Prolog.
  }

-- | Todo lo que produce el pipeline.
data Resultado = Resultado
  { resProblema :: Problema
  , resConsultas :: [Consulta]
  , resConocimientoPl :: String
  , resConsultasPl :: String
  , resReporte :: String
  , resIndexHtml :: String
  }

-- | Ejecuta el pipeline: parseo, validación, modelo intermedio y generación.
ejecutarPipeline :: Generadores -> String -> Resultado
ejecutarPipeline g texto =
  Resultado
    { resProblema = problema
    , resConsultas = consultas
    , resConocimientoPl = conocimiento
    , resConsultasPl = genConsultasPl g consultas
    , resReporte = genReporte g problema
    , resIndexHtml = genHtml g problema consultas conocimiento
    }
  where
    problema = construirProblema (parsearProblema texto)
    consultas = consultasSugeridas problema
    conocimiento = genConocimiento g problema
