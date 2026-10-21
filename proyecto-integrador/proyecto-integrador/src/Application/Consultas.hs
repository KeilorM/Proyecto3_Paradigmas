-- | Generación de consultas sugeridas (capa Application).
--
-- Las consultas dependen del dominio y del contenido: siempre se sugieren
-- listados de las entidades y relaciones presentes, y por cada regla
-- solicitada se agrega la consulta general y una consulta concreta con la
-- primera entidad declarada.
--
-- @ingroup capa_application
module Application.Consultas
  ( Consulta (..)
  , consultasSugeridas
  ) where

import Data.Maybe (listToMaybe)
import Domain.Atomo (aAtomo)
import Domain.Dominio
import Domain.Types

-- | Consulta sugerida: descripción legible y meta Prolog (sin `?-` ni punto final).
data Consulta = Consulta
  { consultaDescripcion :: String
  , consultaMeta :: String
  } deriving (Show, Eq)

-- | Calcula las consultas sugeridas para un problema validado.
consultasSugeridas :: Problema -> [Consulta]
consultasSugeridas p = basicas ++ concatMap (deRegla . valor) (reglasSolicitadas p)
  where
    nombresDe t = [nombreEntidad e | Localizado _ e <- entidades p, tipoDeEntidad e == t]
    primeraPersona = listToMaybe (nombresDe TPersona)
    primerCurso = listToMaybe (nombresDe TCurso)
    basicas =
      [ Consulta ("Listar " ++ etiquetaTipo t ++ "s") (etiquetaTipo t ++ "(X)")
      | t <- todosLosTipos
      , not (null (nombresDe t))
      ]
        ++ [ Consulta ("Listar relaciones «" ++ n ++ "»") (n ++ "(X, Y)")
           | (n, _) <- relacionesConocidas
           , any ((== n) . nombreRelacion . valor) (relaciones p)
           ]
    conPersona desc meta = [Consulta (desc ++ n) (meta (aAtomo n)) | Just n <- [primeraPersona]]
    conCurso desc meta = [Consulta (desc ++ n) (meta (aAtomo n)) | Just n <- [primerCurso]]
    deRegla Companeros =
      Consulta "Todos los pares de compañeros" "companero(X, Y)"
        : conPersona "Compañeros de " (\a -> "companero(" ++ a ++ ", X)")
    deRegla CursoConDocente =
      Consulta "Cursos con su docente" "curso_con_docente(Curso, Docente)"
        : conCurso "Docente del curso " (\c -> "curso_con_docente(" ++ c ++ ", Docente)")
    deRegla DocenteDeEstudiante =
      Consulta "Docente de cada persona" "docente_de(Persona, Docente)"
        : conPersona "Docentes de " (\a -> "docente_de(" ++ a ++ ", Docente)")
    deRegla CursoSinDocente =
      [Consulta "Cursos sin docente asignado" "curso_sin_docente(Curso)"]
    deRegla Aprobacion =
      [ Consulta "Cursos aprobados" "aprobado(Persona, Curso)"
      , Consulta "Cursos pendientes" "pendiente(Persona, Curso)"
      ]
