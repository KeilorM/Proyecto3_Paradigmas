-- | Catálogo del dominio académico.
--
-- Reune lo que "sabe" el sistema sobre el dominio seleccionado: secciones
-- obligatorias, tipos de entidad, relaciones permitidas y las reglas lógicas
-- con su definicion Prolog. Para cambiar de dominio basta con adaptar este
-- módulo y `Domain.Types`.
--
-- @ingroup capa_domain
module Domain.Dominio
  ( nombreDominio
  , seccionesObligatorias
  , tiposEntidadConocidos
  , relacionesConocidas
  , reglasConocidas
  , reglasDeOtrosDominios
  , predicadosBase
  , nombreRegla
  , descripcionRegla
  , predicadosRegla
  , dependenciasRegla
  , clausulasRegla
  ) where

import Domain.Types

-- | Nombre del dominio de trabajo.
nombreDominio :: String
nombreDominio = "académico"

-- | Secciones que debe tener `problema.txt`.
seccionesObligatorias :: [String]
seccionesObligatorias = ["ENTIDADES", "RELACIONES", "REGLAS_SOLICITADAS"]

-- | Palabras clave aceptadas para declarar entidades.
tiposEntidadConocidos :: [(String, TipoEntidad)]
tiposEntidadConocidos = [(etiquetaTipo t, t) | t <- todosLosTipos]

-- | Palabras clave aceptadas para relaciones, con su constructor.
relacionesConocidas :: [(String, String -> String -> Relacion)]
relacionesConocidas =
  [ ("lleva", Lleva)
  , ("imparte", Imparte)
  , ("aprobo", Aprobo)
  ]

-- | Reglas del dominio indexadas por el nombre que se escribe en el archivo.
reglasConocidas :: [(String, Regla)]
reglasConocidas = [(nombreRegla r, r) | r <- [minBound .. maxBound]]

-- | Nombres de reglas que existen en otros dominios permitidos; sirven para
-- avisar que "no corresponden al dominio seleccionado".
reglasDeOtrosDominios :: [(String, String)]
reglasDeOtrosDominios =
  [ ("hermanos", "familiar")
  , ("abuelos", "familiar")
  , ("primos", "familiar")
  , ("padres", "familiar")
  , ("ruta_posible", "rutas")
  , ("camino", "rutas")
  , ("conexiones", "rutas")
  , ("prioridad", "emergencias")
  , ("recursos_disponibles", "emergencias")
  , ("alerta_stock", "inventario")
  , ("existencia", "inventario")
  ]

-- | Predicados que siempre se declaran como dinámicos en Prolog, con su
-- aridad, para que las reglas no fallen con error si no hay hechos.
predicadosBase :: [(String, Int)]
predicadosBase =
  [(etiquetaTipo t, 1) | t <- todosLosTipos]
    ++ [(n, 2) | (n, _) <- relacionesConocidas]

-- | Nombre con el que se solicita la regla en `problema.txt`.
nombreRegla :: Regla -> String
nombreRegla Companeros = "companeros"
nombreRegla CursoConDocente = "curso_con_docente"
nombreRegla DocenteDeEstudiante = "docente_de_estudiante"
nombreRegla CursoSinDocente = "curso_sin_docente"
nombreRegla Aprobacion = "aprobacion"

-- | Descripción en lenguaje natural de la regla.
descripcionRegla :: Regla -> String
descripcionRegla Companeros = "Dos personas distintas son compañeros si llevan el mismo curso."
descripcionRegla CursoConDocente = "Relaciona cada curso con el docente que lo imparte."
descripcionRegla DocenteDeEstudiante = "Un docente lo es de una persona si imparte un curso que ella lleva."
descripcionRegla CursoSinDocente = "Cursos declarados que ningún docente imparte."
descripcionRegla Aprobacion = "Una persona aprobó un curso que lleva; si no, queda pendiente."

-- | Predicados (nombre y aridad) que define la regla en Prolog.
predicadosRegla :: Regla -> [String]
predicadosRegla Companeros = ["companero/2"]
predicadosRegla CursoConDocente = ["curso_con_docente/2"]
predicadosRegla DocenteDeEstudiante = ["docente_de/2"]
predicadosRegla CursoSinDocente = ["curso_sin_docente/1"]
predicadosRegla Aprobacion = ["aprobado/2", "pendiente/2"]

-- | Relaciones cuyos hechos necesita la regla para producir resultados.
dependenciasRegla :: Regla -> [String]
dependenciasRegla Companeros = ["lleva"]
dependenciasRegla CursoConDocente = ["imparte"]
dependenciasRegla DocenteDeEstudiante = ["lleva", "imparte"]
dependenciasRegla CursoSinDocente = []
dependenciasRegla Aprobacion = ["lleva", "aprobo"]

-- | Clausulas Prolog (una cadena por línea) que implementan la regla.
clausulasRegla :: Regla -> [String]
clausulasRegla Companeros =
  [ "companero(X, Y) :-"
  , "    lleva(X, Curso),"
  , "    lleva(Y, Curso),"
  , "    X \\= Y."
  ]
clausulasRegla CursoConDocente =
  [ "curso_con_docente(Curso, Docente) :-"
  , "    curso(Curso),"
  , "    imparte(Docente, Curso)."
  ]
clausulasRegla DocenteDeEstudiante =
  [ "docente_de(Persona, Docente) :-"
  , "    lleva(Persona, Curso),"
  , "    imparte(Docente, Curso)."
  ]
clausulasRegla CursoSinDocente =
  [ "curso_sin_docente(Curso) :-"
  , "    curso(Curso),"
  , "    \\+ imparte(_, Curso)."
  ]
clausulasRegla Aprobacion =
  [ "aprobado(Persona, Curso) :-"
  , "    lleva(Persona, Curso),"
  , "    aprobo(Persona, Curso)."
  , ""
  , "pendiente(Persona, Curso) :-"
  , "    lleva(Persona, Curso),"
  , "    \\+ aprobo(Persona, Curso)."
  ]
