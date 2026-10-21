-- | Modelo intermedio del pipeline (capa Domain).
--
-- Define los tipos de datos propios que representan el problema leido de
-- `problema.txt`: entidades, relaciones, reglas, diagnósticos y el
-- problema completo. Este módulo no depende de ningún otro de la aplicacion.
--
-- Todo lo que se genera despues (Prolog, consultas, reporte y HTML) sale de
-- estos tipos, nunca directamente del texto de entrada.
--
-- @ingroup capa_domain
module Domain.Types
  ( Fuente (..)
  , Localizado (..)
  , TipoEntidad (..)
  , Entidad (..)
  , Relacion (..)
  , Regla (..)
  , Severidad (..)
  , Diagnostico (..)
  , Problema (..)
  , ProblemaCrudo (..)
  , todosLosTipos
  , etiquetaTipo
  , tipoDeEntidad
  , nombreEntidad
  , mkEntidad
  , nombreRelacion
  , extremosRelacion
  , firmaRelacion
  , mkError
  , mkAdvertencia
  , esError
  , soloErrores
  , soloAdvertencias
  ) where

-- | Origen de un dato: número de línea (desde 1) y texto original ya recortado.
data Fuente = Fuente
  { fuenteLinea :: Int
  , fuenteTexto :: String
  } deriving (Show, Eq)

-- | Un valor junto con la línea de `problema.txt` de la que proviene.
-- Permite generar comentarios de trazabilidad en Prolog.
data Localizado a = Localizado
  { origen :: Fuente
  , valor :: a
  } deriving (Show, Eq)

-- | Categorias de entidad del dominio académico.
data TipoEntidad = TPersona | TCurso | TDocente
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Entidad declarada en la seccion `[ENTIDADES]`. Conserva el nombre original.
data Entidad
  = Persona String
  | Curso String
  | Docente String
  deriving (Show, Eq)

-- | Relación binaria declarada en la seccion `[RELACIONES]`.
data Relacion
  = Lleva String String
  | Imparte String String
  | Aprobo String String
  deriving (Show, Eq)

-- | Reglas lógicas que el usuario puede solicitar en `[REGLAS_SOLICITADAS]`.
data Regla
  = Companeros
  | CursoConDocente
  | DocenteDeEstudiante
  | CursoSinDocente
  | Aprobacion
  deriving (Show, Eq, Enum, Bounded)

-- | Gravedad de un diagnóstico.
data Severidad = Error | Advertencia
  deriving (Show, Eq, Ord)

-- | Mensaje de validación, con la línea afectada si existe.
data Diagnostico = Diagnostico
  { diagSeveridad :: Severidad
  , diagLinea :: Maybe Int
  , diagMensaje :: String
  } deriving (Show, Eq)

-- | Problema ya validado: solo contiene elementos correctos.
--
-- El campo `advertencias` reune todos los diagnósticos (errores y
-- advertencias) detectados al leer y validar el archivo.
data Problema = Problema
  { entidades :: [Localizado Entidad]
  , relaciones :: [Localizado Relacion]
  , reglasSolicitadas :: [Localizado Regla]
  , advertencias :: [Diagnostico]
  } deriving (Show)

-- | Resultado del parseo sintáctico, antes de validar significados.
-- Los textos de tipo, relación y regla aun no se han resuelto a constructores.
data ProblemaCrudo = ProblemaCrudo
  { crudoEntidades :: [Localizado (String, String)]
  , crudoRelaciones :: [Localizado (String, String, String)]
  , crudoReglas :: [Localizado String]
  , crudoDiagnosticos :: [Diagnostico]
  } deriving (Show)

-- | Lista de todos los tipos de entidad en orden de declaracion.
todosLosTipos :: [TipoEntidad]
todosLosTipos = [minBound .. maxBound]

-- | Palabra clave en `problema.txt` y nombre del predicado Prolog de un tipo.
etiquetaTipo :: TipoEntidad -> String
etiquetaTipo TPersona = "persona"
etiquetaTipo TCurso = "curso"
etiquetaTipo TDocente = "docente"

-- | Tipo al que pertenece una entidad.
tipoDeEntidad :: Entidad -> TipoEntidad
tipoDeEntidad (Persona _) = TPersona
tipoDeEntidad (Curso _) = TCurso
tipoDeEntidad (Docente _) = TDocente

-- | Nombre original de la entidad.
nombreEntidad :: Entidad -> String
nombreEntidad (Persona n) = n
nombreEntidad (Curso n) = n
nombreEntidad (Docente n) = n

-- | Construye una entidad a partir de su tipo y nombre.
mkEntidad :: TipoEntidad -> String -> Entidad
mkEntidad TPersona = Persona
mkEntidad TCurso = Curso
mkEntidad TDocente = Docente

-- | Palabra clave de la relación (y nombre del predicado Prolog).
nombreRelacion :: Relacion -> String
nombreRelacion (Lleva _ _) = "lleva"
nombreRelacion (Imparte _ _) = "imparte"
nombreRelacion (Aprobo _ _) = "aprobo"

-- | Los dos nombres que une la relación, en orden.
extremosRelacion :: Relacion -> (String, String)
extremosRelacion (Lleva a b) = (a, b)
extremosRelacion (Imparte a b) = (a, b)
extremosRelacion (Aprobo a b) = (a, b)

-- | Tipos de entidad que cada extremo de la relación debe tener.
firmaRelacion :: Relacion -> (TipoEntidad, TipoEntidad)
firmaRelacion (Lleva _ _) = (TPersona, TCurso)
firmaRelacion (Imparte _ _) = (TDocente, TCurso)
firmaRelacion (Aprobo _ _) = (TPersona, TCurso)

-- | Crea un diagnóstico de severidad Error.
mkError :: Maybe Int -> String -> Diagnostico
mkError = Diagnostico Error

-- | Crea un diagnóstico de severidad Advertencia.
mkAdvertencia :: Maybe Int -> String -> Diagnostico
mkAdvertencia = Diagnostico Advertencia

-- | Indica si el diagnóstico es un error.
esError :: Diagnostico -> Bool
esError d = diagSeveridad d == Error

-- | Diagnósticos de severidad Error del problema.
soloErrores :: Problema -> [Diagnostico]
soloErrores = filter esError . advertencias

-- | Diagnósticos de severidad Advertencia del problema.
soloAdvertencias :: Problema -> [Diagnostico]
soloAdvertencias = filter (not . esError) . advertencias
