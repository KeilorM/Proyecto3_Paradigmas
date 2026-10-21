-- | Validación semántica del problema (capa Domain).
--
-- Recibe el resultado del parseo sintáctico y construye el `Problema`
-- final. Todas las funciones son puras. Se detectan:
--
-- - entidades duplicadas (advertencia, se conserva la primera);
-- - tipos de entidad o de relación desconocidos (error);
-- - relaciones con entidades no declaradas o de tipo incorrecto (error);
-- - relaciones duplicadas (advertencia);
-- - reglas desconocidas o de otro dominio (error);
-- - reglas cuyas relaciones necesarias no tienen hechos (advertencia).
--
-- Los elementos inválidos se excluyen del modelo para que el Prolog
-- generado siempre sea coherente.
--
-- @ingroup capa_domain
module Domain.Validacion
  ( construirProblema
  , validarEntidades
  , validarRelaciones
  , validarReglas
  ) where

import Data.List (find, intercalate, sortOn)
import Domain.Atomo
import Domain.Dominio
import Domain.Types

-- | Construye el problema validado a partir del resultado del parseo.
construirProblema :: ProblemaCrudo -> Problema
construirProblema crudo =
  Problema
    { entidades = ents
    , relaciones = rels
    , reglasSolicitadas = reglas
    , advertencias = sortOn diagLinea (crudoDiagnosticos crudo ++ dEnt ++ dRel ++ dReg)
    }
  where
    (ents, dEnt) = validarEntidades (crudoEntidades crudo)
    (rels, dRel) = validarRelaciones ents (crudoRelaciones crudo)
    (reglas, dReg) = validarReglas rels (crudoReglas crudo)

-- | Resuelve el tipo de cada entidad y elimina duplicados.
validarEntidades
  :: [Localizado (String, String)]
  -> ([Localizado Entidad], [Diagnostico])
validarEntidades = go [] []
  where
    go acc ds [] = (reverse acc, reverse ds)
    go acc ds (Localizado f (kw, nombre) : resto) =
      case lookup kw tiposEntidadConocidos of
        Nothing ->
          go acc (mkError (Just (fuenteLinea f)) (msgTipo kw) : ds) resto
        Just t ->
          case find (mismaEntidad t nombre) acc of
            Just previa ->
              go acc (mkAdvertencia (Just (fuenteLinea f)) (msgDup kw nombre previa) : ds) resto
            Nothing ->
              go (Localizado f (mkEntidad t nombre) : acc) ds resto
    mismaEntidad t nombre (Localizado _ e) =
      tipoDeEntidad e == t && aAtomo (nombreEntidad e) == aAtomo nombre
    msgTipo kw =
      "Tipo de entidad desconocido \"" ++ kw ++ "\" (válidos: "
        ++ intercalate ", " (map fst tiposEntidadConocidos) ++ ")"
    msgDup kw nombre previa =
      "Entidad duplicada: " ++ kw ++ " \"" ++ nombre ++ "\" ya fue declarada en la línea "
        ++ show (fuenteLinea (origen previa)) ++ "; se ignora la repetición"

-- | Verifica que cada relación use entidades declaradas y del tipo correcto.
validarRelaciones
  :: [Localizado Entidad]
  -> [Localizado (String, String, String)]
  -> ([Localizado Relacion], [Diagnostico])
validarRelaciones ents = go [] []
  where
    go acc ds [] = (reverse acc, concat (reverse ds))
    go acc ds (Localizado f (kw, a, b) : resto) =
      case lookup kw relacionesConocidas of
        Nothing -> go acc ([mkError ln (msgRel kw)] : ds) resto
        Just mk ->
          let rel = mk a b
              (ta, tb) = firmaRelacion rel
              problemas = chequear kw ln ta a ++ chequear kw ln tb b
           in if not (null problemas)
                then go acc (problemas : ds) resto
                else
                  if any (misma rel) acc
                    then go acc ([mkAdvertencia ln (msgDup kw a b)] : ds) resto
                    else go (Localizado f rel : acc) ds resto
      where
        ln = Just (fuenteLinea f)
    misma rel (Localizado _ r) = clave r == clave rel
    clave r =
      let (x, y) = extremosRelacion r
       in (nombreRelacion r, aAtomo x, aAtomo y)
    chequear kw ln t n
      | any (\(Localizado _ e) -> tipoDeEntidad e == t && aAtomo (nombreEntidad e) == aAtomo n) ents = []
      | Just (Localizado _ otra) <- find (\(Localizado _ e) -> aAtomo (nombreEntidad e) == aAtomo n) ents =
          [ mkError ln
              ( "\"" ++ n ++ "\" está declarado como " ++ etiquetaTipo (tipoDeEntidad otra)
                  ++ ", pero la relación \"" ++ kw ++ "\" espera una entidad de tipo " ++ etiquetaTipo t
              )
          ]
      | otherwise =
          [mkError ln ("La relación \"" ++ kw ++ "\" usa la entidad no declarada \"" ++ n ++ "\"")]
    msgRel kw =
      "Tipo de relación desconocido \"" ++ kw ++ "\" (válidas: "
        ++ intercalate ", " (map fst relacionesConocidas) ++ ")"
    msgDup kw a b =
      "Relación duplicada: " ++ kw ++ " " ++ a ++ ", " ++ b ++ "; se ignora la repetición"

-- | Resuelve los nombres de reglas solicitadas contra el dominio.
--
-- El segundo argumento son las relaciones ya validadas, usadas para avisar
-- si una regla no tendra hechos con los que trabajar.
validarReglas
  :: [Localizado Relacion]
  -> [Localizado String]
  -> ([Localizado Regla], [Diagnostico])
validarReglas rels = go [] []
  where
    go acc ds [] =
      let reglas = reverse acc
       in (reglas, reverse ds ++ concatMap dependencias reglas)
    go acc ds (Localizado f nombre : resto) =
      let ln = Just (fuenteLinea f)
       in case lookup nombre reglasConocidas of
            Just r
              | any ((== r) . valor) acc ->
                  go acc (mkAdvertencia ln ("Regla repetida \"" ++ nombre ++ "\"; se ignora la repetición") : ds) resto
              | otherwise -> go (Localizado f r : acc) ds resto
            Nothing ->
              case lookup nombre reglasDeOtrosDominios of
                Just dom ->
                  go acc (mkError ln ("La regla \"" ++ nombre ++ "\" corresponde al dominio " ++ dom
                                      ++ ", no al dominio " ++ nombreDominio ++ "; se ignora") : ds) resto
                Nothing ->
                  go acc (mkError ln ("Regla desconocida \"" ++ nombre ++ "\" (válidas: "
                                      ++ intercalate ", " (map fst reglasConocidas) ++ ")") : ds) resto
    dependencias (Localizado f r) =
      [ mkAdvertencia (Just (fuenteLinea f))
          ( "La regla \"" ++ nombreRegla r ++ "\" necesita hechos \"" ++ dep
              ++ "\", pero no hay ninguna relación de ese tipo; no producirá resultados"
          )
      | dep <- dependenciasRegla r
      , not (any ((== dep) . nombreRelacion . valor) rels)
      ]
