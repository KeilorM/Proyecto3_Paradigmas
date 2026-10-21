-- | Generación de los archivos Prolog (capa Infrastructure).
--
-- Convierte el modelo intermedio en texto Prolog valido para SWI-Prolog:
--
-- - `conocimiento.pl`: hechos con comentarios de trazabilidad (línea de
--   `problema.txt` de la que provienen) y las reglas solicitadas;
-- - `consultas.pl`: consultas sugeridas, ejecutables con
--   `ejecutar_consultas/0`.
--
-- @ingroup capa_infrastructure
module Infrastructure.PrologGen
  ( generarConocimiento
  , generarConsultasPl
  ) where

import Application.Consultas
import Data.List (intercalate)
import Domain.Atomo (escaparAtomo)
import Domain.Dominio
import Domain.Hechos
import Domain.Types

-- | Genera el contenido completo de `conocimiento.pl`.
--
-- Cada hecho va precedido de un comentario `% linea N: texto original`.
-- Los predicados base se declaran `dynamic` para que las consultas no
-- fallen con error cuando una relación no tiene hechos.
generarConocimiento :: Problema -> String
generarConocimiento p =
  unlines (cabecera ++ declaraciones ++ seccionHechos ++ seccionReglas)
  where
    cabecera =
      [ ":- encoding(utf8)."
      , ""
      , "% =============================================================="
      , "% conocimiento.pl"
      , "% Base de conocimiento generada automáticamente por el pipeline"
      , "% Haskell -> Prolog -> UI. NO editar a mano: se regenera desde"
      , "% problema.txt. Dominio: " ++ nombreDominio
      , "% =============================================================="
      , ""
      ]
    declaraciones =
      [ ":- dynamic " ++ intercalate ", " [n ++ "/" ++ show a | (n, a) <- predicadosBase] ++ "."
      , ""
      , "% ---------------------------- HECHOS ----------------------------"
      , ""
      ]
    seccionHechos
      | null hechos = ["% (el archivo de entrada no produjo hechos válidos)", ""]
      | otherwise = concatMap renderHecho hechos
    hechos = hechosDe p
    renderHecho h =
      [ "% linea " ++ show (fuenteLinea (hechoOrigen h)) ++ ": " ++ fuenteTexto (hechoOrigen h)
      , mostrarHecho h
      , ""
      ]
    seccionReglas =
      "% ---------------------------- REGLAS ----------------------------"
        : ""
        : (if null reglas then ["% (no se solicitaron reglas válidas)", ""] else concatMap renderRegla reglas)
    reglas = reglasSolicitadas p
    renderRegla (Localizado f r) =
      [ "% linea " ++ show (fuenteLinea f) ++ ": " ++ fuenteTexto f
      , "% " ++ descripcionRegla r
      ]
        ++ clausulasRegla r
        ++ [""]

-- | Genera el contenido completo de `consultas.pl`.
--
-- Cada consulta se guarda como un hecho `consulta(Descripcion, Meta)`;
-- `ejecutar_consultas/0` las ejecuta todas e imprime las soluciones.
generarConsultasPl :: [Consulta] -> String
generarConsultasPl cs =
  unlines $
    [ ":- encoding(utf8)."
    , ""
    , "% =============================================================="
    , "% consultas.pl"
    , "% Consultas sugeridas, generadas automáticamente."
    , "%"
    , "% Uso:"
    , "%   swipl consultas.pl"
    , "%   ?- ejecutar_consultas.          % ejecuta todas"
    , "%   ?- consulta(Descripcion, Meta). % lista las sugeridas"
    , "% =============================================================="
    , ""
    , ":- style_check(-singleton)."
    , ":- consult('conocimiento.pl')."
    , ""
    , "% consulta(+Descripcion, +Meta)"
    ]
      ++ (if null cs then ["consulta('(sin consultas)', true)."] else map renderConsulta cs)
      ++ [ ""
         , "% Ejecuta cada consulta sugerida e imprime todas sus soluciones."
         , "ejecutar_consultas :-"
         , "    forall("
         , "        consulta(Descripcion, Meta),"
         , "        (   format(\"~n~w~n\", [Descripcion]),"
         , "            findall(Meta, Meta, Soluciones),"
         , "            (   Soluciones == []"
         , "            ->  format(\"  (sin resultados)~n\")"
         , "            ;   forall(member(S, Soluciones), format(\"  ~q~n\", [S]))"
         , "            )"
         , "        )"
         , "    )."
         ]
  where
    renderConsulta (Consulta d m) =
      "consulta('" ++ escaparAtomo d ++ "', " ++ m ++ ")."
