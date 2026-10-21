-- | Generación de la interfaz `index.html` (capa Presentation).
--
-- La página se construye por completo a partir del modelo intermedio
-- (`Problema`), de las consultas sugeridas y del texto Prolog generado. No
-- hay contenido escrito a mano: si cambia `problema.txt`, al ejecutar el
-- pipeline cambian entidades, relaciones, hechos, reglas, consultas,
-- advertencias y el grafo.
--
-- El resultado es un único archivo HTML autocontenido (sin CDN), que se
-- abre sin servidor.
--
-- @ingroup capa_presentation
module Presentation.HtmlGen
  ( generarHtml
  ) where

import Application.Consultas
import Data.List (elemIndex, sortOn)
import Data.Maybe (mapMaybe)
import Domain.Atomo (aAtomo)
import Domain.Dominio
import Domain.Hechos
import Domain.Types

-- | Genera el documento HTML completo.
generarHtml :: Problema -> [Consulta] -> String -> String
generarHtml p cs conocimiento =
  unlines
    [ "<!DOCTYPE html>"
    , "<html lang='es'>"
    , "<head>"
    , "<meta charset='utf-8'>"
    , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
    , "<title>Pipeline Haskell - Prolog - UI</title>"
    , "<style>"
    , estilos
    , "</style>"
    , "</head>"
    , "<body>"
    , "<header>"
    , "<h1>Pipeline Haskell → Prolog → UI</h1>"
    , "<p>Dominio " ++ esc nombreDominio ++ " · página generada automáticamente desde <code>problema.txt</code></p>"
    , "</header>"
    , navegacion
    , "<main>"
    , seccionResumen p cs
    , seccionEntidades p
    , seccionRelaciones p
    , seccionHechos p
    , seccionReglas p
    , seccionConsultas cs
    , seccionDiagnosticos p
    , seccionConocimiento conocimiento
    , "</main>"
    , "<script>"
    , guion
    , "</script>"
    , "</body>"
    , "</html>"
    ]

-- | Escapa los caracteres especiales de HTML.
esc :: String -> String
esc = concatMap f
  where
    f '<' = "&lt;"
    f '>' = "&gt;"
    f '&' = "&amp;"
    f '"' = "&quot;"
    f '\'' = "&#39;"
    f c = [c]

-- | Barra de navegacion con filtro de busqueda.
navegacion :: String
navegacion =
  concat
    [ "<nav><div>"
    , concat
        [ "<a href='#" ++ i ++ "'>" ++ t ++ "</a>"
        | (i, t) <-
            [ ("resumen", "Resumen")
            , ("entidades", "Entidades")
            , ("relaciones", "Relaciones")
            , ("hechos", "Hechos")
            , ("reglas", "Reglas")
            , ("consultas", "Consultas")
            , ("diagnosticos", "Advertencias y errores")
            , ("conocimiento", "conocimiento.pl")
            ]
        ]
    , "<input id='filtro' type='search' placeholder='Filtrar tablas y listas…' aria-label='Filtrar'>"
    , "</div></nav>"
    ]

-- | Tarjetas con los conteos del pipeline.
seccionResumen :: Problema -> [Consulta] -> String
seccionResumen p cs =
  seccion "resumen" "Resumen" $
    "<div class='cards'>"
      ++ concat
        [ "<div class='card'><b>" ++ show n ++ "</b><span>" ++ t ++ "</span></div>"
        | (n, t) <-
            [ (length (entidades p), "entidades")
            , (length (relaciones p), "relaciones")
            , (length (hechosDe p), "hechos Prolog")
            , (length (reglasSolicitadas p), "reglas")
            , (length cs, "consultas")
            , (length (soloErrores p), "errores")
            , (length (soloAdvertencias p), "advertencias")
            ]
        ]
      ++ "</div>"

-- | Tabla de entidades detectadas.
seccionEntidades :: Problema -> String
seccionEntidades p =
  seccion "entidades" "Entidades detectadas" $
    tabla
      ["Tipo", "Nombre", "Átomo Prolog", "Línea"]
      [ [ "<span class='tag'>" ++ esc (etiquetaTipo (tipoDeEntidad e)) ++ "</span>"
        , esc (nombreEntidad e)
        , "<code>" ++ esc (aAtomo (nombreEntidad e)) ++ "</code>"
        , show (fuenteLinea f)
        ]
      | Localizado f e <- entidades p
      ]

-- | Tabla de relaciones y grafo.
seccionRelaciones :: Problema -> String
seccionRelaciones p =
  seccion "relaciones" "Relaciones generadas" $
    tabla
      ["Relación", "Origen", "Destino", "Línea"]
      [ [ "<span class='tag'>" ++ esc (nombreRelacion r) ++ "</span>"
        , esc a
        , esc b
        , show (fuenteLinea f)
        ]
      | Localizado f r <- relaciones p
      , let (a, b) = extremosRelacion r
      ]
      ++ grafoSvg p

-- | Tabla de hechos Prolog con su línea de origen.
seccionHechos :: Problema -> String
seccionHechos p =
  seccion "hechos" "Hechos Prolog producidos" $
    tabla
      ["Hecho", "Origen en problema.txt"]
      [ [ "<code>" ++ esc (mostrarHecho h) ++ "</code>"
        , "línea " ++ show (fuenteLinea o) ++ ": " ++ esc (fuenteTexto o)
        ]
      | h <- hechosDe p
      , let o = hechoOrigen h
      ]

-- | Reglas solicitadas con su descripción y clausulas.
seccionReglas :: Problema -> String
seccionReglas p =
  seccion "reglas" "Reglas generadas" $
    if null (reglasSolicitadas p)
      then "<p class='muted'>No se generaron reglas.</p>"
      else
        concat
          [ "<h3>" ++ esc (nombreRegla r) ++ " <span class='tag'>línea " ++ show (fuenteLinea f) ++ "</span></h3>"
              ++ "<p class='muted'>" ++ esc (descripcionRegla r)
              ++ " Define: " ++ esc (unwords (predicadosRegla r)) ++ "</p>"
              ++ "<pre>" ++ esc (unlines (clausulasRegla r)) ++ "</pre>"
          | Localizado f r <- reglasSolicitadas p
          ]

-- | Consultas sugeridas con boton para copiarlas.
seccionConsultas :: [Consulta] -> String
seccionConsultas cs =
  seccion "consultas" "Consultas sugeridas" $
    if null cs
      then "<p class='muted'>Sin consultas sugeridas.</p>"
      else
        "<ul class='lista'>"
          ++ concat
            [ "<li class='item'><button class='copy' data-q='" ++ esc m ++ "'>Copiar</button>"
                ++ "<b>" ++ esc d ++ "</b><br><code>?- " ++ esc m ++ ".</code></li>"
            | Consulta d m <- cs
            ]
          ++ "</ul>"
          ++ "<p class='muted'>Para ejecutarlas: <code>swipl consultas.pl</code> y luego <code>?- ejecutar_consultas.</code></p>"

-- | Lista de errores y advertencias.
seccionDiagnosticos :: Problema -> String
seccionDiagnosticos p =
  seccion "diagnosticos" "Advertencias y errores" $
    if null ds
      then "<p class='ok'>✓ Sin errores ni advertencias.</p>"
      else
        "<ul class='lista'>"
          ++ concat
            [ "<li class='item " ++ (if esError d then "error" else "aviso") ++ "'><b>"
                ++ (if esError d then "ERROR" else "ADVERTENCIA") ++ "</b> "
                ++ maybe "<span class='muted'>(global)</span>" (\n -> "<span class='tag'>línea " ++ show n ++ "</span>") (diagLinea d)
                ++ "<br>" ++ esc (diagMensaje d) ++ "</li>"
            | d <- ds
            ]
          ++ "</ul>"
  where
    ds = sortOn (\d -> (not (esError d), diagLinea d)) (advertencias p)

-- | Contenido completo de `conocimiento.pl`.
seccionConocimiento :: String -> String
seccionConocimiento conocimiento =
  seccion "conocimiento" "Contenido de conocimiento.pl" $
    "<pre>" ++ esc conocimiento ++ "</pre>"

-- | Envuelve contenido en una seccion con titulo.
seccion :: String -> String -> String -> String
seccion i titulo cuerpo =
  "<section id='" ++ i ++ "'><h2>" ++ esc titulo ++ "</h2>" ++ cuerpo ++ "</section>"

-- | Tabla HTML; las celdas ya deben venir escapadas.
tabla :: [String] -> [[String]] -> String
tabla _ [] = "<p class='muted'>Sin elementos.</p>"
tabla cabeceras filas =
  concat
    [ "<div class='tw'><table><thead><tr>"
    , concatMap (\h -> "<th>" ++ h ++ "</th>") cabeceras
    , "</tr></thead><tbody>\n"
    , concatMap fila filas
    , "</tbody></table></div>"
    ]
  where
    fila celdas = "<tr>" ++ concatMap (\c -> "<td>" ++ c ++ "</td>") celdas ++ "</tr>\n"

-- | Grafo SVG: personas, cursos y docentes en tres columnas, con una
-- arista por relación.
grafoSvg :: Problema -> String
grafoSvg p
  | null (entidades p) = ""
  | otherwise =
      unlines $
        [ "<svg class='grafo' viewBox='0 0 " ++ show ancho ++ " " ++ show alto ++ "' role='img' aria-label='Grafo de relaciones'>" ]
          ++ [ "<text x='" ++ show (x + anchoNodo `div` 2) ++ "' y='18' text-anchor='middle' class='col'>" ++ esc (etiquetaTipo t) ++ "s</text>"
             | (t, x) <- columnas
             ]
          ++ mapMaybe arista (relaciones p)
          ++ concatMap nodosDe todosLosTipos
          ++ ["</svg>"]
          ++ [ "<p class='muted'>"
                 ++ concat
                   [ "<span class='tag' style='border-color:var(--c-" ++ n ++ ");color:var(--c-" ++ n ++ ")'>" ++ n ++ "</span> "
                   | n <- ["lleva", "imparte", "aprobo"]
                   ]
                 ++ "</p>"
             ]
  where
    anchoNodo = 150 :: Int
    altoNodo = 30 :: Int
    sep = 44 :: Int
    top = 30 :: Int
    ancho = 730 :: Int
    columnas = [(TPersona, 20), (TCurso, 290), (TDocente, 560)] :: [(TipoEntidad, Int)]
    nombresDe t = [nombreEntidad e | Localizado _ e <- entidades p, tipoDeEntidad e == t]
    alto = top + maximum (1 : map (length . nombresDe) todosLosTipos) * sep
    posicion t n = do
      i <- elemIndex (aAtomo n) (map aAtomo (nombresDe t))
      x <- lookup t columnas
      return (x, top + i * sep)
    nodosDe t =
      [ "<g class='nodo'><title>" ++ esc n ++ "</title><rect x='" ++ show x ++ "' y='" ++ show y
          ++ "' width='" ++ show anchoNodo ++ "' height='" ++ show altoNodo ++ "' rx='6'/>"
          ++ "<text x='" ++ show (x + 10) ++ "' y='" ++ show (y + 20) ++ "'>" ++ esc (truncar 20 n) ++ "</text></g>"
      | n <- nombresDe t
      , Just (x, y) <- [posicion t n]
      ]
    arista (Localizado _ r) = do
      let (ta, tb) = firmaRelacion r
          (a, b) = extremosRelacion r
          desvio = if nombreRelacion r == "aprobo" then 5 else 0
      (x1, y1) <- posicion ta a
      (x2, y2) <- posicion tb b
      let (px, qx) = if x1 < x2 then (x1 + anchoNodo, x2) else (x1, x2 + anchoNodo)
          py = y1 + altoNodo `div` 2 + desvio
          qy = y2 + altoNodo `div` 2 + desvio
      return
        ( "<line class='e e-" ++ nombreRelacion r ++ "' x1='" ++ show px ++ "' y1='" ++ show py
            ++ "' x2='" ++ show qx ++ "' y2='" ++ show qy ++ "'><title>" ++ esc (nombreRelacion r ++ ": " ++ a ++ ", " ++ b)
            ++ "</title></line>"
        )
    truncar n s = if length s > n then take (n - 1) s ++ "…" else s

-- | Hoja de estilos (con modo oscuro automatico).
estilos :: String
estilos =
  unlines
    [ ":root{--bg:#f6f7f9;--fg:#1c2230;--muted:#5b6475;--card:#fff;--line:#e2e5ea;--accent:#2f5fd0;--c-lleva:#2f5fd0;--c-imparte:#d9822b;--c-aprobo:#2e9d5b;--err:#c0392b;--warn:#b7791f;--code:#f0f2f6}"
    , "@media (prefers-color-scheme:dark){:root{--bg:#12151c;--fg:#e6e9ef;--muted:#9aa3b5;--card:#1a1f2a;--line:#2b3242;--accent:#7aa2ff;--c-lleva:#7aa2ff;--c-imparte:#f0a45d;--c-aprobo:#58c98a;--err:#ff7b6b;--warn:#e3b341;--code:#10131a}}"
    , "*{box-sizing:border-box}"
    , "body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.55 system-ui,-apple-system,'Segoe UI',Roboto,sans-serif}"
    , "header{padding:28px 24px 12px;max-width:1100px;margin:0 auto}"
    , "h1{margin:0 0 4px;font-size:24px}"
    , "header p{margin:0;color:var(--muted)}"
    , "nav{position:sticky;top:0;background:var(--bg);border-bottom:1px solid var(--line);z-index:5}"
    , "nav div{max-width:1100px;margin:0 auto;padding:8px 24px;display:flex;gap:6px 16px;flex-wrap:wrap;align-items:center}"
    , "nav a{color:var(--accent);text-decoration:none;font-size:14px}"
    , "nav input{margin-left:auto;padding:6px 10px;border:1px solid var(--line);border-radius:6px;background:var(--card);color:var(--fg);min-width:200px}"
    , "main{max-width:1100px;margin:0 auto;padding:8px 24px 48px}"
    , "section{margin-top:30px;scroll-margin-top:60px}"
    , "h2{font-size:18px;margin:0 0 10px}"
    , "h3{font-size:15px;margin:16px 0 2px}"
    , ".cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(130px,1fr));gap:10px}"
    , ".card{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:12px 14px}"
    , ".card b{display:block;font-size:24px}"
    , ".card span{color:var(--muted);font-size:13px}"
    , ".tw{overflow-x:auto}"
    , "table{width:100%;border-collapse:collapse;background:var(--card);border:1px solid var(--line)}"
    , "nav input{min-width:0;flex:1 1 160px}"
    , "th,td{text-align:left;padding:7px 12px;border-bottom:1px solid var(--line);font-size:14px}"
    , "th{background:var(--code);color:var(--muted);font-weight:600}"
    , "tr:last-child td{border-bottom:0}"
    , "code,pre{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;font-size:13px}"
    , "pre{background:var(--code);border:1px solid var(--line);border-radius:10px;padding:12px 14px;overflow:auto;margin:6px 0}"
    , ".tag{display:inline-block;padding:1px 8px;border-radius:99px;font-size:12px;border:1px solid var(--line);color:var(--muted)}"
    , "ul.lista{list-style:none;margin:0;padding:0;display:grid;gap:8px}"
    , "ul.lista li{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:10px 14px}"
    , "li.error{border-left:4px solid var(--err)}"
    , "li.aviso{border-left:4px solid var(--warn)}"
    , ".ok{color:var(--c-aprobo)}"
    , ".muted{color:var(--muted)}"
    , ".oculto{display:none}"
    , "button.copy{float:right;border:1px solid var(--line);background:var(--card);color:var(--fg);border-radius:6px;padding:2px 10px;cursor:pointer}"
    , "svg.grafo{width:100%;height:auto;background:var(--card);border:1px solid var(--line);border-radius:10px;margin-top:14px}"
    , ".nodo rect{fill:var(--code);stroke:var(--line)}"
    , ".nodo text{fill:var(--fg);font-size:13px}"
    , ".col{fill:var(--muted);font-size:12px;text-transform:uppercase;letter-spacing:.06em}"
    , ".e{fill:none;stroke-width:1.8;opacity:.85}"
    , ".e-lleva{stroke:var(--c-lleva)}"
    , ".e-imparte{stroke:var(--c-imparte)}"
    , ".e-aprobo{stroke:var(--c-aprobo);stroke-dasharray:5 3}"
    ]

-- | Script de la página: filtro de busqueda y copiado de consultas.
guion :: String
guion =
  unlines
    [ "const q=document.getElementById('filtro');"
    , "q.addEventListener('input',()=>{const t=q.value.toLowerCase();"
    , " document.querySelectorAll('tbody tr, li.item').forEach(e=>{"
    , "  e.classList.toggle('oculto',t!==''&&!e.textContent.toLowerCase().includes(t));});});"
    , "document.querySelectorAll('button.copy').forEach(b=>b.addEventListener('click',()=>{"
    , " const txt='?- '+b.dataset.q+'.';"
    , " if(navigator.clipboard){navigator.clipboard.writeText(txt);}"
    , " b.textContent='Copiado';setTimeout(()=>{b.textContent='Copiar'},1200);}));"
    ]
