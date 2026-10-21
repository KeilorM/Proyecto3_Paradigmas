# Pipeline Funcional-Lógico: Haskell → Prolog → UI

Proyecto Integrador (30 % del curso). Lee un archivo `problema.txt` con la descripción
estructurada de un problema del **dominio académico**, lo procesa con **Haskell**,
genera una base de conocimiento para **SWI-Prolog** y construye automáticamente una
**interfaz web** para visualizarla. La documentación técnica se genera con **Doxygen** y la
arquitectura sigue los principios de **Clean Architecture**.

```
 problema.txt ──► Parser ──► Validación ──► Problema (modelo intermedio)
                                                  │
                     ┌────────────────────────────┼─────────────────────────┐
                     ▼                            ▼                         ▼
             conocimiento.pl              reporte_validacion.txt        index.html
             consultas.pl                                              (UI generada)
```

## Requisitos

| Herramienta | Versión probada | Para qué |
|---|---|---|
| GHC | 9.4.7 (sirve ≥ 9.0) | compilar el pipeline. Solo usa `base`, `directory` y `filepath` |
| SWI-Prolog | 9.0.4 | ejecutar `conocimiento.pl` y `consultas.pl` |
| Doxygen | 1.9.8 | generar la documentación |
| Graphviz (`dot`) | opcional | diagramas en Doxygen |
| Navegador web | cualquiera | abrir `index.html` (no necesita servidor ni internet) |

Instalación en Ubuntu/Debian: `sudo apt install ghc swi-prolog doxygen graphviz`.
En Windows: GHCup (GHC), instalador de SWI-Prolog y de Doxygen.

## Ejecución

### Con make (Linux/macOS, o Windows con make)

```
make run          # compila y procesa problema.txt, escribe en salida/
make test         # pruebas en Haskell y en Prolog
make consultas    # ejecuta las consultas sugeridas en SWI-Prolog
make doc          # genera docs/doxygen/html/index.html
```

### Sin make

```
mkdir build
ghc -isrc -iapp -outputdir build -o build/proyecto-integrador app/Main.hs
build/proyecto-integrador problema.txt salida
```

### Con cabal

```
cabal run proyecto-integrador -- problema.txt salida
cabal test
```

### Uso del ejecutable

```
proyecto-integrador [problema.txt] [directorio_de_salida]
```

Por defecto lee `problema.txt` y escribe en `salida/`. Genera cuatro archivos:

| Archivo | Contenido |
|---|---|
| `conocimiento.pl` | hechos con comentarios de trazabilidad y reglas solicitadas |
| `consultas.pl` | consultas sugeridas, ejecutables con `ejecutar_consultas/0` |
| `index.html` | interfaz generada (entidades, relaciones, hechos, reglas, consultas, advertencias, grafo y código Prolog) |
| `reporte_validacion.txt` | errores y advertencias con número de línea |

**Condición de defensa:** para usar otro problema solo se ejecuta
`build/proyecto-integrador otro_problema.txt salida`. Los cuatro archivos se regeneran
completos sin tocar ninguna página a mano. Se incluyen `ejemplos/problema2.txt` (otro problema válido),
`ejemplos/problema_con_errores.txt` y `ejemplos/problema_sin_secciones.txt` (validaciones).

## Formato de problema.txt

```
# Los comentarios empiezan con #
[ENTIDADES]
persona: Ana
curso: Paradigmas
docente: Maria

[RELACIONES]
lleva: Ana, Paradigmas
imparte: Maria, Paradigmas
aprobo: Ana, Paradigmas

[REGLAS_SOLICITADAS]
companeros
curso_con_docente
```

| Sección | Palabras válidas |
|---|---|
| `[ENTIDADES]` | `persona`, `curso`, `docente` |
| `[RELACIONES]` | `lleva` (persona, curso), `imparte` (docente, curso), `aprobo` (persona, curso) |
| `[REGLAS_SOLICITADAS]` | `companeros`, `curso_con_docente`, `docente_de_estudiante`, `curso_sin_docente`, `aprobacion` |

Los nombres se convierten a átomos Prolog: `María José` → `maria_jose`, `Bases de Datos` → `bases_de_datos`.

## Ejemplo de salida

Entrada `lleva: Ana, Paradigmas` en la línea 16 produce:

```prolog
% linea 16: lleva: Ana, Paradigmas
lleva(ana, paradigmas).
```

y la regla `companeros` produce:

```prolog
companero(X, Y) :-
    lleva(X, Curso),
    lleva(Y, Curso),
    X \= Y.
```

Consulta en SWI-Prolog:

```
swipl salida/consultas.pl
?- ejecutar_consultas.
?- companero(ana, X).
```

## Validaciones

Todas generan un diagnóstico con número de línea en `reporte_validacion.txt`, en la UI y en consola.
Los elementos inválidos se excluyen de `conocimiento.pl`, que siempre carga sin errores.

| Detección | Severidad |
|---|---|
| Entidad duplicada | advertencia |
| Relación duplicada | advertencia |
| Relación con entidad no declarada o de tipo incorrecto | error |
| Línea con formato inválido, o fuera de una sección | error |
| Sección obligatoria ausente | error |
| Sección repetida, desconocida o vacía | advertencia |
| Regla de otro dominio (`hermanos`, `ruta_posible`, …) o desconocida | error |
| Regla sin hechos que la alimenten | advertencia |

## Arquitectura (Clean Architecture)

Las dependencias apuntan hacia adentro. El caso de uso no conoce Prolog ni HTML: recibe los
generadores por una interfaz (`Generadores`) que `app/Main.hs` conecta.

```
src/
  Domain/                  reglas de negocio puras, sin E/S
    Types.hs               modelo intermedio: Entidad, Relacion, Regla, Problema, Diagnostico
    Dominio.hs             catálogo del dominio académico y definición de las reglas
    Validacion.hs          validación semántica
    Hechos.hs              hechos Prolog derivados del modelo (fuente única para .pl y UI)
    Atomo.hs               nombre libre → átomo Prolog
  Application/             casos de uso
    Parser.hs              secciones y líneas → ProblemaCrudo
    Consultas.hs           consultas sugeridas
    Pipeline.hs            ejecutarPipeline (función pura) e interfaz Generadores
  Infrastructure/          adaptadores técnicos
    PrologGen.hs           conocimiento.pl y consultas.pl
    Reporte.hs             reporte_validacion.txt
    Archivos.hs            lectura/escritura UTF-8
  Presentation/
    HtmlGen.hs             index.html (HTML, CSS, JS y SVG autocontenido)
app/Main.hs                raíz de composición
test/Spec.hs, pruebas.pl   pruebas en Haskell y Prolog
tools/DoxyFilter.hs        filtro para que Doxygen entienda Haskell
```

El modelo intermedio es el de la especificación, ampliado para la trazabilidad:
`Problema` guarda cada entidad, relación y regla junto con su línea de origen (`Localizado`),
y `advertencias` reúne todos los diagnósticos.

## Pruebas

| Capa | Comando | Qué verifica |
|---|---|---|
| Haskell | `make test-haskell` | 35 pruebas: parseo, cada validación, átomos, trazabilidad, Prolog, reporte, HTML y regeneración con otra entrada |
| Prolog | `make test-prolog` | 26 pruebas sobre hechos y reglas del `conocimiento.pl` generado |
| Navegador | abrir `salida/index.html` | secciones visibles, filtro, botones «Copiar», grafo; ver `docs/evidencia/` |

## Documentación con Doxygen

```
doxygen Doxyfile
```

Abrir `docs/doxygen/html/index.html`. Doxygen no incluye analizador de Haskell, por eso el
`Doxyfile` usa `tools/DoxyFilter.hs` (se ejecuta con `runghc`) para traducir los comentarios
`-- |` y las firmas a algo que Doxygen procesa. Cada función exportada tiene su comentario técnico
y la página «Pipeline Haskell → Prolog → UI» explica el flujo con diagramas.

Convención de comentarios en el código: un bloque `-- |` inmediatamente antes de la función o del tipo.
