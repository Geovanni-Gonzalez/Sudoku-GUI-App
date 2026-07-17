# TECHNICAL_REVIEW — Sudoku-GUI-App

Fecha de revisión: 2026-07-16. Método: análisis estático, enunciado (`docs/Proyecto Programado 3 -Sudoku.md`), CI y git.

## Comprensión

Sudoku de escritorio con **arquitectura híbrida Java + Prolog** (~1,090 LOC): GUI Swing (`Main.java` 475 LOC, `SudokuGrid.java` con celdas custom), y la **lógica de validación/resolución en Prolog** (`logic.pl`, 204 líneas) invocada desde Java vía **subproceso `swipl`** (`PrologBridge.java`: ProcessBuilder + parsing de stdout). Segunda integración Prolog del portafolio, con mecanismo distinto al de Aventura-del-Tesoro-Perdido (subproceso vs. microservicio HTTP).

## Evaluación

| Aspecto | Estado |
|---|---|
| Lógica declarativa en Prolog (validación de tablero, resolución) | 🟦 `programa/logic.pl` |
| Puente Java↔Prolog por subproceso con parsing de salida | 🟦 `PrologBridge.java` (226 LOC, ProcessBuilder + BufferedReader + waitFor) |
| GUI Swing con grid custom | 🟦 `SudokuGrid.java` (`CellField` interno) |
| Guardado de partida | 🟦 `saved_game.dat` (generado) |
| Higiene | ⛔→✅ 6 `.class` + `saved_game.dat` trackeados — corregido (`git rm --cached`), pendiente de commit |
| Tests | ⛔ Ninguno; CI compila |

## Veredicto

Nivel: **Junior+/Mid en integración**. Interés real: comparar los dos mecanismos de interop Prolog del portafolio (subproceso aquí, HTTP en Aventura) — buen material de entrevista sobre trade-offs (latencia por invocación vs. proceso residente). Citar en CV solo como refuerzo del bloque Prolog.
