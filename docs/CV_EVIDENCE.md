# CV_EVIDENCE — Sudoku-GUI-App

Supplementary evidence for the **Prolog/logic-programming block** (primary: Aventura-del-Tesoro-Perdido).

## Unique evidence

| Item | Evidence |
|---|---|
| Second Prolog interop mechanism: subprocess invocation (`swipl` via ProcessBuilder + stdout parsing) vs. Aventura's HTTP microservice | `programa/src/PrologBridge.java` |
| Sudoku validation/solving rules in Prolog | `programa/logic.pl` (204 lines) |
| Swing custom grid component | `programa/src/SudokuGrid.java` |

## Interview talking point

Having built Java↔Prolog integration two ways (resident HTTP service vs. per-call subprocess), you can discuss the trade-offs concretely: startup latency per invocation, state persistence, error handling, and deployment complexity.

## ATS keywords (incremental)

Java-Prolog interoperability, ProcessBuilder, subprocess management, Swing, constraint logic.
