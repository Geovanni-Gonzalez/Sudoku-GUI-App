# 🧩 Sudoku AI Deluxe

> **Una fusión de Ingeniería de Software Clásica e Inteligencia Artificial Simbólica.**
>
> Este proyecto demuestra el poder de combinar **Java** (para una interfaz de usuario robusta y reactiva) con **Prolog** (para razonamiento lógico avanzado y resolución de restricciones).

---

## ✨ Características e Innovaciones

### 🧠 Inteligencia Artificial (Logic-Driven)

* **Motor Híbrido**: La lógica del juego no está "hardcoded" en ifs/else, sino definida declarativamente en Prolog usando `clpfd` (Constraint Logic Programming over Finite Domains).
* **XAI (IA Explicativa)**: No solo resuelve, **explica**. El sistema analiza el tablero y justifica por qué una celda debe tener cierto valor (ej. "Naked Single").
* **Smart Candidates (Pencil Marks)**: Visualización en tiempo real de los números posibles en las celdas vacías, calculados dinámicamente por el motor de inferencia.

### 🎮 Experiencia de Usuario (UX)

* **Modo Matrix / Cyberpunk** 🕶️: Un tema visual alternativo de alto contraste (Negro/Verde Neón) para sesiones nocturnas o amantes de la estética hacker.
* **Niveles de Dificultad**: Generación dinámica de tableros (Fácil, Medio, Difícil) ajustando la entropía y pistas iniciales.
* **Persistencia**: Sistema de **Guardado y Carga** para no perder nunca tu progreso.
* **Feedback Inmediato**: Validación de errores, cronómetro integrado y sistema de pistas limitado.

---

## 🛠️ Requisitos del Sistema

1. **Java Development Kit (JDK)**: Versión 11 o superior.
2. **SWI-Prolog**: Debe estar instalado y accesible en el PATH del sistema (`swipl`).
    * *Verificación*: Ejecuta `swipl --version` en tu terminal.

---

## 🚀 Instalación y Ejecución

### 1. Clonar el Repositorio

\`\`\`bash
git clone <https://github.com/Start-Of-The-Art/Sudoku-GUI-App.git>
cd Sudoku-GUI-App
\`\`\`

### 2. Compilar (Windows)

Asegúrate de usar la codificación UTF-8 para soportar caracteres especiales:
\`\`\`powershell
cd programa
javac -encoding UTF-8 src/*.java
\`\`\`

### 3. Ejecutar

\`\`\`powershell
java src.Main
\`\`\`

---

## 📂 Arquitectura

* **`programa/logic.pl`**: El "Cerebro". Define las reglas del Sudoku, genera puzzles, resuelve tableros y calcula candidatos.
* **`src/PrologBridge.java`**: El "Puente". Gestiona la comunicación por subprocesos estándar (stdin/stdout) entre Java y SWI-Prolog.
* **`src/Main.java`**: El "Orquestador". Controla el flujo de la aplicación, menús y eventos.
* **`src/SudokuGrid.java`**: La "Vista". Componente gráfico personalizado con soporte para renderizado avanzado (temas, candidatos).

---

## 📝 Lista de Tareas (Roadmap)

* [x] Motor Básico (Generación y Validación).
* [x] Interfaz Gráfica (Swing).
* [x] **Innovación**: Modo Matrix.
* [x] **Innovación**: Sistema de Recomendación de Candidatos.
* [x] **Innovación**: Explicación de Movimientos (XAI).
* [x] Guardado y Carga.
* [ ] (Futuro) Soporte para Sudoku 16x16.
