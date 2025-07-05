# Maze Generator and Explorer (Lisp)

This project implements a maze generator and explorer using **Lisp** (XLISP-PLUS). It was developed as a final assignment for the **Programming Languages** course (2024-25 academic year).

## 📖 Description

The application consists of two main components: a maze generator that creates random mazes using Depth-First Search (DFS) algorithm, and an interactive explorer that allows users to navigate through the maze. Key features:

- **Maze generation**: creates random mazes of any specified size with walls, paths, entry and exit points.
- **Interactive exploration**: users can navigate the maze using keyboard controls.
- **Score tracking**: records player names and move counts when completing mazes.
- **Visual interface**: provides a graphical representation of the maze and player position.

### 🛠 Features

1. **Maze generation**:
   - Input: filename, rows, and columns.
   - Output: a text file containing the maze structure.
   - Example:
     ```lisp
     (genera "maze.txt" 10 10)
     ```

2. **Maze exploration**:
   - Interactive navigation using arrow keys (WASD or arrow keys).
   - Real-time position tracking.
   - Example:
     ```lisp
     (explora "maze.txt")
     ```

3. **Score tracking**:
   - Records player names and move counts.
   - Displays top 10 scores for each maze.

---

## 🗂 Project Structure

The project consists of two main files:

### `genera.lsp` - Maze Generation
- **Main functions**:
  - `genera`: Main function to generate and save a maze.
  - `genera-intern`: Internal maze generation logic.
  - `dfs`: Depth-First Search algorithm implementation.

- **Support functions**:
  - `genera-matriu-parets`: Creates initial wall matrix.
  - `casella-valida-genera`: Checks if a cell is valid for path creation.
  - `afegir-vores`: Adds borders to the maze.

### `explora.lsp` - Maze Exploration
- **Main functions**:
  - `explora`: Main exploration interface.
  - `explora-intern`: Internal exploration logic.

- **Support functions**:
  - `llegir`: Reads maze from file.
  - `cercar-casella`: Finds specific cells (entry/exit).
  - `casella-valida-explora`: Validates player moves.
  - `dibuixar-laberint`: Graphical display functions.
  - `escriure-dades`/`llegir-dades`: Score handling.

---

## 🚀 How It Works

1. **Maze Generation**:
   - Creates a grid of walls with random entry and exit points.
   - Uses DFS to carve out paths while maintaining maze validity.
   - Adds borders and saves to a text file.

2. **Maze Exploration**:
   - Loads the maze from file and displays it graphically.
   - Tracks player position and validates moves.
   - Records completion time when reaching the exit.

3. **Score Handling**:
   - Stores player names and move counts in a separate file.
   - Displays top scores sorted by fewest moves.

---

## 🎯 Technical Highlights

- **Functional programming approach**:
  - Heavy use of recursion for maze generation and exploration.
  - Immutable data structures where possible.

- **Algorithm implementation**:
  - DFS for maze generation with random path selection.
  - Position validation for player movement.

- **File handling**:
  - Reads and writes maze structures to text files.
  - Maintains score records in separate files.

- **User interface**:
  - Graphical display of maze using simple drawing primitives.
  - Keyboard input handling for navigation.

---

## 📝 Author

- **Gaizka Medina Gordo**  
