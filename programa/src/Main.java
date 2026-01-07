package src;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.util.List;
import java.util.ArrayList;

public class Main {
    private static SudokuGrid grid;
    private static PrologBridge bridge;

    // Game State
    private static List<Integer> currentSolution;
    private static List<Integer> initialPuzzle;
    private static JLabel statusLabel;

    // UI State
    private static JComboBox<String> diffCombo;
    private static JToggleButton themeToggle;

    // Stats
    private static int hintsUsed = 0;
    private static int verifyCount = 0;
    private static int errorCount = 0;
    private static final int MAX_HINTS = 5;

    // Timer
    private static Timer gameTimer;
    private static int secondsElapsed = 0;
    private static JLabel timerLabel;

    public static void main(String[] args) {
        try {
            for (UIManager.LookAndFeelInfo info : UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (Exception ignored) {
        }

        bridge = new PrologBridge("logic.pl");

        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame("Sudoku Prolog - Edición Deluxe + Matrix");
            frame.setSize(1024, 768);
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setLayout(new BorderLayout());

            // --- Header ---
            JPanel header = new JPanel(new BorderLayout());
            header.setBackground(new Color(50, 50, 60));
            header.setBorder(new EmptyBorder(15, 20, 15, 20));

            JLabel title = new JLabel("Sudoku Inteligente");
            title.setFont(new Font("Segoe UI", Font.BOLD, 24));
            title.setForeground(Color.WHITE);
            header.add(title, BorderLayout.WEST);

            // Status Panel
            JPanel statusPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 20, 0));
            statusPanel.setOpaque(false);

            timerLabel = new JLabel("⏱ 00:00");
            timerLabel.setFont(new Font("Segoe UI", Font.BOLD, 18));
            timerLabel.setForeground(new Color(100, 255, 100)); // Bright Green
            statusPanel.add(timerLabel);

            statusLabel = new JLabel("Listo para jugar");
            statusLabel.setFont(new Font("Segoe UI", Font.ITALIC, 14));
            statusLabel.setForeground(new Color(200, 200, 200));
            statusPanel.add(statusLabel);

            header.add(statusPanel, BorderLayout.EAST);

            frame.add(header, BorderLayout.NORTH);

            // --- Grid ---
            JPanel gridContainer = new JPanel(new FlowLayout(FlowLayout.CENTER, 0, 20));
            grid = new SudokuGrid();
            gridContainer.add(grid);
            frame.add(gridContainer, BorderLayout.CENTER);

            // --- Sidebar / Controls ---
            // --- Sidebar / Controls ---
            JPanel sidebar = new JPanel();
            sidebar.setLayout(new BoxLayout(sidebar, BoxLayout.Y_AXIS));
            sidebar.setBorder(new EmptyBorder(20, 20, 20, 20));
            sidebar.setBackground(new Color(245, 245, 245));
            sidebar.setPreferredSize(new Dimension(280, 0)); // Slightly wider

            // --- Configuración ---
            addSectionTitle(sidebar, "CONFIGURACIÓN");

            String[] levels = { "Fácil", "Medio", "Difícil" };
            diffCombo = new JComboBox<>(levels);
            alignComponent(diffCombo);
            sidebar.add(diffCombo);
            sidebar.add(Box.createRigidArea(new Dimension(0, 10)));

            themeToggle = new JToggleButton("Modo Matrix: OFF");
            alignComponent(themeToggle);
            themeToggle.addActionListener(e -> {
                boolean active = themeToggle.isSelected();
                themeToggle.setText(active ? "Modo Matrix: ON" : "Modo Matrix: OFF");
                toggleMatrixMode(active, frame, header, sidebar, gridContainer);
            });
            sidebar.add(themeToggle);
            sidebar.add(Box.createRigidArea(new Dimension(0, 10)));

            JCheckBox chkCandidates = new JCheckBox("Mostrar Candidatos");
            chkCandidates.setOpaque(false);
            alignComponent(chkCandidates);
            chkCandidates.addActionListener(e -> {
                boolean show = chkCandidates.isSelected();
                grid.setShowCandidates(show);
                if (show)
                    updateAllCandidates();
            });
            sidebar.add(chkCandidates);
            sidebar.add(Box.createVerticalGlue()); // Spacer
            sidebar.add(Box.createRigidArea(new Dimension(0, 20)));

            // --- Partida ---
            addSectionTitle(sidebar, "PARTIDA");
            JButton btnSave = createStyledButton("Guardar", new Color(100, 100, 100));
            JButton btnLoad = createStyledButton("Cargar", new Color(100, 100, 100));
            // Group Save/Load in a row? No, keep vertical for consistency or use small
            // buttons?
            // Vertical is safer for BoxLayout.
            sidebar.add(btnSave);
            sidebar.add(Box.createRigidArea(new Dimension(0, 5)));
            sidebar.add(btnLoad);
            sidebar.add(Box.createRigidArea(new Dimension(0, 20)));

            // --- Controles ---
            addSectionTitle(sidebar, "CONTROL");
            JButton btnNew = createStyledButton("Nueva Partida", new Color(70, 130, 180));
            JButton btnRestart = createStyledButton("Reiniciar", new Color(119, 136, 153));
            sidebar.add(btnNew);
            sidebar.add(Box.createRigidArea(new Dimension(0, 5)));
            sidebar.add(btnRestart);
            sidebar.add(Box.createRigidArea(new Dimension(0, 20)));

            // --- Asistencia ---
            addSectionTitle(sidebar, "AYUDA IA");
            JButton btnHint = createStyledButton("Pista (5)", new Color(60, 179, 113));
            JButton btnExplain = createStyledButton("Explicar", new Color(147, 112, 219));

            sidebar.add(btnHint);
            sidebar.add(Box.createRigidArea(new Dimension(0, 5)));
            sidebar.add(btnExplain);
            sidebar.add(Box.createRigidArea(new Dimension(0, 20)));

            // --- Validación ---
            addSectionTitle(sidebar, "VALIDACIÓN");
            JButton btnCheck = createStyledButton("Verificar", new Color(255, 165, 0));
            JButton btnSolve = createStyledButton("Resolver", new Color(220, 20, 60));
            sidebar.add(btnCheck);
            sidebar.add(Box.createRigidArea(new Dimension(0, 5)));
            sidebar.add(btnSolve);

            sidebar.add(Box.createVerticalGlue()); // Push Exit to bottom

            JButton btnExit = createStyledButton("Salir", Color.DARK_GRAY);
            sidebar.add(btnExit);

            // Wire listeners
            btnSave.addActionListener(e -> saveGame());
            btnLoad.addActionListener(e -> loadGame());
            btnNew.addActionListener(e -> newGame(btnHint));
            btnRestart.addActionListener(e -> restartGame());
            btnHint.addActionListener(e -> useHint(btnHint));
            btnExplain.addActionListener(e -> explainSelectedCell());
            btnCheck.addActionListener(e -> verifyGame());
            btnSolve.addActionListener(e -> solveGame());
            btnExit.addActionListener(e -> System.exit(0));

            frame.add(sidebar, BorderLayout.EAST);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
            newGame(btnHint);
        });
    }

    private static void alignComponent(JComponent c) {
        c.setAlignmentX(Component.CENTER_ALIGNMENT);
        c.setMaximumSize(new Dimension(240, 30));
    }

    private static void toggleMatrixMode(boolean active, JFrame frame, JPanel header, JPanel sidebar,
            JPanel gridContainer) {
        Color darkBg = Color.BLACK;
        Color neonGreen = new Color(0, 255, 0);

        if (active) {
            // Matrix
            gridContainer.setBackground(Color.BLACK);
            sidebar.setBackground(new Color(10, 10, 10));
            header.setBackground(new Color(0, 20, 0));
        } else {
            // Normal
            gridContainer.setBackground(new Color(238, 238, 238));
            sidebar.setBackground(new Color(245, 245, 245));
            header.setBackground(new Color(50, 50, 60));
        }
        grid.setMatrixMode(active);
    }

    private static void addSectionTitle(JPanel panel, String text) {
        JLabel label = new JLabel(text);
        label.setFont(new Font("Segoe UI", Font.BOLD, 12));
        label.setForeground(Color.DARK_GRAY);
        label.setAlignmentX(Component.CENTER_ALIGNMENT);
        panel.add(label);
        panel.add(Box.createRigidArea(new Dimension(0, 5)));
    }

    private static JButton createStyledButton(String text, Color bg) {
        JButton btn = new JButton(text);
        btn.setAlignmentX(Component.CENTER_ALIGNMENT);
        btn.setMaximumSize(new Dimension(240, 35)); // Wider and slightly shorter
        btn.setBackground(bg);
        btn.setForeground(Color.WHITE);
        btn.setFocusPainted(false);
        btn.setFocusable(false); // CRITICAL: Prevent stealing focus from Grid Cells
        btn.setFont(new Font("Segoe UI", Font.BOLD, 13)); // Adjusted font
        // Add minimal border
        btn.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(bg.darker(), 1),
                BorderFactory.createEmptyBorder(5, 15, 5, 15)));
        return btn;
    }

    private static void startTimer() {
        if (gameTimer != null)
            gameTimer.stop();
        secondsElapsed = 0;
        timerLabel.setText("⏱ 00:00");

        gameTimer = new Timer(1000, e -> {
            secondsElapsed++;
            int min = secondsElapsed / 60;
            int sec = secondsElapsed % 60;
            timerLabel.setText(String.format("⏱ %02d:%02d", min, sec));
        });
        gameTimer.start();
    }

    private static void stopTimer() {
        if (gameTimer != null)
            gameTimer.stop();
    }

    private static void newGame(JButton btnHint) {
        statusLabel.setText("Generando nuevo puzzle...");

        // Map Combo to Difficulty ID
        String sel = (String) diffCombo.getSelectedItem();
        int diff = 2; // Default Medium
        if ("Fácil".equals(sel))
            diff = 1;
        if ("Difícil".equals(sel))
            diff = 3;

        PrologBridge.GameData data = bridge.generateNewGame(diff);

        if (data == null || data.puzzle.size() != 81) {
            JOptionPane.showMessageDialog(null, "Error: No se pudo conectar con el motor Prolog.");
            statusLabel.setText("Error de conexión");
        } else {
            currentSolution = data.solution;
            initialPuzzle = new ArrayList<>(data.puzzle);
            grid.setBoard(data.puzzle);

            hintsUsed = 0;
            verifyCount = 0;
            errorCount = 0;
            btnHint.setText("Obtener Pista (" + (MAX_HINTS - hintsUsed) + ")");
            btnHint.setEnabled(true);
            statusLabel.setText("Juego iniciado (" + sel + ")");

            startTimer();
        }
    }

    private static void restartGame() {
        if (initialPuzzle != null) {
            grid.setBoard(initialPuzzle);
            statusLabel.setText("Tablero reiniciado");
            secondsElapsed = 0;
            timerLabel.setText("⏱ 00:00");
        }
    }

    private static void useHint(JButton btnHint) {
        if (hintsUsed >= MAX_HINTS) {
            JOptionPane.showMessageDialog(null, "Has agotado tus 5 pistas.");
            return;
        }

        int[] hint = bridge.getHint(grid.getBoard(), currentSolution);
        if (hint != null) {
            int r = hint[0] - 1;
            int c = hint[1] - 1;
            int v = hint[2];

            grid.setCell(r, c, v);
            hintsUsed++;
            btnHint.setText("Obtener Pista (" + (MAX_HINTS - hintsUsed) + ")");
            statusLabel.setText("Pista utilizada: " + v + " en (" + (r + 1) + "," + (c + 1) + ")");

            if (hintsUsed >= MAX_HINTS)
                btnHint.setEnabled(false);
        } else {
            JOptionPane.showMessageDialog(null, "¡Vas muy bien! No hay celdas obvias o vacías.");
        }
    }

    private static void verifyGame() {
        if (currentSolution == null)
            return;
        verifyCount++;

        List<Integer> current = grid.getBoard();
        int[] result = bridge.verifyGame(current, currentSolution);

        if (result[0] > 0)
            errorCount += result[0];

        if (result[0] == 0 && result[1] == 0) {
            stopTimer(); // Stop
            showStats("¡Excelente! Has completado el Sudoku.");
            statusLabel.setText("Juego Completado");
        } else {
            String msg = "Errores encontrados: " + result[0] + "\nCasillas vacías: " + result[1];
            JOptionPane.showMessageDialog(null, msg, "Resultado de Validación", JOptionPane.INFORMATION_MESSAGE);
            statusLabel.setText("Validación: " + result[0] + " errores");
        }
    }

    private static void solveGame() {
        if (currentSolution != null) {
            stopTimer(); // Stop
            grid.setBoard(currentSolution);
            showStats("Resolución Automática");
            statusLabel.setText("Juego Resuelto por la IA");
        }
    }

    // --- Persistence & Explanation ---

    private static void saveGame() {
        try (java.io.ObjectOutputStream out = new java.io.ObjectOutputStream(
                new java.io.FileOutputStream("saved_game.dat"))) {
            out.writeObject(initialPuzzle);
            out.writeObject(currentSolution);
            out.writeObject(grid.getBoard()); // Save user progress
            out.writeInt(secondsElapsed);
            JOptionPane.showMessageDialog(null, "Partida guardada correctamente.");
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "Error al guardar: " + e.getMessage());
        }
    }

    @SuppressWarnings("unchecked")
    private static void loadGame() {
        try (java.io.ObjectInputStream in = new java.io.ObjectInputStream(
                new java.io.FileInputStream("saved_game.dat"))) {
            initialPuzzle = (List<Integer>) in.readObject();
            currentSolution = (List<Integer>) in.readObject();
            List<Integer> progress = (List<Integer>) in.readObject();
            secondsElapsed = in.readInt();

            // Restore state
            grid.setBoard(progress);
            // Re-lock initial cells by comparing with initialPuzzle
            // Since setBoard resets styles, we might need a smarter setBoard or just
            // re-apply lock.
            // Simplified: setBoard normally handles locking if passed non-zeros.
            // Better to re-set 'initial' logic:
            grid.setBoard(progress); // This locks all non-zeros. Wait, this locks EVERYTHING user typed too?
            // Fix: setBoard logic locks if val!=0. We need to distinguish user input.
            // For now, load restores numbers but might lock user input.
            // Improvement: setBoard should take 'initial' mask.
            // Quick fix: Loop and unlock cells that are 0 in initialPuzzle.

            for (int i = 0; i < 81; i++) {
                if (initialPuzzle.get(i) == 0 && progress.get(i) != 0) {
                    // This was user input, unlock it
                    int r = i / 9;
                    int c = i % 9;
                    grid.setEditable(r, c, true);
                }
            }

            timerLabel.setText(String.format("⏱ %02d:%02d", secondsElapsed / 60, secondsElapsed % 60));
            startTimer(); // Resume timer
            JOptionPane.showMessageDialog(null, "Partida cargada.");
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null, "Error al cargar: " + e.getMessage());
        }
    }

    private static void explainSelectedCell() {
        // Need to know which cell is selected or focused.
        // SudokuGrid needs a way to report focus.
        int[] pos = grid.getFocusedCell();
        if (pos == null) {
            JOptionPane.showMessageDialog(null, "Haz clic en una celda vacía para explicar.");
            return;
        }

        List<Integer> current = grid.getBoard();
        String explanation = bridge.explainMove(current, pos[0], pos[1]);
        JOptionPane.showMessageDialog(null, explanation, "Explicación IA", JOptionPane.INFORMATION_MESSAGE);
    }

    private static void updateAllCandidates() {
        statusLabel.setText("Calculando candidatos...");

        SwingWorker<List<List<Integer>>, Void> worker = new SwingWorker<>() {
            @Override
            protected List<List<Integer>> doInBackground() throws Exception {
                // Calculate for ALL cells in one go
                List<Integer> current = grid.getBoard();
                return bridge.getAllCandidates(current);
            }

            @Override
            protected void done() {
                try {
                    List<List<Integer>> allCands = get();
                    if (allCands != null && allCands.size() == 81) {
                        for (int i = 0; i < 81; i++) {
                            int r = i / 9;
                            int c = i % 9;
                            // Only set if raw list is not empty (logic.pl returns [] for filled)
                            // But SetCandidates logic expects null or empty?
                            // Grid handles non-empty list.
                            List<Integer> cands = allCands.get(i);
                            if (cands.isEmpty())
                                cands = null;
                            grid.setCandidates(r, c, cands);
                        }
                    }
                    statusLabel.setText("Candidatos actualizados");
                } catch (Exception e) {
                    statusLabel.setText("Error calc. candidatos");
                    e.printStackTrace();
                }
            }
        };
        worker.execute();
    }

    private static void showStats(String reason) {
        String stats = String.format("""
                %s

                === Estadísticas de Partida ===

                • Tiempo Total: %s
                • Verificaciones: %d
                • Errores Totales: %d
                • Pistas Utilizadas: %d de %d

                ¡Gracias por jugar!
                """, reason, timerLabel.getText(), verifyCount, errorCount, hintsUsed, MAX_HINTS);

        JOptionPane.showMessageDialog(null, stats, "Resumen", JOptionPane.INFORMATION_MESSAGE);
    }
}
