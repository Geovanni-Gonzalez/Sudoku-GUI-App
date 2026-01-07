package src;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class SudokuGrid extends JPanel {
    private CellField[][] cells = new CellField[9][9];
    private boolean isMatrixMode = false;
    private boolean showCandidates = false;

    public SudokuGrid() {
        setLayout(new GridLayout(9, 9));
        setPreferredSize(new Dimension(540, 540));

        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                CellField field = new CellField(row, col);
                cells[row][col] = field;
                add(field);
            }
        }
    }

    public void setShowCandidates(boolean show) {
        this.showCandidates = show;
        repaint();
    }

    public void setCandidates(int r, int c, List<Integer> cands) {
        cells[r][c].setCandidates(cands);
    }

    public void setMatrixMode(boolean enabled) {
        this.isMatrixMode = enabled;
        this.setBackground(enabled ? Color.BLACK : new Color(238, 238, 238));
        for (int row = 0; row < 9; row++) {
            for (int col = 0; col < 9; col++) {
                updateCell(row, col);
            }
        }
    }

    private void updateCell(int r, int c) {
        CellField f = cells[r][c];
        boolean isEditable = f.isEditable();

        Color bg, fg;
        Color border = isMatrixMode ? new Color(0, 255, 0) : new Color(50, 50, 50);

        // Border
        int top = (r % 3 == 0) ? 3 : 1;
        int left = (c % 3 == 0) ? 3 : 1;
        int bottom = (r == 8) ? 3 : 1;
        int right = (c == 8) ? 3 : 1;
        f.setBorder(BorderFactory.createMatteBorder(top, left, bottom, right, border));

        if (isMatrixMode) {
            f.setCaretColor(Color.GREEN);
            if (!isEditable) {
                bg = new Color(0, 20, 0);
                fg = new Color(0, 255, 0);
            } else {
                bg = Color.BLACK;
                fg = new Color(150, 255, 150);
            }
        } else {
            f.setCaretColor(Color.BLACK);
            if (!isEditable) {
                bg = new Color(230, 230, 240);
                fg = new Color(20, 20, 40);
            } else {
                bg = Color.WHITE;
                fg = new Color(0, 102, 204);
            }
        }
        f.setBackground(bg);
        f.setForeground(fg);
    }

    public void setBoard(List<Integer> flatBoard) {
        if (flatBoard.size() != 81)
            return;
        for (int i = 0; i < 81; i++) {
            int r = i / 9;
            int c = i % 9;
            int val = flatBoard.get(i);

            CellField f = cells[r][c];
            f.setCandidates(null); // Clear old candidates
            if (val != 0) {
                f.setText(String.valueOf(val));
                f.setEditable(false);
            } else {
                f.setText("");
                f.setEditable(true);
            }
            updateCell(r, c);
        }
    }

    public List<Integer> getBoard() {
        List<Integer> list = new ArrayList<>();
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                String text = cells[r][c].getText().trim();
                if (text.isEmpty())
                    list.add(0);
                else {
                    try {
                        list.add(Integer.parseInt(text));
                    } catch (NumberFormatException e) {
                        list.add(0);
                    }
                }
            }
        }
        return list;
    }

    public int[] getFocusedCell() {
        for (int r = 0; r < 9; r++) {
            for (int c = 0; c < 9; c++) {
                if (cells[r][c].hasFocus())
                    return new int[] { r, c };
            }
        }
        return null;
    }

    public void setEditable(int r, int c, boolean editable) {
        cells[r][c].setEditable(editable);
        if (editable) {
            cells[r][c].setForeground(isMatrixMode ? new Color(150, 255, 150) : new Color(0, 102, 204));
            cells[r][c].setBackground(isMatrixMode ? Color.BLACK : Color.WHITE);
        }
    }

    public void setCell(int r, int c, int val) {
        cells[r][c].setText(String.valueOf(val));
        cells[r][c].setCandidates(null);
        updateCell(r, c);
        cells[r][c].setBackground(isMatrixMode ? new Color(0, 50, 0) : new Color(240, 255, 240));
    }

    // Custom Field to draw Pencil Marks
    class CellField extends JTextField {
        int r, c;
        List<Integer> cands;

        public CellField(int r, int c) {
            this.r = r;
            this.c = c;
            setHorizontalAlignment(JTextField.CENTER);
            setFont(new Font("Segoe UI", Font.BOLD, 24));
        }

        public void setCandidates(List<Integer> c) {
            this.cands = c;
            repaint();
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            if (showCandidates && cands != null && getText().isEmpty()) {
                Graphics2D g2 = (Graphics2D) g;
                g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
                g2.setFont(new Font("Segoe UI", Font.PLAIN, 10));
                g2.setColor(isMatrixMode ? Color.GREEN : Color.GRAY);

                // 3x3 grid inside cell
                int w = getWidth() / 3;
                int h = getHeight() / 3;

                for (int num : cands) {
                    int row = (num - 1) / 3;
                    int col = (num - 1) % 3;
                    int x = col * w + w / 2 - 3;
                    int y = row * h + h - 2;
                    g2.drawString(String.valueOf(num), x, y);
                }
            }
        }
    }
}
