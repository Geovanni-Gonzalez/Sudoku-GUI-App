package src;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class PrologBridge {

    private final String logicPath;

    public static class GameData {
        public List<Integer> puzzle;
        public List<Integer> solution;

        public GameData(List<Integer> p, List<Integer> s) {
            this.puzzle = p;
            this.solution = s;
        }
    }

    public PrologBridge(String logicPath) {
        this.logicPath = logicPath;
    }

    private String runQuery(String query) {
        try {
            ProcessBuilder pb = new ProcessBuilder(
                    "swipl",
                    "-q",
                    "-f", logicPath,
                    "-g", query + ", halt",
                    "-t", "halt");
            pb.redirectErrorStream(true);
            Process process = pb.start();

            BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
            StringBuilder output = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty())
                    continue;
                if (line.startsWith("Warning:"))
                    continue; // Ignore Prolog warnings
                output.append(line);
            }
            process.waitFor();
            String res = output.toString().trim();
            System.out.println("Query: [" + query + "]");
            System.out.println("Prolog raw output: " + res);
            return res;
        } catch (Exception e) {
            e.printStackTrace();
            return "ERROR";
        }
    }

    public GameData generateNewGame(int difficulty) {
        // Difficulty: 1=Easy, 2=Medium, 3=Hard
        // CleanFP ensures we get 0s instead of variables like _G234
        String query = String.format(
                "generate_sudoku(%d, P, S), flatten(P, FP), maplist(var_to_zero, FP, CleanFP), flatten(S, FS), write(CleanFP), write('#'), write(FS)",
                difficulty);
        String raw = runQuery(query);

        if (raw == null || !raw.contains("#")) {
            System.err.println("Bridge Error: Invalid response format -> " + raw);
            return null;
        }
        String[] parts = raw.split("#");
        if (parts.length < 2)
            return null;

        return new GameData(parseList(parts[0]), parseList(parts[1]));
    }

    public int[] verifyGame(List<Integer> current, List<Integer> solution) {
        String curStr = toPrologRows(current);
        String solStr = toPrologRows(solution);

        String query = String.format("verify_sudoku(%s, %s, E, B), write(E), write('#'), write(B)", curStr, solStr);
        String raw = runQuery(query);

        try {
            String[] parts = raw.split("#");
            int e = Integer.parseInt(parts[0].trim());
            int b = Integer.parseInt(parts[1].trim());
            return new int[] { e, b };
        } catch (Exception ex) {
            return new int[] { -1, -1 };
        }
    }

    public List<Integer> solveGame(List<Integer> flatBoard) {
        String rows = toPrologRows(flatBoard);
        String query = "Board = " + rows + ", solve(Board), flatten(Board, F), write(F)";
        return parseList(runQuery(query));
    }

    // Returns [Row, Col, Value] or null
    public int[] getHint(List<Integer> current, List<Integer> solution) {
        String curStr = toPrologRows(current);
        String solStr = toPrologRows(solution);
        String query = String.format("get_hint(%s, %s, R, C, V), write(R), write('#'), write(C), write('#'), write(V)",
                curStr, solStr);
        String raw = runQuery(query);

        try {
            String[] parts = raw.split("#");
            int r = Integer.parseInt(parts[0].trim());
            int c = Integer.parseInt(parts[1].trim());
            int v = Integer.parseInt(parts[2].trim());
            return new int[] { r, c, v };
        } catch (Exception ex) {
            return null;
        }
    }

    // Returns list of possible numbers for a cell
    public List<Integer> getCandidates(List<Integer> current, int row, int col) {
        String curStr = toPrologRows(current);
        // Row/Col 1-based for Prolog
        String query = String.format("get_candidates(%s, %d, %d, L), write(L)", curStr, row + 1, col + 1);
        String raw = runQuery(query);
        return parseList(raw);
    }

    // Returns List of 81 lists (one per cell, empty if filled)
    public List<List<Integer>> getAllCandidates(List<Integer> current) {
        String curStr = toPrologRows(current);
        // Query: get_all_candidates(C, All), flatten(All, Flat), write(Flat)
        // Flattening makes it hard to reconstruct.
        // Let's write the raw structure or handle parsing carefully.
        // Actually, let's just use the simpler approach:
        // Prolog writes [[1,2],[3],[],...]
        String query = String.format("get_all_candidates(%s, All), write(All)", curStr);
        String raw = runQuery(query);
        return parseNestedList(raw);
    }

    // Returns explanation string
    public String explainMove(List<Integer> current, int row, int col) {
        String curStr = toPrologRows(current);
        String query = String.format("explain_move(%s, %d, %d, S), write(S)", curStr, row + 1, col + 1);
        return runQuery(query).replace("'", ""); // Cleanup prolog strings
    }

    private List<List<Integer>> parseNestedList(String raw) {
        List<List<Integer>> result = new ArrayList<>();
        if (raw == null || !raw.startsWith("["))
            return result;

        // Remove outer brackets
        String content = raw.substring(1, raw.length() - 1);

        // Split by "],[" is risky if spaces exist.
        // Simple state machine parser or regex for [[...], [...]]
        // Since Prolog outputs compact: [],[1,2],[3] etc.

        // Quick/Dirty parser for this specific format
        int depth = 0;
        StringBuilder currentBuffer = new StringBuilder();
        for (char c : content.toCharArray()) {
            if (c == '[') {
                depth++;
                if (depth > 1)
                    currentBuffer.append(c);
            } else if (c == ']') {
                depth--;
                if (depth == 0) {
                    result.add(parseList("[" + currentBuffer.toString() + "]"));
                    currentBuffer.setLength(0);
                } else {
                    currentBuffer.append(c);
                }
            } else if (c == ',') {
                if (depth > 0)
                    currentBuffer.append(c);
                // if depth == 0, it's the separator between lists, ignore
            } else {
                currentBuffer.append(c);
            }
        }
        return result;
    }

    private List<Integer> parseList(String raw) {
        List<Integer> result = new ArrayList<>();
        if (raw == null || !raw.startsWith("["))
            return result;

        String content = raw.substring(1, raw.length() - 1);
        String[] parts = content.split(",");
        for (String p : parts) {
            p = p.trim();
            if (p.matches("\\d+")) {
                result.add(Integer.parseInt(p));
            } else {
                result.add(0);
            }
        }
        return result;
    }

    private String toPrologRows(List<Integer> flat) {
        StringBuilder sb = new StringBuilder("[");
        for (int r = 0; r < 9; r++) {
            sb.append("[");
            for (int c = 0; c < 9; c++) {
                int val = flat.get(r * 9 + c);
                if (val == 0)
                    sb.append("_");
                else
                    sb.append(val);
                if (c < 8)
                    sb.append(",");
            }
            sb.append("]");
            if (r < 8)
                sb.append(",");
        }
        sb.append("]");
        return sb.toString();
    }
}
