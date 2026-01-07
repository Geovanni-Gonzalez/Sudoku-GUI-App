% Sudoku Logic in Prolog
% Course: IC-4700
% Student: [Name]

:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(lists)).

% --- Board Representation ---
% The board is represented as a List of 9 Lists (Rows).
% Each cell contains a number 1-9 or a variable.

% --- Init ---
% True if Board is a valid 9x9 empty grid
init_board(Board) :-
    length(Board, 9),
    maplist(same_length(Board), Board).

% --- Validation (Core) ---
% Checks if the board is valid according to Sudoku rules
valid_sudoku(Board) :-
    % 1. Rows are distinct
    maplist(all_distinct, Board),
    
    % 2. Columns are distinct
    transpose(Board, Columns),
    maplist(all_distinct, Columns),
    
    % 3. 3x3 Blocks are distinct
    Board = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    blocks(R1, R2, R3),
    blocks(R4, R5, R6),
    blocks(R7, R8, R9).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

% --- Solver ---
solve(Board) :-
    init_board(Board),
    append(Board, Vs), Vs ins 1..9,
    valid_sudoku(Board),
    label(Vs).

% --- Generator ---
% Generates a Puzzle with specific difficulty
% Difficulty: 1 (Easy), 2 (Medium), 3 (Hard)
generate_sudoku(Difficulty, Puzzle, Solution) :-
    init_board(Solution),
    
    % 1. Fill Diagonal Blocks
    Solution = [R1,R2,R3, R4,R5,R6, R7,R8,R9],
    fill_block(R1, R2, R3, 0),
    fill_block(R4, R5, R6, 3),
    fill_block(R7, R8, R9, 6),
    
    % 2. Solve
    solve(Solution),
    
    % 3. Create Puzzle
    make_puzzle(Difficulty, Solution, Puzzle).

copy_row(Row, NewRow) :- length(Row, L), length(NewRow, L).

% ... fill_block and sub_list unchanged ...
fill_block(R1, R2, R3, Offset) :-
    sub_list(R1, Offset, S1),
    sub_list(R2, Offset, S2),
    sub_list(R3, Offset, S3),
    append([S1, S2, S3], BlockVars),
    BlockVars ins 1..9,
    all_distinct(BlockVars),
    label(BlockVars).

sub_list(List, Offset, Sub) :-
    length(Prefix, Offset),
    append(Prefix, Rest, List),
    length(Sub, 3),
    append(Sub, _, Rest).

make_puzzle(Diff, Solution, Puzzle) :-
    determine_clues(Diff, Min, Max),
    random_between(Min, Max, CluesCount),
    findall(pos(R,C), (between(1,9,R), between(1,9,C)), AllPos),
    random_permutation(AllPos, Shuffled),
    length(KeptPos, CluesCount),
    append(KeptPos, _, Shuffled),
    
    init_board(Puzzle),
    fill_from_sol(KeptPos, Solution, Puzzle).

determine_clues(1, 40, 50). % Easy
determine_clues(2, 30, 39). % Medium
determine_clues(3, 17, 25). % Hard (Original)

fill_from_sol([], _, _).
fill_from_sol([pos(R,C)|Ps], Sol, Puz) :-
    nth1(R, Sol, RowS), nth1(C, RowS, Val),
    nth1(R, Puz, RowP), nth1(C, RowP, Val),
    fill_from_sol(Ps, Sol, Puz).

% --- Bulk Candidates ---
get_all_candidates(Current, AllCandidates) :-
    init_board(Board),
    maplist(copy_row_vars, Current, Board),
    append(Board, Vs), Vs ins 1..9,  % CRITICAL: Define domains
    valid_sudoku(Board),
    
    findall(Candidates, (
        nth1(_, Board, Row),
        nth1(_, Row, Cell),
        (var(Cell) -> 
            findall(V, indomain(Cell), Candidates)
        ;
            Candidates = []
        )
    ), FlatCandidates),
    chop_9(FlatCandidates, AllCandidates).

copy_row_vars(Row, NewRow) :-
    maplist(copy_cell_var, Row, NewRow).

% Convert 0 (from Java) to fresh Variable, keep others
copy_cell_var(0, _) :- !.
copy_cell_var(N, N).

chop_9([], []).
chop_9(List, [Row|Rest]) :-
    length(Row, 9),
    append(Row, Tail, List),
    chop_9(Tail, Rest).

% --- XAI: Explain Move ---
% Case 1: Naked Single (1 candidate)
explain_move(Current, Row, Col, Reason) :-
    get_all_candidates(Current, AllCands),
    nth1(Row, AllCands, R_Cands),
    nth1(Col, R_Cands, Cands),
    length(Cands, 1),
    Cands = [Val],
    format(string(Reason), "Solo el ~d es posible aqui (Naked Single).", [Val]).

% Case 2: Fallback
explain_move(_, _, _, Reason) :-
    format(string(Reason), "Multiples candidatos posibles o celda llena.", []).

% --- Verification ---
% Errors: Count of cells in Current that match neither Empty nor Solution
% Blanks: Count of Empty cells
verify_sudoku(Current, Solution, Errors, Blanks) :-
    flatten(Current, CList),
    flatten(Solution, SList),
    count_mismatches(CList, SList, 0, Errors),
    include(var, CList, VarList),
    length(VarList, Blanks).

count_mismatches([], [], Acc, Acc).
count_mismatches([C|Cs], [S|Ss], Acc, Count) :-
    (var(C) -> NewAcc = Acc ; 
     C \= S -> NewAcc is Acc + 1 ; 
     NewAcc = Acc),
    count_mismatches(Cs, Ss, NewAcc, Count).

% --- Statistics / Hint ---
get_hint(Current, Solution, HintRow, HintCol, HintVal) :-
    findall(pos(R,C), (
        nth1(R, Current, Row), nth1(C, Row, Cell), var(Cell)
    ), Empties),
    random_member(pos(HintRow, HintCol), Empties),
    nth1(HintRow, Solution, SolRow),
    nth1(HintCol, SolRow, HintVal).

% --- Helpers for Java Interop ---
var_to_zero(X, 0) :- var(X), !.
var_to_zero(X, X).

