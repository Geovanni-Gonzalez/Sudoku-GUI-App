:- use_module(library(clpfd)).
:- use_module(library(lists)).

solve_debug :-
    Board = [
        [_,_,_,5,4,7,_,_,8],
        [4,5,_,_,9,_,_,1,7],
        [7,8,9,_,6,_,_,4,5],
        [_,_,_,_,2,3,_,_,_],
        [_,1,7,_,_,6,_,3,2],
        [2,3,4,_,8,9,5,_,1],
        [6,_,5,8,7,_,_,_,_],
        [_,_,_,9,_,2,4,_,_],
        [3,4,2,6,_,5,7,8,9]
    ],
    append(Board, Vs), Vs ins 1..9,
    write('Checking sudoku rules...'), nl,
    (valid_sudoku(Board) -> write('Board Valid') ; write('Board Invalid')), nl,
    halt.

valid_sudoku(Board) :-
    maplist(all_distinct, Board),
    transpose(Board, Columns),
    maplist(all_distinct, Columns),
    Board = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
    blocks(R1, R2, R3),
    blocks(R4, R5, R6),
    blocks(R7, R8, R9).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).
