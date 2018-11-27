% model the position
%   1 2 3 4 5 6 7 8 9
%10  _ _ _ _ _ _ _ _
% 9 |_|_|_|_|_|_|_|_|
% 8 |_|_|_|_|_|_|_|_|
% 7 |_|_|_|_|_|_|_|_|
%_6_|_|_|_|_|_|_|_|_|__
%          River
%_5____________________
% 4 |_|_|_|_|_|_|_|_|
% 3 |_|_|_|_|_|_|_|_|
% 2 |_|_|_|_|_|_|_|_|
% 1 |_|_|_|_|_|_|_|_|

% The board is inversed (upside-down) every turn to making sure the current player's
% initial home is at the bottom rank (i.e. Rank 1), so the player will always attact
% from rank 1 -> rank 10.

% Valid positions
valid_pos(pos(R, X)) :- X>0, X<10, R>0, R<11.

% dif_pos(A, B) where A =\= B
dif_pos(pos(R1, _), pos(R2, _)) :- dif(R1, R2).
dif_pos(pos(_, X1), pos(_, X2)) :- dif(X1, X2).

valid_pos_for(general, pos(R, X)) :- R>0, R<4, X>3, X<7.

valid_pos_for(advisor, pos(1, 4)).
valid_pos_for(advisor, pos(1, 6)).
valid_pos_for(advisor, pos(2, 5)).
valid_pos_for(advisor, pos(3, X)) :- valid_pos_for(advisor, pos(1, X)).

valid_pos_for(elephant, pos(1,3)).
valid_pos_for(elephant, pos(1,7)).
valid_pos_for(elephant, pos(3,1)).
valid_pos_for(elephant, pos(3,5)).
valid_pos_for(elephant, pos(3,9)).
valid_pos_for(elephant, pos(5,X)) :- valid_pos_for(elephant, pos(1,X)).

is_one_non_diagonal_step(pos(R1, X1), pos(R2, X2)) :-
    DR is abs(R1-R2),
    DX is abs(X1-X2),
    DR+DX=:=1.

is_n_diagonal_step(pos(R1, X1), pos(R2, X2), abs(R1-R2)) :-
    abs(R1-R2)=:=abs(X1-X2).

get_one_step_towards(pos(R,X1), pos(R, X2), pos(R, X3)) :- X1>X2,X3 is X1-1.
get_one_step_towards(pos(R,X1), pos(R, X2), pos(R, X3)) :- X2>X1,X3 is X1+1.
get_one_step_towards(pos(R1,X), pos(R2, X), pos(R3, X)) :- R1>R2,R3 is R1-1.
get_one_step_towards(pos(R1,X), pos(R2, X), pos(R3, X)) :- R2>R1,R3 is R1+1.

get_one_diagonal_step_towards(pos(R1,X1), pos(R2, X2), pos(R3, X3)) :-
    is_n_diagonal_step(pos(R1,X1), pos(R2,X2), N),
    R3 is R1 + (R2-R1)/N,
    X3 is X1 + (X2-X1)/N.

get_horse_midway_pos(pos(R1,X1), pos(R2,X2), pos(R1,X3)) :-
    abs(R2-R1) =:= abs(X2-X1) - 1,
    X3 is (X1+X2)/2.
get_horse_midway_pos(pos(R1,X1), pos(R2,X2), pos(R3,X1)) :-
    abs(R2-R1) =:= abs(X2-X1) + 1,
    R3 is (R1+R2)/2.

% ----------------------------------------------------------------
% Initialization
init_board([
    piece(chariot, red, pos(1, 1)),
    piece(chariot, red, pos(1, 9)),
    piece(horse, red, pos(1, 2)),
    piece(horse, red, pos(1, 8)),
    piece(elephant, red, pos(1, 3)),
    piece(elephant, red, pos(1, 7)),
    piece(advisor, red, pos(1, 4)),
    piece(advisor, red, pos(1, 6)),
    piece(general, red, pos(1, 5)),
    piece(cannon, red, pos(3, 2)),
    piece(cannon, red, pos(3, 8)),
    piece(soldier, red, pos(4, 1)),
    piece(soldier, red, pos(4, 3)),
    piece(soldier, red, pos(4, 5)),
    piece(soldier, red, pos(4, 7)),
    piece(soldier, red, pos(4, 9)),
    piece(chariot, black, pos(10, 1)),
    piece(chariot, black, pos(10, 9)),
    piece(horse, black, pos(10, 2)),
    piece(horse, black, pos(10, 8)),
    piece(elephant, black, pos(10, 3)),
    piece(elephant, black, pos(10, 7)),
    piece(advisor, black, pos(10, 4)),
    piece(advisor, black, pos(10, 6)),
    piece(general, black, pos(10, 5)),
    piece(cannon, black, pos(8, 2)),
    piece(cannon, black, pos(8, 8)),
    piece(soldier, black, pos(7, 1)),
    piece(soldier, black, pos(7, 3)),
    piece(soldier, black, pos(7, 5)),
    piece(soldier, black, pos(7, 7)),
    piece(soldier, black, pos(7, 9))]).

% inverse the board for black side to take the red stategies.
inverse_board([], []).
inverse_board([piece(Class, Color, pos(R, X)) | T], [piece(Class, Color, pos(IR, X))| R]) :-
    R+IR=:=11,
    inverse_board(T, R).

% State - [Turn, Winner]
% Turn: red, black, none(GameOver)
% Winner: red, black, none.
init_state([red, none]).

% ----------------------------------------------------------------
% Helpers

% Get the chess piece from the board at the specified position.
get_piece(_, [], none).
get_piece(Pos, [piece(Class, Color, Pos)|_], piece(Class, Color, Pos)).
get_piece(Pos, [_|T], Piece) :-
    get_piece(Pos, T, Piece).

% move_piece(Piece, Dst, Board, NBoard, PieceDropped)
move_piece(piece(Class, Color, _), Dst, [], [piece(Class, Color, Dst)], none).
move_piece(Piece, Dst, [piece(Class, Color, Dst)|T], R, piece(Class, Color, Dst)) :-
    move_piece(Piece, Dst, T, R, none).
move_piece(piece(Class, Color, Src), Dst, [piece(Class, Color, Src)|T], R, Dropped) :-
    move_piece(piece(Class, Color, Src), Dst, T, R, Dropped).

% Check the result of a specific step.
check_step(_, Dst, Board, clear) :-
    valid_pos(Dst),
    get_piece(Dst, Board, none).
check_step(piece(_, Color, _), Dst, Board, block) :-
    valid_pos(Dst),
    get_piece(Dst, Board, piece(_, Color, _)).
check_step(piece(_, Color1, _), Dst, Board, attack) :-
    valid_pos(Dst),
    get_piece(Dst, Board, piece(_, Color2, _)),
    dif(Color1, Color2).

% If the Dst is either a clear step or an attack step.
is_a_non_blocking_step(Piece, Dst, Board) :-
    check_step(Piece, Dst, Board, Res),
    dif(Res, block).

% If the Dst is either a block step or an attack step.
is_a_non_clear_step(Piece, Dst, Board) :-
    check_step(Piece, Dst, Board, Res),
    dif(Res, clear).

other(red, black).
other(black, red).

% Transit the game state with considering the pieces droped.
% transit_state(OldState, PieceDropped, NewState).
transit_state(_, piece(general, Color, _), [none, other(Color)]).
transit_state([Turn, none], _, [other(Turn), none]).


% ----------------------------------------------------------------
% Move Checking

% Soldier
check_move(piece(soldier, pos(R1, X)), pos(R2, X), Board) :-
    is_a_non_blocking_step(piece(soldier, pos(R1, X)), pos(R2, X), Board),
    R2-R1=:=1.
check_move(piece(soldier, pos(R, X1)), pos(R, X2), Board) :-
    is_a_non_blocking_step(piece(soldier, pos(R, X1)), pos(R, X2), Board),
    R>5,
    abs(X2-X1)=:=1.

% General
check_move(piece(general, Color, Src), Dst, Board) :-
    valid_pos_for(general, Dst),
    is_one_non_diagonal_step(Src, Dst),
    is_a_non_blocking_step(piece(general, Color, Src), Dst, Board).

% Advisor
check_move(piece(advisor, Color, Src), Dst, Board) :-
    valid_pos_for(advisor, Dst),
    is_n_diagonal_step(Src, Dst, 1),
    is_a_non_blocking_step(piece(advisor, Color, Src), Dst, Board).

% Elephant
check_move(piece(elephant, Color, Src), Dst, Board) :-
    valid_pos_for(elephant, Dst),
    is_n_diagonal_step(Src, Dst, 2),
    get_one_diagonal_step_towards(Src, Dst, Midway),
    check_step(piece(elephant, Color, Src), Midway, Board, clear),
    is_a_non_blocking_step(piece(elephant, Color, Midway), Dst, Board).

% Horse
check_move(piece(horse, Color, Src), Dst, Board) :-
    valid_pos(Dst),
    get_horse_midway_pos(Src, Dst, Midway),
    check_step(piece(horse, Color, Src), Midway, Board, clear),
    is_a_non_blocking_step(piece(horse, Color, Midway), Dst, Board).

% Chariot
check_move(piece(chariot, Color, Src), Dst, Board) :-
    valid_pos(Dst),
    is_one_non_diagonal_step(Src, Dst),
    is_a_non_blocking_step(piece(chariot, Color, Src), Dst, Board).
check_move(piece(chariot, Color, Src), Dst, Board) :-
    \+ is_one_non_diagonal_step(Src, Dst),
    get_one_step_towards(Src, Dst, Midway),
    valid_pos(Midway),
    check_step(piece(chariot, Color, Src), Midway, Board, clear),
    check_move(piece(chariot, Color, Midway), Dst, Board).

% Cannon
check_move(Piece, Dst, Board) :-
    cannon_move_checker(Piece, Dst, Board, loading).

% Cannon move checking helper
cannon_move_checker(piece(cannon, Color, Src), Dst, Board, _) :-
    valid_pos(Dst),
    is_one_non_diagonal_step(Src, Dst),
    check_step(piece(cannon, Color, Src), Dst, Board, clear).
cannon_move_checker(piece(cannon, Color, Src), Dst, Board, armed) :-
    is_one_non_diagonal_step(Src, Dst),
    check_step(piece(cannon, Color, Src), Dst, Board, attack).
cannon_move_checker(piece(cannon, Color, Src), Dst, Board, State) :-
    \+ is_one_non_diagonal_step(Src, Dst),
    get_one_step_towards(Src, Dst, Midway),
    valid_pos(Midway),
    check_step(piece(cannon, Color, Src), Midway, Board, clear),
    cannon_move_checker(piece(cannon, Color, Midway), Dst, Board, State).
cannon_move_checker(piece(cannon, Color, Src), Dst, Board, loading) :-
    \+ is_one_non_diagonal_step(Src, Dst),
    get_one_step_towards(Src, Dst, Midway),
    is_a_non_clear_step(piece(cannon, Color, Src), Midway, Board),
    cannon_move_checker(piece(cannon, Color, Midway), Dst, Board, armed).

% ----------------------------------------------------------------
% Move

% move(Src, Dst, game(Board, State), game(NBoard, NState))
move(Src, Dst, game(Board, [Turn, Win]), game(NBoard, NState)) :-
    valid_pos(Dst),
    get_piece(Src, Board, piece(Class, Turn, Src)),
    check_move(piece(Class, Turn, Src), Dst, Board),
    move_piece(piece(Class, Turn, Src), Dst, Board, RawNBoard, PieceDropped),
    transit_state([Turn, Win], PieceDropped, NState),
    inverse_board(RawNBoard, NBoard).
