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

% The board is transposed (180 degree rotation) every turn to making sure the current player's
% initial home is at the bottom rank (i.e. Rank 1), so the player will always attact
% from rank 1 -> rank 10.

% Valid positions
valid_pos(pos(R, X)) :- X>0, X<10, R>0, R<11.

% dif_pos(A, B) where A =\= B
dif_pos(pos(R1, _), pos(R2, _)) :- dif(R1, R2), !.
dif_pos(pos(_, X1), pos(_, X2)) :- dif(X1, X2), !.

valid_pos_for(general, pos(R, X)) :- R>0, R<4, X>3, X<7.

valid_pos_for(advisor, pos(1, 4)) :- true, !.
valid_pos_for(advisor, pos(1, 6)) :- true, !.
valid_pos_for(advisor, pos(2, 5)) :- true, !.
valid_pos_for(advisor, pos(3, X)) :- valid_pos_for(advisor, pos(1, X)).

valid_pos_for(elephant, pos(1,3)) :- true, !.
valid_pos_for(elephant, pos(1,7)) :- true, !.
valid_pos_for(elephant, pos(3,1)) :- true, !.
valid_pos_for(elephant, pos(3,5)) :- true, !.
valid_pos_for(elephant, pos(3,9)) :- true, !.
valid_pos_for(elephant, pos(5,X)) :- valid_pos_for(elephant, pos(1,X)).

is_one_non_diagonal_step(pos(R1, X1), pos(R2, X2)) :-
    DR is abs(R1-R2),
    DX is abs(X1-X2),
    DR+DX=:=1.

is_n_diagonal_step(pos(R1, X1), pos(R2, X2), Rv) :-
    abs(R1-R2)=:=abs(X1-X2),
    Rv is abs(R1-R2).

get_one_step_towards(pos(R,X1), pos(R, X2), pos(R, X3)) :- X1>X2+1,X3 is X1-1, !.
get_one_step_towards(pos(R,X1), pos(R, X2), pos(R, X3)) :- X2>X1+1,X3 is X1+1, !.
get_one_step_towards(pos(R1,X), pos(R2, X), pos(R3, X)) :- R1>R2+1,R3 is R1-1, !.
get_one_step_towards(pos(R1,X), pos(R2, X), pos(R3, X)) :- R2>R1+1,R3 is R1+1, !.

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

is_valid_horse_move(pos(R1,X1), pos(R2,X2)) :-
    DR is abs(R1-R2),
    DX is abs(X1-X2),
    (DR == 2, DX == 1;
    DR == 1, DX == 2).

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

debug_board([
    piece(cannon, red, pos(1,9)),
    piece(chariot, red, pos(10, 1)),
    piece(chariot, black, pos(1, 1)),
    piece(general, red, pos(1, 5)),
    piece(general, black, pos(10, 5))]).

debug_board2([
    piece(horse, red, pos(10, 2)),
    piece(chariot, black, pos(1, 1)),
    piece(advisor, red, pos(1, 4)),
    piece(general, red, pos(1, 5)),
    piece(advisor, red, pos(1, 6)),
    piece(soldier, black, pos(7, 5)),
    piece(advisor, black, pos(10, 4)),
    piece(advisor, black, pos(10, 6)),
    piece(general, black, pos(10, 5))]).

% transpose the board for black side to take the red stategies.
transpose_pos(piece(Class, Color, pos(R,X)), piece(Class, Color, pos(IR,IX))) :-
    IR is 11 - R,
    IX is 10 - X.
transpose_board([], []).
transpose_board([Piece | T], [NPiece | R]) :-
    transpose_pos(Piece, NPiece),
    transpose_board(T, R).

% State - [Turn, Winner]
% Turn: red, black, none(GameOver)
% Winner: red, black, none.
init_state([red, none]).

% ----------------------------------------------------------------
% Helpers

% Get the chess piece from the board at the specified position.
get_piece(Pos, [piece(Class, Color, Pos)|_], piece(Class, Color, Pos)) :-
    true, !.
get_piece(Pos, [_|T], Piece) :-
    get_piece(Pos, T, Piece).

% piece property accessor.
piece_class(piece(Class,_,_), Class).
piece_color(piece(_,Color,_), Color).
piece_pos(piece(_,_,Pos), Pos).
% piece value assignment
piece_value(none, 0).
piece_value(piece(soldier,_,_), 1).
piece_value(piece(horse,_,_), 2).
piece_value(piece(elephant,_,_), 2).
piece_value(piece(advisor,_,_), 2).
piece_value(piece(cannon,_,_), 3).
piece_value(piece(chariot,_,_), 3).
piece_value(piece(general,_,_), 500).

% drop_piece(Pos, Board, Piece, NBoard).
drop_piece(_, [], none, []) :-
    true, !.
drop_piece(Pos, [Piece|T], Piece, T) :-
    piece_pos(Piece, Pos), !.
drop_piece(Pos, [H|T], Piece, [H|R]) :-
    drop_piece(Pos, T, Piece, R).

% move_piece(Src, Dst, Board, NBoard, PieceDropped)
move_piece(Src, Dst, Board, [piece(Class, Color, Dst)|NBoard], PieceDropped) :-
    drop_piece(Src, Board, piece(Class, Color, Src), NB1),
    drop_piece(Dst, NB1, PieceDropped, NBoard).

% Check the result of a specific step.
check_step(piece(_,_,Src), Dst, Board, clear) :-
    dif_pos(Src, Dst),
    \+ get_piece(Dst, Board, _), !.
check_step(piece(_,_,Src), Src, _, block) :- true, !.
check_step(piece(_, Color, _), Dst, Board, block) :-
    get_piece(Dst, Board, piece(_, Color, _)), !.
check_step(piece(_, Color1, _), Dst, Board, attack) :-
    get_piece(Dst, Board, piece(_, Color2, _)),
    dif(Color1, Color2), !.

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
transit_state(_, piece(general, Color, _), [none, Winner]) :-
    other(Color, Winner), !.
transit_state([Turn, _], none, [NextTurn, none]) :-
    other(Turn, NextTurn), !.
transit_state([Turn, _], piece(Class, _,_), [NextTurn, none]) :-
    dif(Class, general),
    other(Turn, NextTurn), !.

% ----------------------------------------------------------------
% MaxValue Helper

% piece_move_value(Piece, Dst, Board, Value).
move_value(piece(Class,Color,Src), Dst, Board, Value) :-
    check_move(piece(Class,Color,Src), Dst, Board),
    move_piece(Src, Dst, Board, _, PD),
    piece_value(PD, Value), !.

% best_piece_move(Piece, pos(R,X), Board, BestMove, move(Dst, ValueDifference))
best_piece_move(_, pos(11,_), _, BestMove, BestMove) :- true, !.
best_piece_move(Piece, pos(R,10), Board, BestMove, Move) :-
    NR is R + 1,
    best_piece_move(Piece, pos(NR,1), Board, BestMove, Move), !.
best_piece_move(Piece, pos(R,X), Board, move(BestDst, BestValueDiff), Move) :-
    NX is X + 1,
    (move_value(Piece, pos(R,X), Board, MoveValueDiff),
    MoveValueDiff > BestValueDiff ->
    best_piece_move(Piece,pos(R,NX), Board, move(pos(R,X), MoveValueDiff), Move);
    best_piece_move(Piece,pos(R,NX), Board, move(BestDst, BestValueDiff), Move)), !.

% Get the best position for the current piece to move into.
best_piece_move(Piece,Board,Move) :-
    best_piece_move(Piece, pos(1,1), Board, move(pos(0,0), -500), Move).

% best_turn_move(Turn, Board, BestMove, move(Piece,Dst,VD)).
best_turn_move(_, [], BestMove, BestMove) :- true, !.
best_turn_move(Turn, [Piece|T], move(BP,BD,BVD), Move) :-
    (piece_color(Piece, Turn),
    best_piece_move(Piece, [Piece|T], move(Dst, VD)),
    VD > BVD ->
    best_turn_move(Turn, T, move(Piece,Dst,VD), Move);
    best_turn_move(Turn, T, move(BP,BD,BVD), Move)), !.

% Get the best move for the current turn player.
best_turn_move(Turn, Board, Move) :-
    best_turn_move(Turn, Board, move(none, pos(0,0), -500), Move), !.

% ----------------------------------------------------------------
% Move Checking

% Soldier
check_move(piece(soldier, Color, pos(R1, X)), pos(R2, X), Board) :-
    is_a_non_blocking_step(piece(soldier, Color, pos(R1, X)), pos(R2, X), Board),
    R2 is R1+1, !.
check_move(piece(soldier, Color, pos(R, X1)), pos(R, X2), Board) :-
    is_a_non_blocking_step(piece(soldier, Color, pos(R, X1)), pos(R, X2), Board),
    R>5,
    abs(X2-X1)=:=1, !.

% General
check_move(piece(general, Color, Src), Dst, Board) :-
    valid_pos_for(general, Dst),
    is_one_non_diagonal_step(Src, Dst),
    is_a_non_blocking_step(piece(general, Color, Src), Dst, Board), !.
check_move(piece(general, Color, Src), Dst, Board) :-
    other(Color, OtherColor),
    get_piece(Dst, Board, piece(general, OtherColor, Dst)),
    check_move(piece(chariot, Color, Src), Dst, Board), !.

% Advisor
check_move(piece(advisor, Color, Src), Dst, Board) :-
    valid_pos_for(advisor, Dst),
    is_n_diagonal_step(Src, Dst, 1),
    is_a_non_blocking_step(piece(advisor, Color, Src), Dst, Board), !.

% Elephant
check_move(piece(elephant, Color, Src), Dst, Board) :-
    valid_pos_for(elephant, Dst),
    is_n_diagonal_step(Src, Dst, 2),
    get_one_diagonal_step_towards(Src, Dst, Midway),
    check_step(piece(elephant, Color, Src), Midway, Board, clear),
    is_a_non_blocking_step(piece(elephant, Color, Midway), Dst, Board), !.

% Horse
check_move(piece(horse, Color, Src), Dst, Board) :-
    is_valid_horse_move(Src, Dst),
    get_horse_midway_pos(Src, Dst, Midway),
    check_step(piece(horse, Color, Src), Midway, Board, clear),
    is_a_non_blocking_step(piece(horse, Color, Midway), Dst, Board), !.

% Chariot
check_move(piece(chariot, Color, Src), Dst, Board) :-
    is_one_non_diagonal_step(Src, Dst),
    is_a_non_blocking_step(piece(chariot, Color, Src), Dst, Board), !.
check_move(piece(chariot, Color, Src), Dst, Board) :-
    get_one_step_towards(Src, Dst, Midway),
    check_step(piece(chariot, Color, Src), Midway, Board, clear),
    check_move(piece(chariot, Color, Midway), Dst, Board).

% Cannon
check_move(piece(cannon, Color, Src), Dst, Board) :-
    clearmove_checker(piece(cannon, Color, Src), Dst, Board), !.
check_move(piece(cannon, Color, Src), Dst, Board) :-
    cannon_attack_checker(piece(cannon, Color, Src), Dst, Board, clear), !.

% Check if the move is clear.
clearmove_checker(piece(Class, Color, Src), Dst, Board) :-
    is_one_non_diagonal_step(Src, Dst),
    check_step(piece(Class, Color, Src), Dst, Board, clear), !.
clearmove_checker(piece(Class, Color, Src), Dst, Board) :-
    get_one_step_towards(Src, Dst, Midway),
    check_step(piece(Class, Color, Src), Midway, Board, clear),
    clearmove_checker(piece(Class, Color, Midway), Dst, Board).

% Cannon attack checking helpers
next_form(clear, armed) :- true, !.
next_form(armed, fired) :- true, !.
next_form(fired, fired) :- true, !.

cannon_attack_checker(piece(cannon, Color, Src), Dst, Board, armed):-
    is_one_non_diagonal_step(Src, Dst),
    check_step(piece(cannon, Color, Src), Dst, Board, attack), !.
cannon_attack_checker(piece(cannon, Color, Src), Dst, Board, Form):-
    get_one_step_towards(Src, Dst, Midway),
    (is_a_non_clear_step(piece(cannon, Color, Src),Midway, Board) ->
        next_form(Form, NForm),
        cannon_attack_checker(piece(cannon, Color, Midway), Dst, Board, NForm);
        cannon_attack_checker(piece(cannon, Color, Midway), Dst, Board, Form)).

% ----------------------------------------------------------------
% Main

% move(Src, Dst, game(Board, State), game(NBoard, NState))
move(Src, Dst, game(Board, [Turn, Win]), game(NBoard, NState), PieceDropped) :-
    valid_pos(Dst),
    get_piece(Src, Board, Piece),
    piece_color(Piece, Turn),
    check_move(Piece, Dst, Board),
    move_piece(Src, Dst, Board, RawNBoard, PieceDropped),
    transit_state([Turn, Win], PieceDropped, NState),
    transpose_board(RawNBoard, NBoard), !.

% player(Type, game(Board, State),MoveFrom, MoveTo).
player(human, game(Board, _),MoveFrom, MoveTo) :-
    format('Enter the piece position you want to move (in format pos(R,X). e.g. pos(10,1).)?:\n'),
    read(MoveFrom),
    get_piece(MoveFrom, Board, Piece),
    format('Good, you are moving ~w\n', Piece),
    format('Where do you want to move it into?:\n'),
    read(MoveTo).

player(cpu, game(Board, [Turn,_]), MoveFrom, MoveTo) :-
    best_turn_move(Turn, Board, move(piece(_,_,MoveFrom),MoveTo,_)), !.

play(game(Board, [none, Winner]), _, _) :-
    format('===========================\n'),
    format(' Game Over, ~w had won the game! \n', Winner),
    format('===========================\n'),
    draw_board(Board).


play(game(Board, [Turn, none]), TurnPlayerType, OpponentPlayerType) :-
    format('\n....... Start of ~w turn .......\n', Turn),
    format('Turn for [ ~w ] to make a move. \n', Turn),
    format('````````````````````````````````\n'),
    draw_board(Board),
    player(TurnPlayerType, game(Board, [Turn, none]), Src, Dst),
    move(Src, Dst, game(Board, [Turn, none]), NGame, PieceDropped),
    format('========= Valid move =========\n'),
    format('[ ~w ] move: ~w -> ~w\nPiece dropped: ~w\n', [Turn, Src, Dst, PieceDropped]),
    format('`````` End of ~w turn ```````\n\n', Turn),
    play(NGame, OpponentPlayerType, TurnPlayerType);
    format('\n\n[!!!!!!! INVALID MOVE !!!!!!!]\n Either the input is not of the format pos(R,X), or the move is illigal.\n\n'),
    play(game(Board, [Turn, none]), TurnPlayerType, OpponentPlayerType).

start:-
    init_board(Board),
    init_state(State),
    play(game(Board,State), human, human), !.

start(Board, P1, P2):-
    init_state(State),
    play(game(Board,State), P1, P2), !.

vscpu(Board) :-
    init_state(State),
    play(game(Board,State), human, cpu), !.


% ----------------------------------------------------------------
% Prints
draw_piece(piece(chariot, red, _)) :- put_char('Ⓒ'), !.
draw_piece(piece(horse, red, _)) :- put_char('Ⓗ'), !.
draw_piece(piece(elephant, red, _)) :- put_char('Ⓔ'), !.
draw_piece(piece(advisor, red, _)) :- put_char('Ⓐ'), !.
draw_piece(piece(general, red, _)) :- put_char('Ⓖ'), !.
draw_piece(piece(cannon, red, _)) :- put_char('Ⓟ'), !.
draw_piece(piece(soldier, red, _)) :- put_char('Ⓢ'), !.

draw_piece(piece(chariot, black, _)) :- put_char('🅒'), !.
draw_piece(piece(horse, black, _)) :- put_char('🅗'), !.
draw_piece(piece(elephant, black, _)) :- put_char('🅔'), !.
draw_piece(piece(advisor, black, _)) :- put_char('🅐'), !.
draw_piece(piece(general, black, _)) :- put_char('🅖'), !.
draw_piece(piece(cannon, black, _)) :- put_char('🅟'), !.
draw_piece(piece(soldier, black, _)) :- put_char('🅢'), !.


draw_pos(R, 11, _) :-
    format('| ~d\n', R), !.
draw_pos(R, 0, Board) :-
    format('~|~` t~d~2| |', R),
    draw_pos(R, 1, Board).
draw_pos(R, X, Board) :-
    put_char(' '),
    (get_piece(pos(R,X), Board, P) ->
    draw_piece(P);
    put_char(' ')),
    NX is X + 1,
    draw_pos(R, NX, Board).

draw_rank(0, _) :-
    format('    -------------------\n'),
    format('     1 2 3 4 5 6 7 8 9\n'), !.
draw_rank(11, Board) :-
    format('     1 2 3 4 5 6 7 8 9\n'),
    format('    -------------------\n'),
    draw_rank(10, Board), !.
draw_rank(R, Board) :-
    draw_pos(R, 0, Board),
    NR is R - 1,
    draw_rank(NR, Board).

draw_board(Board) :- draw_rank(11,Board), !.
