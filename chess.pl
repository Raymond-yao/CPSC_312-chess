
% model the position
%   1_2_3_4_5_6_7_8_9
%10 |_|_|_|_|_|_|_|_|
% 9 |_|_|_|_|_|_|_|_|
% 8 |_|_|_|_|_|_|_|_|
% 7 |_|_|_|_|_|_|_|_|
%_6_|_|_|_|_|_|_|_|_|__
%          River
%______________________
% 5 |_|_|_|_|_|_|_|_|
% 4 |_|_|_|_|_|_|_|_|
% 3 |_|_|_|_|_|_|_|_|
% 2 |_|_|_|_|_|_|_|_|
% 1 |_|_|_|_|_|_|_|_|

% chess(C, T, S) the chess C has type T and belongs to Side S (either red or black)
% red side
chess(red_chariot_1, chariot, red).
chess(red_chariot_2, chariot, red).
chess(red_horse_1, horse, red).
chess(red_horse_2, horse, red).
chess(red_elephant_1, elephant, red).
chess(red_elephant_2, elephant, red).
chess(red_advisor_1, advisor, red).
chess(red_advisor_2, advisor, red).
chess(red_general, general, red).
chess(red_cannon_1, cannon, red).
chess(red_cannon_2, cannon, red).
chess(red_soldier_1, soldier, red).
chess(red_soldier_2, soldier, red).
chess(red_soldier_3, soldier, red).
chess(red_soldier_4, soldier, red).
chess(red_soldier_5, soldier, red).

% black side
chess(black_chariot_1, chariot, black).
chess(black_chariot_2, chariot, black).
chess(black_horse_1, horse, black).
chess(black_horse_2, horse, black).
chess(black_elephant_1, elephant, black).
chess(black_elephant_2, elephant, black).
chess(black_advisor_1, advisor, black).
chess(black_advisor_2, advisor, black).
chess(black_general, general, black).
chess(black_cannon_1, cannon, black).
chess(black_cannon_2, cannon, black).
chess(black_soldier_1, soldier, black).
chess(black_soldier_2, soldier, black).
chess(black_soldier_3, soldier, black).
chess(black_soldier_4, soldier, black).
chess(black_soldier_5, soldier, black).

% pos(X,Y,C) the chess C at Xth row, Yth column.
%initial position
% red side
pos(1,1,red_chariot_1).
pos(1,2,red_horse_1).
pos(1,3,red_elephant_1).
pos(1,4,red_advisor_1).
pos(1,5,red_general).
pos(1,6,red_advisor_2).
pos(1,7,red_elephant_2).
pos(1,8,red_horse_2).
pos(1,9,red_chariot_2).

pos(3,2,red_cannon_1).
pos(3,8,red_cannon_2).
pos(4,1,red_soldier_1).
pos(4,3,red_soldier_2).
pos(4,5,red_soldier_3).
pos(4,7,red_soldier_4).
pos(4,9,red_soldier_5).

% black side
pos(10,1,black_chariot_1).
pos(10,2,black_horse_1).
pos(10,3,black_elephant_1).
pos(10,4,black_advisor_1).
pos(10,5,black_general).
pos(10,6,black_advisor_2).
pos(10,7,black_elephant_2).
pos(10,8,black_horse_2).
pos(10,9,black_chariot_2).

pos(8,2,black_cannon_1).
pos(8,8,black_cannon_2).
pos(7,1,black_soldier_1).
pos(7,3,black_soldier_2).
pos(7,5,black_soldier_3).
pos(7,7,black_soldier_4).
pos(7,9,black_soldier_5).

%rules
valid_pos(X,Y) :- X >= 1, X =< 10, Y >= 1, Y =< 9.

% clear_path(P1, P2) true if there is no chess between position P1 and P2
% move(F, T) true if the chess at pos F is movable to pos T
clear_path(pos(X,Y1,_), pos(X,Y2,_)) :- Y1 =< 8, Y1 >= 1, Y2 is Y1 + 1.
clear_path(pos(X,Y1,_), pos(X,Y2,_)) :- Y1 =< 9, Y1 >= 2, Y2 is Y1 - 1.
clear_path(pos(X1,Y,_), pos(X2,Y,_)) :- X1 =< 9, X1 >= 1, X2 is X1 + 1.
clear_path(pos(X1,Y,_), pos(X2,Y,_)) :- X1 =< 10, X1 >= 2, X2 is X1 - 1.
clear_path(pos(X,Y1,_), pos(X,Y2,_)) :-
    Y1 < Y2,
    Y3 is Y2 - 1,
    Y3 =< 9,
    Y1 < Y3,
    clear_path(pos(X,Y1,_), pos(X,Y3,_)),
    \+ pos(X,Y3,_).
clear_path(pos(X,Y2,_), pos(X,Y2,_)) :-
    Y1 < Y2,
    Y3 is Y1 + 1,
    Y3 >= 1,
    Y3 < Y2,
    clear_path(pos(X,Y1,_), pos(X,Y3,_)),
    \+ pos(X,Y3,_).
clear_path(pos(X,Y1,_), pos(X,Y2,_)) :-
    Y1 > Y2,
    Y3 is Y1 - 1,
    Y3 =< 9,
    Y3 > Y2,
    clear_path(pos(X,Y3,_), pos(X,Y2,_)),
    \+ pos(X,Y3,_).
clear_path(pos(X,Y1,_), pos(X,Y2,_)) :-
    Y1 > Y2,
    Y3 is Y2 + 1,
    Y3 >= 1,
    Y1 > Y3,
    clear_path(pos(X,Y1,_), pos(X,Y3,_)),
    \+ pos(X,Y3,_).

clear_path(pos(X1,Y,_), pos(X2,Y,_)) :-
    X1 < X2,
    X3 is X2 - 1,
    X3 =< 9,
    X1 < X3,
    clear_path(pos(X1,Y,_), pos(X3,Y,_)),
    \+ pos(X3,Y,_).
clear_path(pos(X1,Y,_), pos(X2,Y,_)) :-
    X1 < X2,
    X3 is X1 + 1,
    X3 >= 1,
    X3 < X2,
    clear_path(pos(X3,Y,_), pos(X2,Y,_)),
    \+ pos(X3,Y,_).
clear_path(pos(X1,Y,_), pos(X2,Y,_)) :-
    X1 > X2,
    X3 is X1 - 1,
    X3 =< 9,
    X3 > X2,
    clear_path(pos(X3,Y,_), pos(X2,Y,_)),
    \+ pos(X3,Y,_).
clear_path(pos(X1,Y, _), pos(X2,Y,_)) :-
    X1 > X2,
    X3 is X2 + 1,
    X3 >= 1,
    X1 > X3,
    clear_path(pos(X1,Y,_), pos(X3,Y,_)),
    \+ pos(X3,Y,_).

% chariot move rules
% A -> D iff B, C are empty, D is empty or on different side
%
% A - B - C - D
%
move(pos(X,Y1,P), pos(X,Y2,P2)) :- 
    pos(X,Y1,P),
    Y1 >= 1, Y1 =< 9, Y2 >= 1, Y2 =< 9, dif(Y1, Y2),
    clear_path(pos(X,Y1,P), pos(X,Y2,P2)),
    chess(P, chariot, R),
    pos(X,Y2,P2),
    chess(P2, _, B),
    dif(R,B).
move(pos(X,Y1,P), pos(X,Y2,P2)) :- 
    pos(X,Y1,P),
    Y1 >= 1, Y1 =< 9, Y2 >= 1, Y2 =< 9, dif(Y1, Y2),
    clear_path(pos(X,Y1,P), pos(X,Y2,P2)),
    chess(P, chariot, _),
    \+ pos(X,Y2,_).
move(pos(X1,Y,P), pos(X2,Y,P2)) :- 
    pos(X1,Y,P),
    X1 >= 1, X1 =< 10, X2 >= 1, X2 =< 10, dif(X1, X2),
    clear_path(pos(X1,Y,P), pos(X2,Y,P2)),
    chess(P, chariot, R),
    pos(X2,Y,P2),
    chess(P2, _, B),
    dif(R,B).
move(pos(X1,Y,P), pos(X2,Y,P2)) :- 
    pos(X1,Y,P),
    X1 >= 1, X1 =< 10, X2 >= 1, X2 =< 10, dif(X1, X2),
    clear_path(pos(X1,Y,P), pos(X2,Y,P2)),
    chess(P, chariot, _),
    \+ pos(X2,Y,_).

% horse move rules
% A -> B iff  C is empty, B is empty or on different side. 
% B -> A iff  E is empty, A is empty or on different side.
% D -> F iff  C is empty, F is empty or on different side.
% F -> D iff  E is empty, D is empty or on different side.
% C -> H
% A -> G
% ... 
%
% D - B
% |   |
% C - E - G
% |   |   |
% A - F - H
%

% A -> B
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 2,
    Y2 is Y1 + 1,
    X3 is X1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    \+ pos(X2, Y2, _),
    \+ pos(X3, Y1, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 2,
    Y2 is Y1 + 1,
    X3 is X1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    pos(X2,Y2,P2),
    \+ pos(X3, Y1, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% F -> D
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 2,
    Y2 is Y1 - 1,
    X3 is X1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    \+ pos(X2, Y2, _),
    \+ pos(X3, Y1, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 2,
    Y2 is Y1 - 1,
    X3 is X1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    pos(X2,Y2,P2),
    \+ pos(X3, Y1, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% B -> A
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 2,
    Y2 is Y1 + 1,
    X3 is X1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    \+ pos(X2, Y2, _),
    \+ pos(X3, Y1, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 2,
    Y2 is Y1 + 1,
    X3 is X1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    pos(X2,Y2,P2),
    \+ pos(X3, Y1, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% D -> F
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 2,
    Y2 is Y1 - 1,
    X3 is X1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    \+ pos(X2, Y2, _),
    \+ pos(X3, Y1, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 2,
    Y2 is Y1 - 1,
    X3 is X1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X3, Y1),
    pos(X2,Y2,P2),
    \+ pos(X3, Y1, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% A -> G
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 1,
    Y2 is Y1 + 2,
    Y3 is Y1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    \+ pos(X1, Y3, _),
    \+ pos(X2, Y2, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 1,
    Y2 is Y1 + 2,
    Y3 is Y1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    pos(X2,Y2,P2),
    \+ pos(X1, Y3, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% C -> H
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 1,
    Y2 is Y1 + 2,
    Y3 is Y1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    \+ pos(X1, Y3, _),
    \+ pos(X2, Y2, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 1,
    Y2 is Y1 + 2,
    Y3 is Y1 + 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    pos(X2,Y2,P2),
    \+ pos(X1, Y3, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% G -> A
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 1,
    Y2 is Y1 - 2,
    Y3 is Y1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    \+ pos(X1, Y3, _),
    \+ pos(X2, Y2, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 - 1,
    Y2 is Y1 - 2,
    Y3 is Y1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    pos(X2,Y2,P2),
    \+ pos(X1, Y3, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% H -> C
move(pos(X1,Y1,P), pos(X2,Y2,_)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 1,
    Y2 is Y1 - 2,
    Y3 is Y1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    \+ pos(X1, Y3, _),
    \+ pos(X2, Y2, _),
    chess(P, horse, _).
move(pos(X1,Y1,P), pos(X2,Y2,P2)) :- 
    pos(X1,Y1,P),
    X2 is X1 + 1,
    Y2 is Y1 - 2,
    Y3 is Y1 - 1,
    valid_pos(X1,Y1), valid_pos(X2,Y2), valid_pos(X1, Y3),
    pos(X2,Y2,P2),
    \+ pos(X1, Y3, _),
    chess(P, horse, R),
    chess(P2, _, B),
    dif(R, B).

% cannon move rules
move(pos(X,Y1,P), pos(X,Y2,_)) :- 
    valid_pos(X,Y1), valid_pos(X,Y2),
    pos(X,Y1,P),
    \+ pos(X,Y2,_),
    chess(P, cannon, _),
    clear_path(pos(X,Y1,P), pos(X,Y2,_)).
move(pos(X,Y1,P), pos(X,Y2,P2)) :- 
    Y1 < Y2,
    valid_pos(X,Y1), valid_pos(X,Y2),
    pos(X,Y1,P),
    pos(X,Y2,P2),
    chess(P, cannon, R),
    chess(P2,_, B),
    dif(R,B),
    pos(X, Y3, _),
    Y1 < Y3, Y3 < Y2,
    clear_path(pos(X,Y1,P), pos(X,Y3,_)),
    clear_path(pos(X,Y2,P), pos(X,Y3,_)).
move(pos(X,Y1,P), pos(X,Y2,P2)) :- 
    Y1 > Y2,
    valid_pos(X,Y1), valid_pos(X,Y2),
    pos(X,Y1,P),
    pos(X,Y2,P2),
    chess(P, cannon, R),
    chess(P2,_, B),
    dif(R,B),
    pos(X, Y3, _),
    Y1 > Y3, Y3 > Y2,
    clear_path(pos(X,Y1,P), pos(X,Y3,_)),
    clear_path(pos(X,Y2,P), pos(X,Y3,_)).

move(pos(X1,Y,P), pos(X2,Y,_)) :- 
    valid_pos(X1,Y), valid_pos(X2,Y),
    pos(X1,Y,P),
    \+ pos(X2,Y,_),
    chess(P, cannon, _),
    clear_path(pos(X1,Y,P), pos(X2,Y,_)).
move(pos(X1,Y,P), pos(X2,Y,P2)) :- 
    X1 < X2,
    valid_pos(X1,Y), valid_pos(X2,Y),
    pos(X1,Y,P),
    pos(X2,Y,P2),
    chess(P, cannon, R),
    chess(P2,_, B),
    dif(R,B),
    pos(X3, Y, _),
    X1 < X3, X3 < X2,
    clear_path(pos(X1,Y,P), pos(X3,Y,_)),
    clear_path(pos(X2,Y,P), pos(X3,Y,_)).
move(pos(X1,Y,P), pos(X2,Y,P2)) :- 
    X1 > X2,
    valid_pos(X1,Y), valid_pos(X2,Y),
    pos(X1,Y,P),
    pos(X2,Y,P2),
    chess(P, cannon, R),
    chess(P2,_, B),
    dif(R,B),
    pos(X3, Y, _),
    X1 > X3, X3 > X2,
    clear_path(pos(X1,Y,P), pos(X3,Y,_)),
    clear_path(pos(X2,Y,P), pos(X3,Y,_)).

% move rules for soldier
move(pos(X,Y1,P), pos(X, Y2, _)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, red),
    X >= 6,
    \+ pos(X, Y2, _),
    Y2 is Y1 + 1.
move(pos(X,Y1,P), pos(X, Y2, _)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, red),
    X >= 6,
    \+ pos(X, Y2, _),
    Y2 is Y1 - 1.
move(pos(X,Y1,P), pos(X, Y2, P2)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, red),
    X >= 6,
    pos(X, Y2, P2),
    chess(P2, _, black),
    Y2 is Y1 + 1.
move(pos(X,Y1,P), pos(X, Y2, P2)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, red),
    X >= 6,
    pos(X, Y2, P2),
    chess(P2, _, black),
    Y2 is Y1 - 1.

move(pos(X1,Y,P), pos(X2, Y, _)) :-
    valid_pos(X1,Y), valid_pos(X2, Y),
    pos(X1, Y, P),
    chess(P, soldier, red),
    \+ pos(X2, Y, _),
    X2 is X1 + 1.
move(pos(X1,Y,P), pos(X2, Y, P2)) :-
    valid_pos(X1,Y), valid_pos(X2, Y),
    pos(X1, Y, P),
    chess(P, soldier, red),
    pos(X2, Y, _),
    chess(P2, soldier, black),
    X2 is X1 + 1.

move(pos(X,Y1,P), pos(X, Y2, _)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, black),
    X =< 5,
    \+ pos(X, Y2, _),
    Y2 is Y1 + 1.
move(pos(X,Y1,P), pos(X, Y2, _)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, black),
    X =< 5,
    \+ pos(X, Y2, _),
    Y2 is Y1 - 1.
move(pos(X,Y1,P), pos(X, Y2, P2)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, black),
    X =< 5,
    pos(X, Y2, P2),
    chess(P2, _, red),
    Y2 is Y1 + 1.
move(pos(X,Y1,P), pos(X, Y2, P2)) :-
    valid_pos(X,Y1), valid_pos(X, Y2),
    pos(X, Y1, P),
    chess(P, soldier, black),
    X =< 5,
    pos(X, Y2, P2),
    chess(P2, _, red),
    Y2 is Y1 - 1.

move(pos(X1,Y,P), pos(X2, Y, _)) :-
    valid_pos(X1,Y), valid_pos(X2, Y),
    pos(X1, Y, P),
    chess(P, soldier, black),
    \+ pos(X2, Y, _),
    X2 is X1 - 1.
move(pos(X1,Y,P), pos(X2, Y, P2)) :-
    valid_pos(X1,Y), valid_pos(X2, Y),
    pos(X1, Y, P),
    chess(P, soldier, black),
    pos(X2, Y, _),
    chess(P2, soldier, red),
    X2 is X1 - 1.
