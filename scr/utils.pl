cls :- write('\e[H\e[2J').

indexer(M, I, J, R):-
	nth1(I, M, T),
	nth1(J, T, R).

pMatrix(_, _, _, 0, _).
pMatrix(M, I, J, B, C):-
        indexer(M, I, J, Elem),
	write(Elem),
	write(' '),
	T is J mod C,
        breakLine(T),
	update(T, I, J, NewI, NewJ),
	MAX  is B - 1,
	pMatrix(M, NewI, NewJ, MAX, C).


breakLine(0):- nl.
breakLine(_).

update(0, I, _, NewI, NewJ):-
	NewI is I + 1,
	NewJ is 1.
update(_, I, J, NewI, NewJ):-
	NewI is I,
	NewJ is J + 1.

printMatrix(M, N) :-
	MAX is N * N,
	pMatrix(M, 1, 1, MAX, N).


take(N, _, Xs):-
	N =< 0, !,
	N =:=0,
	Xs = [].

take(_, [], []).

take(N, [X|Xs], [X|Ys]):-
	M is N - 1,
	take(M, Xs, Ys).


makeRepos(0, _, _, []).
makeRepos(N, M, S, [X|Y]):-
	K is N - 1,
	take(M, S, X),
	makeRepos(K, M, S, Y).


init :- wall(W), stair(S), shuffle(Bag), play(W, S, Bag).

% Este es el hombre, se llama con
% [Wall, Stair, Bag] que representa cada jugador
% Se llama a MakeRepos y se actualizan los tableros
% de cada jugador en funcion de la estrategia que siga cada uno
% Se llama recursivo a play con los nuevos [Wall, Stair, Bag] y otros
% argvs necesarios...

play(Wall, Stair, Bag):-
	printMatrix(Wall, 5),
	nl,
	print(Stair),
	nl,
	print(Bag).



stair(X):- X = [[empty],
	       [empty, empty],
	       [empty, empty, empty],
	       [empty, empty, empty, empty],
	       [empty, empty, empty, empty, empty]].


wall(X):- X = [[blue: 0, yellow: 0, red:   0, black: 0, white: 0],
	      [white: 0, blue:   0, yellow:0, red:   0, black: 0],
	      [black: 0, white:  0, blue:  0, yellow:0, red:   0],
	      [red:   0, black:  0, white: 0, blue:  0, yellow:0],
	      [yellow:0, red:    0, black: 0, white: 0, blue:  0]].



shuffle(R) :- random_permutation([1, 1, 1, 1, 1, 1,
				  1, 1, 1, 1, 1, 1,
				  1, 1, 1, 1, 1, 1,
				  1, 1, 1, 1, 1, 1,
				  2, 2, 2, 2, 2, 2,
				  2, 2, 2, 2, 2, 2,
				  2, 2, 2, 2, 2, 2,
				  2, 2, 2, 2, 2, 2,
				  3, 3, 3, 3, 3, 3,
				  3, 3, 3, 3, 3, 3,
				  3, 3, 3, 3, 3, 3,
				  3, 3, 3, 3, 3, 3,
				  4, 4, 4, 4, 4, 4,
				  4, 4, 4, 4, 4, 4,
				  4, 4, 4, 4, 4, 4,
				  4, 4, 4, 4, 4, 4,
				  5, 5, 5, 5, 5, 5,
				  5, 5, 5, 5, 5, 5,
				  5, 5, 5, 5, 5, 5,
				  5, 5, 5, 5, 5, 5], R).
