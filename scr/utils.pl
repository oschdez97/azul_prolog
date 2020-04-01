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


randint(X, Y) :- Y is random(X).

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



makeRepos(Y):- shuffle(W), make_Repos(5, W, Y), print(Y).
make_Repos(0, _, R):-print(R).
make_Repos(N, [I1, I2, I3, I4|Y], R):-
	M is N - 1,
	append(R, [[I1, I2, I3, I4]], W),
	make_Repos(M, Y, W).

test(X):-make_Repos(2, [1, 2, 3, 4, 5, 6, 7, 8], X).

init :- play([[blue: 0, yellow: 0, red:   0, black: 0, white: 0],
	     [white: 0, blue:   0, yellow:0, red:   0, black: 0],
             [black: 0, white:  0, blue:  0, yellow:0, red:   0],
             [red:   0, black:  0, white: 0, blue:  0, yellow:0],
             [yellow:0, red:    0, black: 0, white: 0, blue:  0]]
	     ,
	     [[empty],
             [empty, empty],
             [empty, empty, empty],
             [empty, empty, empty, empty],
	     [empty, empty, empty, empty, empty]]).

modify(_, [[1, 2], [3, 4]]).

play(Wall, _):-
	printMatrix(Wall, 5),
	modify(Wall, NewWall),
	write('//////////////////'),
	nl,
	printMatrix(NewWall, 2).

cls :- write('\e[H\e[2J').




