cls :- write('\e[H\e[2J').

% indexer([[1, 2], [3, 4]], 1, 2, 2)
% retorna el elemento en la posicion [I,J] de la matriz M
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

update_up(I,J, NewI, NewJ) :- NewI is I - 1, NewJ is J.
update_down(I,J,NewI, NewJ) :- NewI is I + 1, NewJ is J.
update_left(I,J,NewI,NewJ) :- NewI is I, NewJ is J - 1.
update_right(I,J,NewI, NewJ) :- NewI is I, New is J + 1.

sumUp(W, 1, J, _, Sum, Res) :- Res is Sum.
sumUp(W, _, _, 0, Sum, Res):- Res is Sum.
sumUp(W, I, J,NewValue, Sum, Res) :- update_up(I,J,NewI,NewJ),indexer(W, NewI, NewJ, Brick: Value ), sumUp(W, NewI,NewJ, Value, Sum + Value, Res).

sumDown(W, 5, J, _, Sum, Res) :- Res is Sum.
sumDown(W, _, _, 0, Sum, Res):- Res is Sum.
sumDown(W, I, J,NewValue, Sum, Res) :- update_down(I,J,NewI,NewJ),indexer(W, NewI, NewJ, Brick: Value ), sumDown(W, NewI,NewJ, Value, Sum + Value, Res).

sumLeft(W, I, 1, _, Sum, Res) :- Res is Sum.
sumLeft(W, _, _, 0, Sum, Res):- Res is Sum.
sumLeft(W, I, J,NewValue, Sum, Res) :- update_left(I,J,NewI,NewJ),indexer(W, NewI, NewJ, Brick: Value ), sumLeft(W, NewI,NewJ, Value, Sum + Value, Res).

sumRight(W, I, 5, _, Sum, Res) :- Res is Sum.
sumRight(W, _, _, 0, Sum, Res):- Res is Sum.
sumRight(W, I, J,NewValue, Sum, Res) :- update_right(I,J,NewI,NewJ),indexer(W, NewI, NewJ, Brick: Value ), sumRight(W, NewI,NewJ, Value, Sum + Value, Res).



sum(W, I, J, R) :- sumUp(W, I, J, 2, 0, ResUp),
	sumDown(W, I, J, 2, 0, ResD),
	sumLeft(W, I, J, 2, 0, ResL),
	sumRight(W, I, J, 2,0, ResR),
	add_all(ResUp,ResD,ResL,ResR, Sol),
	R is Sol.

add_all(0, 0, 0, 0, Sol) :- Sol is 1.
add_all(Up, Down, 0, 0, Sol) :- Sol is Up + Down + 1.
add_all(0,0, Left,Right, Sol) :- Sol is Left + Right + 1.
add_all(Up,Down,Left,Right) :- Sol is Left+Right + Up + Down + 2.   
			
			



% imprime la matriz cuadrada M de tama�o N
printMatrix(M, N) :-
	MAX is N * N,
	pMatrix(M, 1, 1, MAX, N).


% take(3, [1, 2, 3, 4], [1, 2, 3])
% toma los N primeros elementos de la coleccion L
take(0, _, []).
take(N, [X|Y], [X|W]):- M is N-1, take(M, Y, W).

% replace(4, a, [1, 2, 3, 4], [1, 2, 3, a])
% reemplaza el elemento E por el elemento S
replace(_, _, [], []).
replace(E, S, [E|T1], [S|T2]):- replace(E, S, T1, T2).
replace(E, S, [H|T1], [H|T2]):- E\=H, replace(E, S, T1, T2).


% setAtIndex(2, 10, [a, b, c, d], [a, 10, c, d])
% reemplaza el elemento en el indice I por el elemento E
setAtIndex(_, _, [], []).
setAtIndex(1, E, [_|Y], [E|Y]).
setAtIndex(I, E, [X|Y], [X|R]) :- K is I-1, setAtIndex(K, E, Y, R).

% makeRepos(2, 3, [1, 2, 3, 4, 5, 6], [[1, 2, 3], [4, 5, 6]])
% crea N repositorios de tama�o M de losas de la coleccion S
makeRepos(0, _, _, []).
makeRepos(N, M, S, [X|Y]):-
	K is N - 1,
	take(M, S, X),
	makeRepos(K, M, S, Y).

% inicia una partida con una cantidad N de jugadores
init(N):- (N =:= 2 -> init2) ; (N =:= 3 -> init3) ; init4.

init2 :- wall(W1),
	wall(W2),
	stair(S1),
	stair(S2),
	shuffle(Bag),
	play([[W1, S1, 0], [W2, S2, 0]], Bag, 5).

init3 :- wall(W1),
	wall(W2),
	wall(W3),
	stair(S1),
	stair(S2),
	stair(S3),
	shuffle(Bag),
	play([[W1, S1, 0], [W2, S2, 0], [W3, S3, 0]], Bag, 7).

init4 :- wall(W1),
	 wall(W2),
	 wall(W3),
         wall(W4),
	 stair(S1),
	 stair(S2),
	 stair(S3),
	 stair(S4),
	 shuffle(Bag),
	 play([[W1, S1, 0], [W2, S2, 0], [W3, S3, 0], [W4, S4, 0]], Bag, 9).


% Nota tengo que hacer que makeRepos me devuelva
% ademas de los repos que forma en un instante de tiempo
% que me de tb lo que resta de la bolsa para la siguiente iteracion


% aqui va la logica del juego
play(Players, Bag, NR):-
	makeRepos(NR, 4, Bag, Repos),
	print(Repos),
	nl,nl,
	print(Bag).
        % lo que sigue aqui es
	% hacer una jugada por cada jugador
	% actualizar todas las cosas y llamar recursivo





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


wall1(X):- X = [[blue: 0, yellow: 0, red:   1, black: 0, white: 0],
	      [white: 0, blue:   0, yellow:1, red:   0, black: 0],
	      [black: 0, white:  0, blue:  0, yellow:0, red:   0],
	      [red:   0, black:  0, white: 0, blue:  0, yellow:0],
	      [yellow:0, red:    0, black: 0, white: 0, blue:  0]].


% randomiza las 100 piezas del juego
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













































































