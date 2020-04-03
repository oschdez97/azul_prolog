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

% imprime la matriz cuadrada M de tamaño N
printMatrix(M, N) :-
	MAX is N * N,
	pMatrix(M, 1, 1, MAX, N).


% imprime la escalera
printStair([]).
printStair([X|Y]):- print(X), nl, printStair(Y).


% take(3, [1, 2, 3, 4], [1, 2, 3])
% toma los N primeros elementos de la coleccion L
take(0, _, []).
take(_, [], []).
take(N, [X|Y], [X|W]):-
	M is N-1,
	take(M, Y, W).

% replace(4, a, [1, 2, 3, 4], [1, 2, 3, a])
% reemplaza el elemento E por el elemento S
replace(_, _, [], []).
replace(E, S, [E|T1], [S|T2]):- replace(E, S, T1, T2).
replace(E, S, [H|T1], [H|T2]):- E\=H, replace(E, S, T1, T2).

% moveTo(2, [a, b, c, d], [c, d])
% retorna la lista [I - len] comenzando en 0
moveTo(0, L, L).
moveTo(I, L, R):- length(L, K), I > K, R = [].
moveTo(I, [_|Y], R):-
	N is I-1,
	moveTo(N, Y ,R).


% setAtIndex(2, 10, [a, b, c, d], [a, 10, c, d])
% reemplaza el elemento en el indice I por el elemento E
setAtIndex(_, _, [], []).
setAtIndex(1, E, [_|Y], [E|Y]).
setAtIndex(I, E, [X|Y], [X|R]) :- K is I-1, setAtIndex(K, E, Y, R).

% makeRepos(2, 3, [1, 2, 3, 4, 5, 6], [[1, 2, 3], [4, 5, 6]])
% crea N repositorios de tamaño M de losas de la coleccion S
makeRepos(0, _, _, _, []).
makeRepos(N, T, M, S, [X|Y]):-
        H is (T - N) * M,
	moveTo(H, S, S1),
	K is N - 1,
	take(M, S1, X),
	makeRepos(K, T, M, S, Y).

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

% retorna el reverso de la lista L a partir del indice I
reverseAt([], _, []).
reverseAt(L, 0, R):- reverse(L, R).
reverseAt([_|Y], I, R):-
	K is I-1,
	reverseAt(Y, K, R).

% true si el length de X es 0
len0(X):- length(X, K), 0 is K.

% true si los repos estan vacios
emptyRepos(L):-
	findall(X, (member(X, L), len0(X)), R1),
	length(L, K),
	length(R1,K1),
	S is K - K1,
	0 is S.

% retorna cuantos espacios en blanco hay en una lista
emptySpaces([], 0).
emptySpaces([empty], 1).
emptySpaces([empty|Y], R):- emptySpaces(Y, K), R is K + 1.
emptySpaces([_|Y], R):- emptySpaces(Y, R).

% cuenta la cantidad de losas de color T en L
countColors(L, T, R):- findall(X, (member(X, L), X is T), K), length(K, R).

% retorna el maximo de una lista
max([], M, M).
max([X|Y], M, R):- (X > M -> K = X ; K = M), max(Y, K, R).

% retorna el indice de X en la lista L empezando por 0
indexOf(_, [], -1).
indexOf(X, [X|_], 0).
indexOf(X, [_|Z], R):- indexOf(X, Z, K), R is 1 + K.

% retorna una lista del tamaño de la cantidad de repos
% que tiene en la pos i la cantidad del color que mas se repite
countRepos([], []).
countRepos([X|Y], [M|Z]):-
	countColors(X, 1, R1),
	countColors(X, 2, R2),
	countColors(X, 3, R3),
	countColors(X, 4, R4),
	countColors(X, 5, R5),
	max([R1, R2, R3, R4, R5], -1, M),
	countRepos(Y, Z).

% retorna el indice del repo que mas fichas tiene del mismo color
% <Greedy>
fatRepo(R):-
	countRepos([[1, 1, 3, 4], [2, 1, 5, 5], [3, 3, 3, 1], [1, 2, 3, 4]], T),
	max(T, -1, M),
	indexOf(M, T, R).


% me hace falta hacer que mientras no esten vacios los repos,
% el jugador i haga una movida, o sea que escoja un repo y saque
% algunas casillas y el resto vayan al repo del medio


% seguimos con el greedy lo que tengo que hacer es cuando escoja de un
% repo voy a la posicion esa de la escalera y trato de ponerlos ahi si
% no puedo entones tengo que ir visitando la escalera en el orden
% inverso (tengo que ir pa abajo y despues revertir los niveles
% superiores)

% retorna true si puedo colocar
% los elementos R en la posicion I de la escalera
% para ello tengo que comprobar varias cosas
% 1. que halla espacio
% 2. que no hayan elementos de otro color en esa posicion
% 3. que en el muro y ano esten ocupadas esas casillas ?
canPutAt(R, I, S).



test(I) :- stair(L), reverseAt(L, I, R), printStair(R).

% aqui va la logica del juego
play(Players, Bag, NR):-
	K is NR * 4 * 2,
	moveTo(K, Bag, RBag),
	makeRepos(NR, NR, 4, RBag, Repos),
	makeMove(Players, Repos).
	%print(Repos),
	%nl,nl,
	%print(Bag).
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













cc([], K, K).
cc([empty|_], K, K).
cc([_|Y], K, R):-
	M is K + 1,
	cc(Y, M, R).




























































