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


printList([]).
printList([X|Y]):- print(X),nl,printList(Y).

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
% empezando por 1
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
newGame(N):- (N =:= 2 -> init2) ; (N =:= 3 -> init3) ; init4.

init2 :- wall(W1),
	 wall(W2),
	 stair(S1),
	 stair(S2),
	 shuffle(Bag),
	 play([[W1, S1, [], 0], [W2, S2, [], 0]], Bag, 5, [], 0).

init3 :- wall(W1),
	 wall(W2),
	 wall(W3),
	 stair(S1),
	 stair(S2),
	 stair(S3),
	 shuffle(Bag),
	 play([[W1, S1, [], 0], [W2, S2, [], 0], [W3, S3, [], 0]], Bag, 7, [], 0).

init4 :- wall(W1),
	 wall(W2),
	 wall(W3),
         wall(W4),
	 stair(S1),
	 stair(S2),
	 stair(S3),
	 stair(S4),
	 shuffle(Bag),
	 play([[W1, S1, [], 0], [W2, S2, [], 0], [W3, S3, [], 0], [W4, S4, [], 0]], Bag, 9, [], 0).

% retorna el reverso de la lista L a partir del indice I
reverseAt([], _, []).
reverseAt(L, 0, R):- reverse(L, R).
reverseAt([_|Y], I, R):-
	K is I-1,
	reverseAt(Y, K, R).


% true si el length de X es 0
len0(X):- length(X, K), 0 is K.

% true si los repos estan vacios
emptyRepos(L, R):-
	findall(X, (member(X, L), len0(X)), R1),
	length(L, K),
	length(R1,K1),
	S is K - K1,
	( S =:= 0 -> R is 1 ; R is 0 ).

% retorna cuantos espacios en blanco hay en una lista
emptySpaces([], 0).
emptySpaces([empty], 1).
emptySpaces([empty|Y], R):- emptySpaces(Y, K), R is K + 1.
emptySpaces([_|Y], R):- emptySpaces(Y, R).

% cuenta la cantidad de losas de color T en L
countColors(L, T, R):- findall(X, (member(X, L), X is T), K), length(K, R).


countRepo([], _, []).
countRepo([X|Y], T, [R1|Z]):-
	countColors(X, T, R1),
	countRepos1(Y, T, Z).


% retorna el maximo de una lista
max([], M, M).
max([X|Y], M, R):- (X > M -> K = X ; K = M), max(Y, K, R).

% retorna el indice de X en la lista L empezando por 1
indexOf(_, [], -1).
indexOf(X, [X|_], 1).
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

% retorna el color que mas
% se repite en un repo determinado
whichColor(X, R):-
	countColors(X, 1, R1),
	countColors(X, 2, R2),
	countColors(X, 3, R3),
	countColors(X, 4, R4),
	countColors(X, 5, R5),
	T = [R1, R2, R3, R4, R5],
	max(T, -1, M),
	indexOf(M, T, R).


% retorna una lista con los colores
% que sobran en el repo que se selecciono
% o sea los que van a ir al repo del medio
forMidRepo([], _, []).
forMidRepo([M|Y], M, R):- forMidRepo(Y, M, R).
forMidRepo([X|Y], M, [X|R]):- forMidRepo(Y, M, R).

% retorna la escalera y el piso del jugador i
% luego de hacer la actualizaciones pertinentes
% para ubicar las fichas correspondientes



% dado un jugador y los repos retorna al jugador
% actualizado luego de haber escogido un repo y
% acomodar las fichas en si escalera
fetchRepo(Player, Repos, X):-
	fatRepo(Repos, I),
	nth1(I, Repos, R),
	whichColor(R, C),
	forMidRepo(R, C, FMid),
	countColors(R, C, T),
	print(T).


%retorna el indice del repo que mas fichas tiene del mismo color
% <Greedy>
fatRepo(L, R):-
	countRepos(L, T),
	max(T, -1, M),
	indexOf(M, T, R).


% me hace falta hacer que mientras no esten vacios los repos,
% el jugador i haga una movida, o sea que escoja un repo y saque
% algunas casillas y el resto vayan al repo del medio


% retorna true si puedo colocar
% los elementos R en la posicion I de la escalera
% para ello tengo que comprobar varias cosas
% 1. que halla espacio
% 2. que no hayan elementos de otro color en esa posicion
% 3. que en el muro y ano esten ocupadas esas casillas ?


countChips([], 0).
countChips([X|Y], R):-
	length(X, K),
	countChips(Y, R1),
	R is R1 + K.

rotateList([], []).
rotateList([X|Y], Z):- append(Y, [X], Z).


pushChips(_, 0, F, F).
pushChips(_, _, [], []).
pushChips(C, CC, [empty|Y], [C|R]):-
	NCC is CC-1,
	pushChips(C, NCC, Y, R).
pushChips(C, CC, [X|Y], R):- pushChips(C, CC, Y, T), append([X], T, R).

% dado una seleccion valida de losas retorna la escalara actualizada
updateStair1([F1, F2, F3, F4, F5], [_, C, CC, SI], NStair):-
	( SI =:= 1 -> pushChips(C, CC, F1, NF1) ; NF1 = F1 ),
        ( SI =:= 2 -> pushChips(C, CC, F2, NF2) ; NF2 = F2 ),
	( SI =:= 3 -> pushChips(C, CC, F3, NF3) ; NF3 = F3 ),
	( SI =:= 4 -> pushChips(C, CC, F4, NF4) ; NF4 = F4 ),
	( SI =:= 5 -> pushChips(C, CC, F5, NF5) ; NF5 = F5 ),
	NStair = [NF1, NF2, NF3, NF4, NF5].


getNItems(_, 0, []).
getNItems([], _, []).
getNItems([X|Y], N, [X|R]):- M is N-1, getNItems(Y, M, R).

updateFloor(F, CFC, R):-
	length(F, FL),
	K is 7-FL,
	getNItems(CFC, K, RCFC),
	append(F, RCFC, R).


% retorna 1 si contine la ficha
% del primer jugador, 0 en otro caso
first([], 0).
first([s|_], 1).
first([_|Y], R):- first(Y, R).

% retorna una lista sin la ficha del
% primer jugador que escogio del suelo
removeSpecialChip([], []).
removeSpecialChip([s|Y], R):- removeSpecialChip(Y, R).
removeSpecialChip([X|Y], [X|R]):- removeSpecialChip(Y, R).

makeNegativeMove([], _, []).
makeNegativeMove([[]|Y], I, R):- K is I+1, makeNegativeMove(Y, K, R).
makeNegativeMove([[_|_]|_],I, I).

getAnyColor([], -1).
getAnyColor([s|Y], R):- getAnyColor(Y, R).
getAnyColor([C|_], C).

forFloorChips([], _, []).
forFloorChips([C|R1], C, [C|R]):- forFloorChips(R1, C, R).
forFloorChips([_|R1], C, R):- forFloorChips(R1, C, R).


makeARound(Players, _, _, 1, Players).
makeARound([[Wall, Stair, Floor, Puntuation]|Y], Repos, MR, 0, R):-
	  append(MR, Repos, AllRepos),

	  fm1(Stair, AllRepos, 1, RFM1),
	  fm(RFM1, Stair, Wall, FM),

	  length(FM, L),

	  %print(L),nl,nl,

	  ( L =:= 0 ->

	    makeNegativeMove(AllRepos, 1, RI),
	    nth1(RI, AllRepos, NMT),
	    getAnyColor(NMT, C),
	    forFloorChips(NMT, C, RFC),
	    updateFloor(Floor, RFC, NFloor1),
	    %print('NFM'),
	    NStair = Stair
	    %read(_)


	  ;

	    %RDM is random(L),
	    %nth0(RDM, FM, [RI, C, CC, SI]),
            %print([RI,C, CC, SI]),
	    random_permutation(FM, RFM),
	    member([RI, C, CC, SI], RFM),
	    updateStair1(Stair, [RI, C, CC, SI], NStair),
	    NFloor1 = []

	  ),

	  %print('A'),nl,
	  %read(_),

	  %printList(FM),nl,

	  %print(AllRepos),nl,nl,

	  %print([RI, C, CC, SI]),nl,nl,

	  %printStair(NStair),nl,nl,

	  ( RI =:= 1 ->

	   % analizar si es el
	   % primero que coge del medio

	    member(X, MR),
	    first(X, FR),
	    ( FR =:= 1 -> removeSpecialChip(X, RRSC), updateFloor(Floor, [s], NFloor2) ; RRSC = X, NFloor2 = [] ),

	    NAllRepos = Repos,

	    %print(RRSC),nl,nl,

	    forMidRepo(RRSC, C, NMR)

	    ;

	    RIT is RI-1,
	    nth1(RIT, Repos, TR),
	    forMidRepo(TR, C, FMR),
	    member(E, MR),
	    append(E, FMR, NMR),
	    setAtIndex(RIT, [], Repos, NAllRepos)
	  ),

	  %print(AllRepos),nl,nl,

	  %print(TR),nl,nl,
	  %print(FMR),nl,nl,
	  %print(NMR),nl,nl,
	  %print(NAllRepos),nl,nl,

	  union(NFloor1, NFloor2, NFloor3),
	  union(NFloor3, Floor, NFloor),
	  %print(NFloor),nl,nl,

	  append(Y, [[Wall, NStair, NFloor, Puntuation]], NPlayers),

	  %printList(NPlayers),


	  %print('GOING AGAIN'),nl,
	  %read('B'),

	  append(NAllRepos, [NMR], FALLR),
	  emptyRepos(FALLR, S),
	  %print(S),nl,nl,

	  makeARound(NPlayers, NAllRepos, [NMR], S, R)


	  % tengo que hacer el movimiento descrito en M
	  % esto es actualizar la escalera del jugador
	  % la escalera se llena de izquierda a derecha
	  % actualizar el repo del medio
	  % NOTA: tener en cuenta que los FM son aquellos
	  % que son totalmente factibles o sea que hay espacio
	  % para hacerlo y no viola ninguna regla del juego
	  % puede que no existan estos movimientos en un momento
	  % determinado, en cuyo caso habria que escoger cualquiera
	  % de los posibles y restarse puntos mandando fichas al suelo

	.

join([], []).
join([X|Y], R) :- join(Y, R1), append(X, R1, R).

remainChips(L, R):- findall(X, (member(X, L), len(X)), R1), join(R1, R2),
	removeSpecialChip(R2, R).


endGame2(F):-
	nth1(1, F, _:E1),
	nth1(2, F, _:E2),
	nth1(3, F, _:E3),
	nth1(4, F, _:E4),
	nth1(5, F, _:E5),
	T = [E1, E2, E3, E4, E5],
	forall(member(X, T), X =:= 1).

endGame1([F1, F2, F3, F4, F5]):-
	endGame2(F1) ;
	endGame2(F2) ;
	endGame2(F3) ;
	endGame2(F4) ;
	endGame2(F5).

endGame([]):- 1 is 0.
endGame([[W, _, _, _]|Y]):- ( endGame1(W) -> 1 is 1 ; endGame(Y) ).


all(L, R):-
	nth1(1, L, _:E1),
	nth1(2, L, _:E2),
	nth1(3, L, _:E3),
	nth1(4, L, _:E4),
	nth1(5, L, _:E5),
	T = [E1, E2, E3, E4, E5],
	( forall(member(X, T), X =:= 1) -> R is 1 ; R is 0 ).


completeColor([R1, R2, R3, R4, R5], C, R):-
	indexOf(C:_, R1, E1),
	indexOf(C:_, R2, E2),
	indexOf(C:_, R3, E3),
	indexOf(C:_, R4, E4),
	indexOf(C:_, R5, E5),

	nth1(E1, R1, _:X1),
	nth1(E2, R2, _:X2),
	nth1(E3, R3, _:X3),
	nth1(E4, R4, _:X4),
	nth1(E5, R5, _:X5),

	T = [X1, X2, X3, X4, X5],
	( forall(member(X, T), X =:= 1) -> R is 1 ; R is 0 ).


finalCount([], []).
finalCount([[W, _, _, P]|Y], [NP|R]):-
	nth1(1, W, R1),
	nth1(2, W, R2),
	nth1(3, W, R3),
	nth1(4, W, R4),
	nth1(5, W, R5),

	getColumn(W, 1, C1),
	getColumn(W, 2, C2),
	getColumn(W, 3, C3),
	getColumn(W, 4, C4),
	getColumn(W, 5, C5),

	all(R1, RR1),
	all(R2, RR2),
	all(R3, RR3),
	all(R4, RR4),
	all(R5, RR5),

	all(C1, RC1),
	all(C2, RC2),
	all(C3, RC3),
	all(C4, RC4),
	all(C5, RC5),

	completeColor(W, 1, CC1),
	completeColor(W, 2, CC2),
	completeColor(W, 3, CC3),
	completeColor(W, 4, CC4),
	completeColor(W, 5, CC5),

	NCR is RR1+RR2+RR3+RR4+RR5,
	NCC is RC1+RC2+RC3+RC4+RC5,
	CCN is CC1+CC2+CC3+CC4+CC5,
	NP  is 2*NCR+7*NCC+10*CCN+P,

	finalCount(Y, R).


getWinner(Players):-
	finalCount(Players, FPuntuations),
	max(FPuntuations, -1, M),
	indexOf(M, FPuntuations, W),
	nl,nl,
	print('----------- GAME OVER -----------'),nl,nl,
	print('FINAL PUNTUATIONS: '),
	print(FPuntuations),nl,nl,
	print('PLAYER '),
	print(W),
	print(' '),
	print('WON!!!'),nl,nl
	.

printStatus([], _, []).
printStatus([[W, S, F, P]|Y], I, R):-
	print('PLAYER '), print(I),nl,
	printStair(S),nl,nl,
	printMatrix(W, 5),nl,nl,
	print(F),nl,nl,
	print(P),nl,nl,
	K is I+1,
	printStatus(Y, K, R).


equalColor(L):- member(X, L), forall(member(Y, L), X =:= Y).


% Players: Lista de jugadores
% Bag    : La bolsa de fichas
% NR	 : Numero de repos
% RC     : fichas sobrantes para cuando se acabe la bolsa
% 1	 : fin del juego
play(Players, _, _, _, 1).
play(Players, [], _, [], _):- getWinner(Players).
play(Players, [], NR, RC, 0):- play(Players, RC, NR, [], 0).
play(Players, Bag, NR, RC, 0):-
	makeRepos(NR, NR, 4, Bag, Repos),
	countChips(Repos, K),
	moveTo(K, Bag, RBag),

	length(Players, L),
	( L =:= 2 ->

	 makeARound(Players, Repos, [[s]], 0,
		    [[W1, S1, F1, Pun1],
		     [W2, S2, F2, Pun2]]),

	 countPoints([W1, S1, F1, Pun1], [P1, RCP1]),
 	 countPoints([W2, S2, F2, Pun2], [P2, RCP2]),

	 remainChips(RCP1, RC1),
	 remainChips(RCP2, RC2),
	 append(RC1, RC2, RC3),
	 append(RC, RC3, NRC1),

	 length(RBag, RBL),
	 length(NRC1, NRCL),
	 ( RBL =:= 0, NRCL =< 4, equalColor(NRC1) -> NRC = [] ; NRC = NRC1 ),


	 NPlayers = [P1, P2],
	 printStatus(NPlayers, 1, _)

	 ;

	 ( L =:= 3 ->

	 makeARound(Players, Repos, [[s]], 0,
		    [[W1, S1, F1, Pun1],
		     [W2, S2, F2, Pun2],
		     [W3, S3, F3, Pun3]]),

	 countPoints([W1, S1, F1, Pun1], [P1, RCP1]),
 	 countPoints([W2, S2, F2, Pun2], [P2, RCP2]),
	 countPoints([W3, S3, F3, Pun3], [P3, RCP3]),

	 remainChips(RCP1, RC1),
	 remainChips(RCP2, RC2),
	 remainChips(RCP3, RC3),

	 append(RC1, RC2, T1),
	 append(T1, RC3, T2),
	 append(RC, T2, NRC1),

	 length(RBag, RBL),
	 length(NRC1, NRCL),
	 ( RBL =:= 0, NRCL =< 4, equalColor(NRC1) -> NRC = [] ; NRC = NRC1 ),


	 NPlayers = [P1, P2, P3],
	 printStatus(NPlayers, 1, _)

	 ;

	 makeARound(Players, Repos, [[s]], 0,
		    [[W1, S1, F1, Pun1],
		     [W2, S2, F2, Pun2],
		     [W3, S3, F3, Pun3],
		     [W4, S4, F4, Pun4]]),

	 countPoints([W1, S1, F1, Pun1], [P1, RCP1]),
 	 countPoints([W2, S2, F2, Pun2], [P2, RCP2]),
	 countPoints([W3, S3, F3, Pun3], [P3, RCP3]),
	 countPoints([W4, S4, F4, Pun4], [P4, RCP4]),

	 remainChips(RCP1, RC1),
	 remainChips(RCP2, RC2),
	 remainChips(RCP3, RC3),
	 remainChips(RCP4, RC4),

	 append(RC1, RC2, T1),
	 append(T1, RC3, T2),
	 append(T2, RC4, T3),
	 append(RC, T3, NRC1),

	 length(RBag, RBL),
	 length(NRC1, NRCL),
	 ( RBL =:= 0, NRCL =< 4, equalColor(NRC1) -> NRC = [] ; NRC = NRC1 ),

	 NPlayers = [P1, P2, P3, P4],
	 printStatus(NPlayers, 1, _)

	 )

	),


	( endGame(NPlayers) -> getWinner(NPlayers), NW is 1 ; NW is 0),

	play(NPlayers, RBag, NR, NRC, NW)

	.

stair(X):- X = [[empty],
	       [empty, empty],
	       [empty, empty, empty],
	       [empty, empty, empty, empty],
	       [empty, empty, empty, empty, empty]].


wall(X):- X = [[1:0, 2:0, 3:0, 4:0, 5:0],
	       [5:0, 1:0, 2:0, 3:0, 4:0],
	       [4:0, 5:0, 1:0, 2:0, 3:0],
	       [3:0, 4:0, 5:0, 1:0, 2:0],
	       [2:0, 3:0, 4:0, 5:0, 1:0]].


soil(X):- X = [0, -1, -2, -4, -6, -8, -11, -14].

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




% Lo que voy a hacer es lo siguiente:
% 1. Voy por cada repo y por cada color en ese repo miro si puedo
% colocarlos en la escalera porque hay espacio para ellos esto deberia
% retornar una lista que tenga de que repo se trata, que colores fueron
% los que se cogieron y cuales quedaron en el repo del medio.
% 2. Luego que tenga esa lista tengo que recorrerla nuevamente y
% filtrarla pero ahora viendo que realmente se puedan colocar, esto
% implica chequear que en la escalera no hayan otros colores y que en el
% muro ese color este libre.


len(X):- length(X, L), L > 0.

fm1_aux(I, C, CNT, K1, K2, K3, K4, K5, R):-
	( CNT > 0 ->
		( K1 >= CNT -> L1 = [I, C, CNT, 1] ; L1 = [] ),
		( K2 >= CNT -> L2 = [I, C, CNT, 2] ; L2 = [] ),
	        ( K3 >= CNT -> L3 = [I, C, CNT, 3] ; L3 = [] ),
	        ( K4 >= CNT -> L4 = [I, C, CNT, 4] ; L4 = [] ),
	        ( K5 >= CNT -> L5 = [I, C, CNT, 5] ; L5 = [] ),
		findall(X, (member(X, [L1, L2, L3, L4, L5]), len(X)), R)

	        ;

		R = []
	).

% retorna una lista que tiene los movimientos factibles a primer
% nivel, las jugadas que se pueden hacer por cada repo sabiendo
% solamente si hay espacio para colocar la fichas seleccionadas
% en la escalera del jugador i (no se verifican mas condiciones)
% parametros: (escalera, repos, contador de los repos)
% retorna:
% [indice del repo, color, cantidad del color, fila de la escalera]
fm1(_, [], _, []).
fm1([F1, F2, F3, F4, F5],[X|Y], I, R):-
	countColors(X, 1, C1),
	countColors(X, 2, C2),
	countColors(X, 3, C3),
	countColors(X, 4, C4),
	countColors(X, 5, C5),

	emptySpaces(F1, K1),
	emptySpaces(F2, K2),
	emptySpaces(F3, K3),
	emptySpaces(F4, K4),
	emptySpaces(F5, K5),

        fm1_aux(I, 1, C1, K1, K2, K3, K4, K5, L1),
	fm1_aux(I, 2, C2, K1, K2, K3, K4, K5, L2),
	fm1_aux(I, 3, C3, K1, K2, K3, K4, K5, L3),
	fm1_aux(I, 4, C4, K1, K2, K3, K4, K5, L4),
	fm1_aux(I, 5, C5, K1, K2, K3, K4, K5, L5),

	%print(L1),nl,
	%print(L2),nl,
	%print(L3),nl,
	%print(L4),nl,
	%print(L5),nl,nl,nl,

	NEXT is I+1,
	fm1([F1, F2, F3, F4, F5], Y, NEXT, R1),
	append([L1, L2, L3, L4, L5, R1], R).


% retorna true si en una fila de la escalera
% solo hay espacios ocupados por fichas del color C
sameColor([], _).
sameColor([C|Y], C):- sameColor(Y, C).
sameColor([empty|Y], C):- sameColor(Y, C).


% retorna dada una lista L del muro si el color C esta libre
notOccupied([C:0|_], C).
notOccupied([_|Y], C):- notOccupied(Y, C).


% retorna una lista con todos los posibles movimientos validos
fm([], _, _, []).
fm([[RI, C, CC, SI]|Y], Stair, Wall, R):-
	nth1(SI, Stair, SR),
	( sameColor(SR, C) ->
	          nth1(SI, Wall, WR),
		 ( notOccupied(WR, C) -> L = [[RI, C, CC, SI]] ; L = [] ) ; L = [] ),
	fm(Y, Stair, Wall, R1),
	append(L, R1, R).


test1(R):- stair1(S), fm1(S, [[2, 2, 3, 4], [1, 2, 3, 4], [3, 5, 5, 5]], 1, R), printList(R).

test2:- test1(M), stair1(S), wall1(W), fm(M, S, W, R), printList(R).

test3:- wall1(W), nth1(2, W, R), print(R), notOccupied(R, 1).

stair1(X):- X = [[empty],
	        [empty, 1],
	        [empty, empty, empty],
	        [1, empty, empty, empty],
	        [empty, empty, 1, empty, 1]].


wall1(X):- X = [[1:0, 2:1, 3:0, 4:1, 5:0],
	        [5:0, 1:1, 2:0, 3:0, 4:0],
	        [4:0, 5:0, 1:1, 2:0, 3:0],
	        [3:1, 4:0, 5:1, 1:0, 2:0],
	        [2:0, 3:1, 4:1, 5:0, 1:0]].


stair2(X):- X = [[empty],
	        [empty, empty],
	        [3, empty, empty],
	        [2, 2, 2, 2],
	        [3, 3, 3, 3, empty]].


wall2(X):- X = [[1:0, 2:1, 3:1, 4:0, 5:1],
	        [5:0, 1:0, 2:1, 3:1, 4:1],
	        [4:1, 5:1, 1:1, 2:0, 3:0],
	        [3:0, 4:0, 5:1, 1:0, 2:0],
	        [2:0, 3:0, 4:0, 5:0, 1:0]].


notEmpty(1).
notEmpty(2).
notEmpty(3).
notEmpty(4).
notEmpty(5).
completeRow(L):- findall(X, (member(X, L), notEmpty(X)), T), length(L, K1), length(T, K2),
	K3 is K1-K2, 0 is K3.

% retorna una lista con el color correspondiente
% si la columna de la escalera esta completa, en
% otro caso en esa posicion retorna -1
getStairColumn([], []).
getStairColumn([X|Y], [T|R]):-
	( completeRow(X) -> member(T, X) ; T is -1 ),
	getStairColumn(Y, R).


getColumn(Wall, J, C):-
	nth1(1, Wall, F1),
	nth1(2, Wall, F2),
	nth1(3, Wall, F3),
	nth1(4, Wall, F4),
	nth1(5, Wall, F5),
	nth1(J, F1, E1),
	nth1(J, F2, E2),
	nth1(J, F3, E3),
	nth1(J, F4, E4),
	nth1(J, F5, E5),
	C = [E1, E2, E3, E4, E5].

% retorna la cantidad de losas consecutivas dada una
% lista comenzado por el inicio de la lista determinada
countConsecutives([], 0).
countConsecutives([_:0|_], 0):- !.
countConsecutives([_:1|Y], R):- countConsecutives(Y, R1), R is R1 + 1.

getRRow([], _, _, []).
getRRow([_|Y], I, I, Y).
getRRow([_|Y], I, C, R):-
	K is C+1,
	getRRow(Y, I, K, R).


getLRow([], _, _, []).
getLRow(_, I, I, []).
getLRow([X|Y], I, C, [X|R]):-
	K is C+1,
	getLRow(Y, I, K, R).



updateWallAux(1, _, R2, R3, R4, R5, NR, [NR, R2, R3, R4, R5]).
updateWallAux(2, R1, _, R3, R4, R5, NR, [R1, NR, R3, R4, R5]).
updateWallAux(3, R1, R2, _, R4, R5, NR, [R1, R2, NR, R4, R5]).
updateWallAux(4, R1, R2, R3, _, R5, NR, [R1, R2, R3, NR, R5]).
updateWallAux(5, R1, R2, R3, R4, _, NR, [R1, R2, R3, R4, NR]).


% coloca la losa en el muro en la posicion [I, J]
updateWall(Wall, I, J, NWall):-
	nth1(I, Wall, WR),
	nth1(J, WR, C:_),
	setAtIndex(J, C:1, WR, NR),
	nth1(1, Wall, R1),
	nth1(2, Wall, R2),
	nth1(3, Wall, R3),
	nth1(4, Wall, R4),
	nth1(5, Wall, R5),
	updateWallAux(I, R1, R2, R3, R4, R5, NR, NWall).

countPointsAux(WR, C, I, J, P):-
	getRRow(WR, J, 1, RRow),
        getLRow(WR, J, 1, LRow),
	getLRow(C,  I, 1, ACol),
	getRRow(C,  I, 1, BCol),
	reverse(LRow, NLRow),
	reverse(ACol, NACol),
	countConsecutives(RRow, K1),
	countConsecutives(NLRow,K2),
	countConsecutives(NACol,K3),
	countConsecutives(BCol, K4),

	%print(I),nl,
	%print(J),nl,
	%print(RRow),nl,
	%print(LRow),nl,
	%print(ACol),nl,
	%print(BCol),nl,
        %print(K1),nl,
	%print(K2),nl,
	%print(K3),nl,
	%print(K4),nl,

	( K1 > 0, K3 > 0 -> P is K1+K2+K3+K4+2 ;
	  K1 > 0, K4 > 0 -> P is K1+K2+K3+K4+2 ;
	  K2 > 0, K3 > 0 -> P is K1+K2+K3+K4+2 ;
	  K2 > 0, K4 > 0 -> P is K1+K2+K3+K4+2 ; P is K1+K2+K3+K4+1 ).


countPointsAux1(Wall, _, -1, [0, Wall]).
countPointsAux1(Wall, I, E, [S, NWall]):-
	nth1(I, Wall, WR),
	indexOf(E:_, WR, J),
        getColumn(Wall, J, C),
	countPointsAux(WR, C, I, J, S),
	updateWall(Wall, I, J, NWall).
	%printMatrix(Wall1, 5),
	%print(S),
	%nl,nl.

% actualiza una escalera en dependencia de cuales filas estan
% llenas,retorna ademas las fichas sobrantes de cada fila
updateStair([F1, F2, F3, F4, F5], [E1, E2, E3, E4, E5],	[NStair, RC]):-
	( E1 =:= -1 -> NF1 = F1           ; NF1 = [empty] ),
	( E2 =:= -1 -> NF2 = F2, RC2 = [] ; NF2 = [empty, empty], RC2 = [E2] ),
	( E3 =:= -1 -> NF3 = F3, RC3 = [] ; NF3 = [empty, empty, empty], RC3 = [E3, E3] ),
	( E4 =:= -1 -> NF4 = F4, RC4 = [] ; NF4 = [empty, empty, empty, empty], RC4 = [E4, E4, E4] ),
	( E5 =:= -1 -> NF5 = F5, RC5 = [] ; NF5 = [empty, empty, empty, empty, empty],
	  RC5 = [E5, E5, E5, E5] ),
	RC = [RC2, RC3, RC4, RC5],
	NStair = [NF1, NF2, NF3, NF4, NF5].


% Dado un Jugador al final de la ronda i-esima, retorna
% el nuevo estado del jugador:
% actualiza el muro,
% actualiza la escalera,
% actualiza la puntuacion,
% actualiza el suelo,
% retorna ademas las fichas sobrantes de ese jugador para
% la proxima ronda
countPoints([Wall, Stair, Floor, Puntuation], [NPlayer, RC]):-
	%print('IN COUNTING'),nl,

	getStairColumn(Stair, [E1, E2, E3, E4, E5]),

	%print([E1, E2, E3, E4, E5]),
	%read(_),

	countPointsAux1(Wall, 1, E1, [P1, Wall1]),
	%printMatrix(Wall1, 5),nl,

	%read(_),

	countPointsAux1(Wall1, 2, E2, [P2, Wall2]),
	%printMatrix(Wall2, 5),nl,

	%read(_),

	countPointsAux1(Wall2, 3, E3, [P3, Wall3]),
	%printMatrix(Wall3, 5),nl,

	%read(_),

	countPointsAux1(Wall3, 4, E4, [P4, Wall4]),
        %printMatrix(Wall4, 5),nl,

	%read(_),

	countPointsAux1(Wall4, 5, E5, [P5, Wall5]),
	%printMatrix(Wall5, 5),nl,

        %read(_),

	length(Floor, Len),
	soil(F),

	( Len > 7 -> Neg is -14 ;  nth0(Len, F, Neg) ),

	%read(_),

        %print(Len),nl,
	%print(Neg),nl,

	abs(Neg, ANeg),

	TMP is P1+P2+P3+P4+P5-ANeg,

	%abs(TMP, ATMP),

	TMP1 is Puntuation+TMP,
	( TMP1 >= 0 -> NewPuntuation is TMP1 ; NewPuntuation is 0 ),

	%print(TMP),nl,
	%print(TMP1),nl,
	%print(NewPuntuation),nl,

	%print([P1, P2, P3, P4, P5]),nl,nl,



	updateStair(Stair, [E1, E2, E3, E4, E5], [NStair, RCS]),
	append(RCS, [Floor], RC),

	%printMatrix(Wall5, 5),nl,
	%printStair(NStair),nl,
	%print(NewPuntuation),nl,
	%print(RC),nl,nl,nl,

	NPlayer = [Wall5, NStair, [], NewPuntuation]

	%print(NPlayer)

	.


	%print(RRow),nl,
	%print(LRow),nl,
	%print(ACol),nl,
	%print(BCol),nl

	%print(1:J),nl,
	%print(E1),nl,
	%print(WR1)




test4:- stair2(S), wall2(W), countPoints([W, S, [2,2,1,1,1,1,1,1,1,s], 0], R).


test11:- wall2(W), updateWall(W, 4, 2, R), printMatrix(R, 5).



test12(K1, K2, K3, K4):-
	( K1 > 0, K3 > 0 -> P is K1+K2+K3+K4+2 ;
	  K1 > 0, K4 > 0 -> P is K1+K2+K3+K4+2 ;
	  K2 > 0, K3 > 0 -> P is K1+K2+K3+K4+2 ;
	  K2 > 0, K4 > 0 -> P is K1+K2+K3+K4+2 ; P is K1+K2+K3+K4+1 ), print(P).



test13:- stair2(S), findall(E, (member(E, S), completeRow(E)), R), print(R).


test14:- stair2(S), getStairColumn(S, R), print(R).

test15:- stair2(S),
	updateStair(S, [5, 1, -1, 4, -1], [S1, RC]),
	printStair(S1), nl,
	printStair(S),nl,
	print(RC)
	.
