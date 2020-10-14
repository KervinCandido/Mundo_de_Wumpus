/*MAPA
B = buraco, W = wumpus, G = gold):
========================
|     |     |     | B  |
|-----|-----|-----|----|
| G   |  W  | B   |    |
|-----|-----|-----|----|
| 1/2 |     |     |    |
|-----|-----|-----|----|
|1/1  | 2/1 | B   |    |
========================
*/

%Faz com que o fato/conhecimento seja dinamico
:- dynamic([
	posicao/2,
    localizacao_agente/2,
	localizacao_wumpus/2,
	localizacao_buraco/2,
	localizacao_ouro/2,
	visitado/2
]).

%aux
and(A,B) :- A,B.
or(A,B) :- A;B.
nand(A,B) :- not(and(A,B)).
nor(A,B) :- not(or(A,B)).
xor(A,B) :- or(A,B), nand(A,B).

start :-
    write('=-=-=-=-=-=-=-=-= INICIANDO =-=-=-=-=-=-=-=-='),
    nl, %nova linha
    inicia_posicoes(4, 4),
    inicia_agente,
    inicia_wumpus,
    inicia_buracos,
    inicia_ouro,
    inicia_movimento_agente.

inicia_posicoes(M, N) :-
    foreach(between(1, M, I), 
		foreach(between(1, N, J),
			adiciona_posicao(I, J)             
        )
	).

inicia_agente :- 
    assertz(localizacao_agente(1, 1)),
    assertz(visitado(1, 1)).

inicia_wumpus :-
    assertz(localizacao_wumpus(2, 3)).

inicia_buracos :-
    assertz(localizacao_buraco(3, 1)),
    assertz(localizacao_buraco(3, 3)),
    assertz(localizacao_buraco(4, 4)).

inicia_ouro :-
    assertz(localizacao_ouro(1, 3)).

adiciona_posicao(X, Y) :-
    assertz(posicao(X, Y)).

%verifica se eh adjacente na vertical ou horizontal
eh_adjacente(X1, Y1, X2, Y2) :-
    posicao(X1, Y1),
    posicao(X2, Y2),
    xor(X1 =:= X2, Y1 =:= Y2),
    xor(
        xor(X1 + 1 =:= X2, X1 - 1 =:= X2),
    	xor(Y1 + 1 =:= Y2, Y1 - 1 =:= Y2)
    ).

sente_fedo(X, Y) :-
    localizacao_wumpus(X1, Y1),
    eh_adjacente(X, Y, X1, Y1).

sente_brisa(X, Y) :-
    localizacao_buraco(X1, Y1),
    eh_adjacente(X, Y, X1, Y1).

brilha(X, Y) :-
    localizacao_ouro(X, Y).

visitar(X, Y) :-
    assertz(visitado(X, Y)).

prox_localizacao :-
    localizacao_agente(X1, Y1),
    format('Localização atual do agente [~p, ~p] ~n', [X1, Y1]),
    eh_adjacente(X1, Y1, X2, Y2),
    not(sente_fedo(X1, Y1)),
    not(sente_brisa(X1, Y1)),
    not(visitado(X2, Y2)),
    move_agente(X2, Y2).

move_agente(X, Y) :-
    localizacao_agente(X1, Y1),
    (not(eh_adjacente(X, Y, X1, Y1)) ->  
    	(   
          eh_adjacente(X, Y, X2, Y2),
          eh_adjacente(X1, Y1, X2, Y2),
          visitado(X2, Y2),
          format('Agente se moveu para [~p, ~p] ~n', [X2, Y2])
        ); true
    ),
    format('Agente se moveu para [~p, ~p] ~n', [X, Y]),
    retractall(localizacao_agente(_, _)),
    assertz(localizacao_agente(X, Y)),
    visitar(X, Y),
    ( encontrou_ouro ->  true; true),
    ( encontrou_wumpus -> true; prox_localizacao).

encontrou_ouro :-
    localizacao_agente(X, Y),
    brilha(X, Y),
    write('Ouro encontrado na posição '),
    format('[~p, ~p] ', [X, Y]),
    writeln('!!!!'),
    writeln('Colentando o ouro').

encontrou_wumpus :-
    localizacao_agente(X, Y),
   	visitado(X, Y),
    sente_fedo(X, Y),
    sente_fedo(X1, Y1),
    xor(X1 =:= X, Y =:= Y),
    visitado(X1, Y1),
    eh_adjacente(X2, Y2, X1, Y1),
    eh_adjacente(X2, Y2, X, Y),
    not(visitado(X2, Y2)),
    write('Wumpus encontrado na posição'),
    format('[~p, ~p]~n', [X2, Y2]),
    writeln('Atirando a flecha >---->'),
    writeln('Wumpus foi morto !!').

inicia_movimento_agente :-
    prox_localizacao.


