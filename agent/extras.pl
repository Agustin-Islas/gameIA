:- module(extras,
	  [
	    append3/4,
		insertarOrdenado/3
	  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% append3(?Lista1, ?List2, ?Lista3, ?Res)
%
% Concatena tres listas Xs, Ys y Zs.
% 
append3(Xs, Ys, Zs, XsYsZs) :-
	append(Xs, YsZs, XsYsZs),
	append(Ys, Zs, YsZs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% insertarOrdenado(+[Idx, Cx], +Lista1, -Salida)
%
% Inserta al nodo [Idx, Cx] en la lista Lista1 ordenado en forma creciente
%	y devuelve el resultado en la lista Salida. 
%

insertarOrdenado(X, [], [X]).
insertarOrdenado([X, Cx], [[Y, Cy]|Resto], [[X, Cx],[Y, Cy]|Resto]) :-
    Cx < Cy, !.
insertarOrdenado(X, [Y|RestoY], [Y|Resto]) :-
    insertarOrdenado(X, RestoY, Resto).