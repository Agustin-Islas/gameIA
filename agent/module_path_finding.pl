:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.
%
eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%	
% Selecciona el primer nodo de la lista Frontera.
%	
seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo Meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera, 
% en la busqueda de llegar de un nodo origen a uno destino.
%
encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.
%
crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)
%

buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
	CantMetas > 0,
	!,
	retractall(raiz(_)),
	retractall(padre(_,_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),
	crearPlan(Camino, Plan).
	
buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.
%
	
buscarEstrella(Frontera, Metas, Camino, Costo, Destino):-
	write('1\n'),
	buscar(Frontera, [], Metas, Destino),
	write('2\n'),
	encontrarCamino(Destino, C),
	append([Destino], C, C2),	
	write('4\n'),
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	write('8\n'),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
% 
% Busca el camino optimo desde la frontera hacia la Meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera, 
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.
	
buscar(Frontera, _, _M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
	esMeta(Nodo),
	!.

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	%write('%% '), write(Nodo), write(' %%\n'),
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo - TO-DO
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados

	%write('(( '), write(NuevosVisitados), write(' ))\n'),
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas), % agrega vecinos a la frontera - TO-DO
	buscar(NuevaFrontera, NuevosVisitados, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%
agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% generarVecinos(+Nodo, -Vecinos)
%
% Genera los vecinos del nodo Nodo.
%
generarVecinos([Nodo, _], Vecinos) :- 
	node(Nodo, _, _, _, Vecinos),
	forall(member([Vec, _], Vecinos), assert(padre(Vec, Nodo)) ). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregar(+FronteraSinNodo, +Vecinos, -Frontera, +Visitados, +Nodo, +Metas), 
% 
% Agrega vecinos a la frontera
% 
agregar(FronteraSinNodo, [], FronteraSinNodo, _, _, _). % caso base

agregar(FronteraSinNodo, Vecinos, Frontera, Visitados, [Nodo, Costo], Metas) :-

	seleccionar([V, CV], Vecinos, RestoVecinos), % agarro un vecino
	agregar(FronteraSinNodo, RestoVecinos, NuevaFrontera1, Visitados, [Nodo, Costo], Metas), % itero sobre el resto de vecinos
	encontrarCamino(V, Camino), 
	costoCamino(Camino, G), % calculo cuanto cuesta llegar desde la raiz hasta ese vecino
	recorrerMetas(Nodo, [V, CV], G, Metas, NuevaFrontera1, Visitados, NuevosVisitados, Frontera). % ni idea
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% recorrerMetas(+Nodo, +G, +Metas, ?Frontera), 
% 
% Recorre las metas posibles y calcula el valor heuristico para cada una.
% 
recorrerMetas(_, _, _, [], Frontera, Visitados, Visitados, Frontera). % caso base

recorrerMetas(Padre, [Nodo, C], G, Metas, Frontera, Visitados, NuevosVisitados2, NuevaFrontera2) :-

	seleccionar(Meta, Metas, RestoMetas), % selecciono la primer meta 
	recorrerMetas(Padre, [Nodo, C], G, RestoMetas, Frontera, Visitados, NuevosVisitados1, NuevaFrontera1), % itero sobre el resto de metas
	calcularH(Nodo, Meta, H), % calculo H para el nodo actual y la meta actual
	F is G + H, % calculo F como la suma que vimos en clase y eso
	agregarAListaCorrespondiente(Padre, [Nodo, C], F, NuevaFrontera1, NuevosVisitados1, NuevosVisitados2, NuevaFrontera2). % agrego si corresponde


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
%
%
%
%
agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, NuevaFrontera) :-
	member([IdNodo, CurrentCost], Frontera), % si pertenece a la frontera 
	F < CurrentCost, % y el nuevo F es menor al ya calculado
	!,
	delete(Frontera, [IdNodo, CurrentCost], NuevaFrontera1), 
	retractall(padre(IdNodo, _)),
	assert(padre(IdNodo, Padre)),
	agregarAVisitados(NuevaFrontera1, [IdNodo, F], NuevaFrontera). % actualizo la frontera

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, NuevosVisitados, NuevaFrontera) :-
	member([IdNodo, CurrentCost], Visitados), % si pertenece a los visitados
	F < CurrentCost, % y el nuevo F es menor al ya calculado
	!,
	delete(Visitados, [IdNodo, CurrentCost], NuevosVisitados), 
	retractall(padre(IdNodo, _)),
	assert(padre(IdNodo, Padre)),
	agregarAVisitados(Frontera, [IdNodo, F], NuevaFrontera). % actualizo la frontera

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, Frontera) :-
	member([IdNodo, CurrentCost], Visitados), % si pertenece a los visitados
	CurrentCost =< F. % y el nuevo F es mayor o igual al ya calculado
	%!. % salteo el nodo

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, Frontera) :-
	member([IdNodo, F], Frontera).
	% si no se da ningun caso previo pero el nodo ya pertenece a la frontera, no hago nada

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, NuevaFrontera) :-
	agregarAVisitados(Frontera, [IdNodo, F], NuevaFrontera),
	assert(padre(IdNodo, Padre)).
	% caso final, agrego el nodo a la frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino, 
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.
%
costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la distancia euclidea.
%
calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
	distance([X1, Y1], [X2, Y2], Resultado).

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
	Distance is sqrt(DX^2 + DY^2).
