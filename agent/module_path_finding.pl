:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- use_module(extras, [insertarOrdenado/3]).

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
	buscar(Frontera, [], Metas, Destino),
	encontrarCamino(Destino, C),
	append([Destino], C, C2),
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
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
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo
	agregarAlPrincipio(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, VisitadosOut, Nodo, Metas), % agrega vecinos a la frontera
	buscar(NuevaFrontera, VisitadosOut, Metas, MM). % continua la busqueda con la nueva frontera

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAlPrincipio(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo al comienzo de la lista.
%
agregarAlPrincipio(Nodo, Lista, [Nodo | Lista]).

agregarAlFinal(Lista, Nodo, Resultado) :- append(Lista, [Nodo], Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% generarVecinos(+Nodo, -Vecinos)
%
%   Genera los vecinos del nodo Nodo y los retorna en la lista Vecinos.
%
generarVecinos([Nodo, _], VecinosExistentes) :- 
	node(Nodo, _, _, _, Vecinos),
	findall( [N, C], (member([N, C], Vecinos), node(N, _, _, C, _)), VecinosExistentes),
	forall(member([Vec, _], VecinosExistentes), agregarPadreSiCorresponde(Vec, Nodo)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarPadreSiCorresponde(+Nodo, +Padre)
%
%   Reconoce a Padre como padre de Nodo si Nodo no tenía un padre previamente.
%

agregarPadreSiCorresponde(Vec, _) :- padre(Vec, _).

agregarPadreSiCorresponde(Vec, Nodo) :- asserta(padre(Vec, Nodo)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregar(+FronteraSinNodo, +Vecinos, -NuevaFrontera, +Visitados, -NuevosVisitados, +Nodo, +Metas), @TODO falta un parametro
% 
%  Agrega cada vecino de la lista Vecinos a la frontera si corresponde. 
%  La frontera y los nodos visitados cuando se modifican se retornan en 
%     NuevaFrontera y NuevosVisitados respectivamente.
%
agregar(FronteraSinNodo, [], FronteraSinNodo, Visitados, Visitados, _, _). % caso base

agregar(FronteraSinNodo, Vecinos, Frontera, Visitados, VisitadosOut, [Nodo, Costo], Metas) :-
	seleccionar([V, CV], Vecinos, RestoVecinos), % agarro un vecino
	agregar(FronteraSinNodo, RestoVecinos, NuevaFrontera1, Visitados, VisitadosOut1, [Nodo, Costo], Metas), % itero sobre el resto de vecinos
	encontrarCamino(V, Camino), 
	costoCamino(Camino, G), % calculo cuanto cuesta llegar desde la raiz hasta ese vecino
	recorrerMetas(Nodo, [V, CV], G, Metas, NuevaFrontera1, VisitadosOut1, VisitadosOut, Frontera).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% recorrerMetas(+Padre, +[Nodo, C], +G, +Metas, +Frontera, +Visitados, -NuevosVisitados, -NuevaFrontera)
%
%   Recorre las metas posibles y calcula el valor heuristico para cada una.
%   Caso base, no hay metas.
%   Caso recursivo, selecciono la primer meta, itero sobre el resto de metas, 
%     calculo el valor heurístico H desde Nodo hasta la meta, 
%     calculo el valor F como la suma G + H,
%     y luego agrego a la frontera si corresponde hacerlo.
% 

recorrerMetas(_, _, _, [], Frontera, Visitados, Visitados, Frontera). % caso base

recorrerMetas(Padre, [Nodo, C], G, Metas, Frontera, Visitados, NuevosVisitados, NuevaFrontera) :-

	seleccionar(Meta, Metas, RestoMetas), % selecciono la primer meta 
	recorrerMetas(Padre, [Nodo, C], G, RestoMetas, Frontera, Visitados, NuevosVisitados1, NuevaFrontera1), % itero sobre el resto de metas
	calcularH(Nodo, Meta, H), % calculo H para el nodo actual y la meta actual
	F is G + H, % calculo F como la suma que vimos en clase y eso
	agregarAListaCorrespondiente(Padre, [Nodo, C], F, NuevaFrontera1, NuevosVisitados1, NuevosVisitados, NuevaFrontera). % agrego si corresponde


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% agregarAListaCorrespondiente(+Padre, +[IdNodo, Cost], +F, +Frontera, +Visitados, -NuevosVisitados, -NuevaFrontera)
%
%   Actualiza la frontera y los visitados si corresponde.
%   Si el nodo IdNodo existe en la frontera con un costo mayor a F, lo actualizo.
%   Si existe en los visitados con un costo mayor a F, lo borro de los visitados y lo agrego a la frontera.
%   Si existe en cualquiera de las dos con un costo menor o igual a F, lo salteo.
%   Si no se da ningún caso, lo agrego a la frontera.
% 
%   La Frontera y los Visitados una vez actualizados son devueltos en NuevaFrontera y NuevosVisitados respectivamente.
% 

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, NuevaFrontera) :-
	member([IdNodo, CurrentCost], Frontera), % si pertenece a la frontera 
	F < CurrentCost, % y el nuevo F es menor al ya calculado
	!,
	delete(Frontera, [IdNodo, CurrentCost], NuevaFrontera1), 
	retractall(padre(IdNodo, _)),
	asserta(padre(IdNodo, Padre)),
	insertarOrdenado([IdNodo, F], NuevaFrontera1, NuevaFrontera). % actualizo la frontera

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, NuevosVisitados, NuevaFrontera) :-
	member([IdNodo, CurrentCost], Visitados), % si pertenece a los visitados
	F < CurrentCost, % y el nuevo F es menor al ya calculado
	!,
	delete(Visitados, [IdNodo, CurrentCost], NuevosVisitados), 
	retractall(padre(IdNodo, _)),
	asserta(padre(IdNodo, Padre)),
	insertarOrdenado([IdNodo, F], Frontera, NuevaFrontera). % actualizo la frontera

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, Frontera) :-
	member([IdNodo, _], Visitados). % si pertenece a los visitados
	% El nuevo F es mayor o igual al ya calculado

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, Frontera) :-
	member([IdNodo, _], Frontera).
	% si no se da ningun caso previo pero el nodo ya pertenece a la frontera, no hago nada

agregarAListaCorrespondiente(Padre, [IdNodo, Cost], F, Frontera, Visitados, Visitados, NuevaFrontera) :-
	insertarOrdenado([IdNodo, F], Frontera, NuevaFrontera). % caso final, agrego el nodo a la frontera

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
