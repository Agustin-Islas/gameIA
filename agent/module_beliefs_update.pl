:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
		direction/1,
		elim_entities_disappeared/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% El agente actualizan las creencias de la siguiente forma:
% a) Si el elemento ya pertenecía a las creencias actuales y cambió alguno de sus atributos,
% se lo actualiza con la nueva información sinó se lo deja como está.
% b) Si el elemento no pertenecía a las creencias actuales, se lo agrega.
%
% c) Por último si el agente tenía en su estado interno alguna entidad que se encontraba ubicada en la posición
% de un nodo que es percibido nuevamente y en donde en este no se encuentran entidades,
% quiere decir que la entidad desapareció y por lo tanto se la elimina de las creencias.


update_beliefs(Perc):-
	retractall(time(_)),
	retractall(direction(_)),
	retractall(at(MyNode, agente, me)),

	% Lista de entidades en las creencias actuales
	findall(at(IdNode, EntityType, IdEntity), at(IdNode, EntityType, IdEntity), List_beliefs_entities),
	% Lista de nodos en las creencias actuales
	findall(node(Id, PosX, PosY, Cost, Connections), node(Id, PosX, PosY, Cost, Connections), List_beliefs_nodes),

	elim_entities_disappeared(List_beliefs_entities, Perc),
	actualice_nodes(List_beliefs_nodes, Perc),
	actualice_entities(List_beliefs_entities, Perc),
	
	member(time(T), Perc), % Actualiza el tiempo
	assert(time(T)),
	
	member(direction(D), Perc), % Actualiza la dirección
	assert(direction(D)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% elim_entities_disappeared(+List_beliefs_entities, +Perc)
%
% Busca todas las entidades que estan el List_beliefs_entities y tienen el mismo Idnode que los nodos
% encontrados en Perc, pero no estan en las entidades de Perc y las elimina de las creencias.
elim_entities_disappeared(List_beliefs_entities, Perc):-
	findall(at(IdNode, EntityType, IdEntity),
		(member(at(IdNode, _, _), List_beliefs_entities),
		 member(node(IdNode, _, _, _, _), Perc),
		 not(member(at(IdNode, _, _), Perc))),
		List_entities_to_elim),
			
	% Eliminar las entidades encontradas de List_entities_to_elim de las creencias actuales
	forall(member(at(IdNode, EntityType, IdEntity), List_entities_to_elim),
			retractall(at(IdNode, EntityType, IdEntity))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% actualice_nodes(+List_beliefs_nodes, +Perc)
%
% Actualiza el costo y las conexiones de cada nodo encontrado previamente y 
% Agrega nuevos nodos percibidos.
%
actualice_nodes(List_beliefs_nodes, Perc):-
	% actualiza el costo y las conexiones de cada nodo encontrado previamente
	forall((member(node(Id, PosX, PosY, Cost, Connections), Perc), member(node(Id, PosX, PosY, _, _), List_beliefs_nodes)), 
			(retractall(node(Id, PosX, PosY, _, _)), 
			asserta(node(Id, PosX, PosY, Cost, Connections)))),
	
	% agrega nuevos nodos percibidos
	forall((member(node(Id, PosX, PosY, Cost, Connections), Perc), not(member(node(Id, PosX, PosY, Cost, Connections), List_beliefs_nodes))), 
		asserta(node(Id, PosX, PosY, Cost, Connections))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% actualice_entities(List_beliefs_entities, Perc)
%
% Actualiza los relojes y agrega nuevas entidades
%
actualice_entities(List_beliefs_entities, Perc):-
	% Actualizacion de relojes
	forall(((member(at(IdNode, reloj(Old), IdEntity), List_beliefs_entities)), (member(at(IdNode, reloj(New), IdEntity), Perc))),
			retractall(at(IdNode, reloj(Old), IdEntity))),

	% agrega nuevas entidades
	forall((member(at(IdNode, EntityType, IdEntity), Perc), not(member(at(IdNode, EntityType, IdEntity), List_beliefs_entities))), 
		asserta(at(IdNode, EntityType, IdEntity))).