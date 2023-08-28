:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
		direction/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.


update_beliefs(Perc):-

	% El agente olvida todo lo que recordaba
	retractall(time(_)),
	retractall(direction(_)),
	retractall(at(_, _, _)),
	retractall(node(_, _, _, _, _)),

	% y recuerda lo que percibió
	forall(member(Rel, Perc), assert(Rel)).


/* @TODO descomentar
% modificado
% @TODO usar assert o asserta?
update_beliefs(Perc):-
	retractall(time(_)),
	retractall(direction(_)),
	retractall(at(MyNode, agente, me)),
				
	% Lista de entidades en las creencias actuales
	findall(at(IdNode, EntityType, IdEntity), at(IdNode, EntityType, IdEntity), List_beliefs_entities),
	% add new entities
	forall((member(at(IdNode, EntityType, IdEntity), Perc), not(member(at(IdNode, EntityType, IdEntity), List_beliefs_entities))), 
		asserta(at(IdNode, EntityType, IdEntity))),

	% Lista de nodos en las creencias actuales
	findall(node(Id, PosX, PosY, Cost, Connections), node(Id, PosX, PosY, Cost, Connections), List_beliefs_nodes),
	% actualiza el costo y las conexiones de cada nodo encontrado previamente
	forall((member(node(Id, PosX, PosY, Cost, Connections), Perc), member(node(Id, PosX, PosY, _, _), List_beliefs_nodes)), 
			(retractall(node(Id, PosX, PosY, _, _)), 
			asserta(node(Id, PosX, PosY, Cost, Connections)))),
	% agrega nuevos nodos percibidos
	forall((member(node(Id, PosX, PosY, Cost, Connections), Perc), not(member(node(Id, PosX, PosY, Cost, Connections), List_beliefs_nodes))), 
		asserta(node(Id, PosX, PosY, Cost, Connections))),
	
	member(time(T), Perc),
	assert(time(T)),
	
	member(direction(D), Perc),
	assert(direction(D)).
*/