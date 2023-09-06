
:- use_module(module_beliefs_update, [
 	update_beliefs/1,
 	time/1,
 	node/5,
	at/3,
	direction/1
]).

:- use_module(module_path_finding, [
 	buscar_plan_desplazamiento/4,
 	raiz/1,
 	padre/2,
 	esMeta/1,
	seleccionar/3
]).

:- use_module(extras, [
	append3/4
]).

:- dynamic plandesplazamiento/1, vueltacompleta/1.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run(+Perc, -Action, -Text, -Beliefs)
%
% El predicado run/4 implementa el comportamiento del agente.
%
% Es ejecutado automáticamente por el juego una vez por ciclo.
% Implementa la interface con el mundo virtual, 
% recibiendo la información que el agente puede percibir del entorno
% y permite hacer que el agente actue en el mundo virtual
% No pueden cambiarse los parámetros, pero si el cuerpo.
% 
% El primer parámetro (Perc) recibe una lista con la percepción del agente.
% Los otros parámetros envían información al juego.
% La idea es que el agente actualice su estado interno usando Perc, 
% y luego decida que acción hacer instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
% Beliefs es una lista con un subconjunto de creencias del agente, particularmente las que hacer referencia a objetos.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
% IMPORTANTE: La lista de la percepción no viene ordenada. El formato anterior es solo a modo de referencia.
%
% Action debe ser un functor totalmente instanciado (sin variables) que corresponda a una acción valida.
% Si Action tiene algún error el agente pierde el ciclo de ejecución.

run(Perc, Action, Text, Beliefs):-
	update_beliefs(Perc), % implementado en module_beliefs_update
	decide_action(Action, Text),
	findall(at(X, Y, Z), at(X, Y, Z), Beliefs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% decide_action(-Action, -Text)
%
% Decide la acción a realizar por el agente, instanciándola en el parámetro Action.
% Text es un texto con comillas simples como 'hola' que será mostrado en la pantalla del juego.
%
% En la implementación siguiente:
% Los primeros 5 casos): sirven para que el agente pueda levantar objetos del suelo.
% El sexto caso (6) permite al agente ejecutar la siguiente acción de movimiento de un plan guardado,
% calculado previamente.
% El septimo caso (7) permite obtener un nuevo plan de movimiento usando A*.
% El octavo caso (8) hace que el agente gire en su posición, en sentido horario.
% El noveno caso (9) del predicado siempre tiene éxito, y hace que el agente se mueva de manera aleatoria.
%

% Si estoy en la misma posición que una copa, intento levantarla.
decide_action(Action, 'Quiero levantar una copa...') :-
    at(MyNode, agente, me),
    at(MyNode, copa, IdGold),
    node(MyNode, PosX, PosY, _, _),
    Action = levantar_tesoro(IdGold, PosX, PosY),
    retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	!.

% Si estoy en la misma posición que una pocion, intento levantarla.
decide_action(Action, 'Quiero levantar una pocion...') :-
	at(MyNode, agente, me),
	at(MyNode, pocion, IdGold),
	node(MyNode, PosX, PosY, _, _),
	Action = levantar_tesoro(IdGold, PosX, PosY),
	retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	!.

% Si estoy en la misma posición que un reloj, intento levantarlo.
decide_action(Action, 'Quiero levantar un reloj...') :-
	at(MyNode, agente, me),
	at(MyNode, reloj(_), IdGold),
	node(MyNode, PosX, PosY, _, _),
	Action = levantar_tesoro(IdGold, PosX, PosY),
	retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	!.

% Si estoy en la misma posición que un cofre, intento levantarlo.
decide_action(Action, 'Quiero levantar un cofre...') :-
	at(MyNode, agente, me),
	at(MyNode, cofre, IdGold),
	node(MyNode, PosX, PosY, _, _),
	Action = levantar_tesoro(IdGold, PosX, PosY),
	retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	!.

% Si estoy en la misma posición que un diamante, intento levantarlo.
decide_action(Action, 'Quiero levantar un diamante...') :-
	at(MyNode, agente, me),
	at(MyNode, diamante, IdGold),
	node(MyNode, PosX, PosY, _, _),
	Action = levantar_tesoro(IdGold, PosX, PosY),
	retractall(at(MyNode, _, IdGold)),
	retractall(plandesplazamiento(_)),
	!.

% Si tengo un plan de movimientos, ejecuto la siguiente acción.
decide_action(Action, 'Avanzar...'):-
	plandesplazamiento(Plan),
	length(Plan, LargoPlan),
	LargoPlan > 0,
	!,
	obtenerMovimiento(Plan, Action, Resto),
	retractall(plandesplazamiento(_)),
	assert(plandesplazamiento(Resto)).
	
% Si no tengo un plan guardado, busco uno nuevo.
decide_action(Action, 'Avanzar con nuevo plan...'):-
	busqueda_plan(Plan, _Destino, _Costo),
	write(Plan),
	Plan \= [],
	obtenerMovimiento(Plan, Action, Resto),
	assert(plandesplazamiento(Resto)),
	!.

% Giro en sentido horario, para conocer mas terreno.
decide_action(Action, 'Girar para conocer el territorio...'):-
	not(vueltacompleta(_)),
	!,
	(
		direction(w)
		-> Action = girar(d)
		; ( direction(d)
			-> Action = girar(s)
			; ( direction(s)
				-> Action = girar(a)
				; Action = girar(w), asserta(vueltacompleta(w))
				)			
		)
	).

% Me muevo a una posición vecina seleccionada de manera aleatoria.
decide_action(Action, 'Me muevo a la posicion de al lado...'):-
	at(MyNode, agente, me),
	node(MyNode, _, _, _, AdyList),
	length(AdyList, LenAdyList), LenAdyList > 0,
	random_member([IdAdyNode, _CostAdyNode], AdyList),
	!,
	retractall(vueltacompleta(_)),
	Action = avanzar(IdAdyNode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerMovimiento(?Lista, ?Movimiento, ?Resto)
%
% Obtiene el primer movimiento de una lista de movimientos.
% 
obtenerMovimiento([], [], []).
obtenerMovimiento([X|Xs], X, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% busqueda_plan(-Plan, -Destino, -Costo)
%
% Busca un plan de desplazamiento hacia el tesoro que se encuentre mas cerca.
%	
busqueda_plan(Plan, Destino, Costo):-
 	retractall(plandesplazamiento(_)),
 	retractall(esMeta(_)),

 	findall(Nodo, (at(Nodo, EntityType, _), EntityType \= agente), Metas), % nuevas metas
 	buscar_plan_desplazamiento(Metas, Plan, Destino, Costo).