%% -----------------------------------------------------Problema 1

%% hechos de los guardianes y teemplos
guardia(apolo).
guardia(hecate).
guardia(ares).
guardia(hermes).

elementos(fuego).
elementos(agua).
elementos(tierra).
elementos(aire).

nocuida(apolo, fuego).
nocuida(apolo, tierra).
nocuida(hecate, aire).
nocuida(ares, agua).
nocuida(ares, aire).
nocuida(hermes, fuego).
nocuida(hermes, agua).

%% reglas

% Reglas
cuida(Guardian, Templo) :-
    guardia(Guardian),
    elementos(Templo),
    \+ nocuida(Guardian, Templo).


asignar_guardianes([(apolo, TemploApolo), (hecate, TemploHecate), (ares, TemploAres), (hermes, TemploHermes)]) :-
    cuida(apolo, TemploApolo),
    cuida(hecate, TemploHecate),
    cuida(ares, TemploAres),
    cuida(hermes, TemploHermes),
    % Asegurar que cada templo sea único
    TemploApolo \= TemploHecate,
    TemploApolo \= TemploAres,
    TemploApolo \= TemploHermes,
    TemploHecate \= TemploAres,
    TemploHecate \= TemploHermes,
    TemploAres \= TemploHermes.

% Mostrar resultados
resolver :-
    asignar_guardianes(Asignaciones),
    mostrar_resultados(Asignaciones).

mostrar_resultados([]).
mostrar_resultados([(Guardia, Templo) | Resto]) :-
    format('~w cuida el templo de ~w~n', [Guardia, Templo]),
    mostrar_resultados(Resto).


%% -----------------------------------------------------Problema 2

% Hechos: héroes y armas
heroe(aquiles).
heroe(perseo).
heroe(hercules).
heroe(teseo).

arma(espada).
arma(lanza).
arma(arco).
arma(escudo).

% Restricciones
nousa(aquiles, escudo).
nousa(aquiles, arco).
nousa(perseo, espada).
nousa(hercules, lanza).
nousa(hercules, escudo).
usa(teseo, arco).
usa(teseo, escudo).

% Reglas
poseearma(Heroe, Arma) :-
    heroe(Heroe),
    arma(Arma),
    \+ nousa(Heroe, Arma).

asignar_armas([(aquiles, ArmaAquiles), (perseo, ArmaPerseo), (hercules, ArmaHercules), (teseo, ArmaTeseo)]) :-
    poseearma(aquiles, ArmaAquiles),
    poseearma(perseo, ArmaPerseo),
    poseearma(hercules, ArmaHercules),
    poseearma(teseo, ArmaTeseo),
    % Asegurar que cada arma sea única
    ArmaAquiles \= ArmaPerseo,
    ArmaAquiles \= ArmaHercules,
    ArmaAquiles \= ArmaTeseo,
    ArmaPerseo \= ArmaHercules,
    ArmaPerseo \= ArmaTeseo,
    ArmaHercules \= ArmaTeseo.

% Mostrar resultados
resolver_armas :-
    asignar_armas(Asignaciones),
    mostrar_resultados(Asignaciones).

mostrar_resultados_armas([]).
mostrar_resultadosarmas([(Heroe, Arma) | Resto]) :-
    format('~w usa ~w~n', [Heroe, Arma]),
    mostrar_resultadosarmas(Resto).


%% -----------------------------------------------------Problema 3

% Hechos: titanes y gemas
titanes([cronos, oceano, hiperion, japeto]).
gemas([zafiro, rubi, esmeralda, diamante]).

% Restricciones
noposee(cronos, zafiro).
noposee(cronos, diamante).
noposee(oceano, rubi).
noposee(hiperion, zafiro).
noposee(hiperion, rubi).
posee(japeto, diamante). % Japeto puede poseer solo una de estas dos gemas.
posee(japeto, esmeralda).

% Regla: verificar si una asignación es válida
es_asignacion_valida((Titan, Gema)) :-
    \+ noposee(Titan, Gema),
    (Titan \= japeto ; posee(japeto, Gema)).

% Resolver el problema
resolver_gemas :-
    titanes(Titanes),
    gemas(Gemas),
    permutation(Gemas, GemasAsignadas),  % Genera todas las permutaciones posibles de gemas
    pairs_keys_values(Asignaciones, Titanes, GemasAsignadas), % Une titanes con gemas
    maplist(es_asignacion_valida, Asignaciones), % Valida cada asignación
    mostrar_resultados_gemas(Asignaciones).

% Mostrar resultados
mostrar_resultados_gemas([]).
mostrar_resultados_gemas([(Titan, Gema) | Resto]) :-
    format('~w tiene ~w~n', [Titan, Gema]),
    mostrar_resultados_gemas(Resto).
