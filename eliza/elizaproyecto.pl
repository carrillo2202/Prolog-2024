
eliza:-	writeln('Hola , mi nombre es  Eliza tu  chatbot,
	por favor ingresa tu consulta,
	usar solo minúsculas sin . al final:'),
	readln(Input),
	eliza(Input),!.
eliza(Input):- Input == ['Adios'],
	writeln('Adios. espero poder verte ayudado.'), !.
eliza(Input):- Input == ['Adios', '.'],
	writeln('Adios. espero poder verte ayudado.'), !.
eliza(Input) :-
	template(Stim, Resp, IndStim),
	match(Stim, Input),
	% si he llegado aquí es que he
	% hallado el template correcto:
	replace0(IndStim, Input, 0, Resp, R),
	writeln(R),
	readln(Input1),
	eliza(Input1), !.

eliza(Input) :-
    template(Stim, Resp, IndStim),
    match(Stim, Input),
    replace0(IndStim, Input, 0, Resp, R),
    writeln(R), % Mostrar respuesta correctamente
    !. % Termina después de procesar una consulta válida

process_input(Input) :-
    (   
        % Busca coincidencia en plantillas
        template(Stim, Resp, IndStim),
        match(Stim, Input) ->
        replace0(IndStim, Input, 0, Resp, R),
        writeln(R)
    ;
        % Si no coincide ninguna plantilla
        writeln('No entiendo tu consulta. Por favor, intenta nuevamente.')
    ),
    % Vuelve a leer entrada
    readln(NewInput),
    process_input(NewInput).

% Declaraciones discontiguous
:- discontiguous template/3.
:- discontiguous replace0/5.

template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_), '.'], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).

template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_), '.'], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).

template([hola, _], ['Hola', 'como', estas, tu, '?'], []).
template([buendia, _], ['Buendia', 'Como', estas, tu, '?'], []).

template([yo, s(_), yo, soy, s(_),'.'], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu, '.'], [why, do, you, 0, me ,'?'], [1]).
template([yo, soy, s(_),'.'], [porque, eres, tu, 0, '?'], [2]).

% pregunta algo que le gusta a eliza
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).
template([que, te, gusta, s(_), _], [flagLikesNew], [3]).

		 % pregunta algo que hace eliza
template([tu, eres, s(_), _], [flagDo], [2]).
% pregunta algo que es eliza
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

	% pregunta sobre sobre datos de eliza
template([eres, de, s(_), _], [flagWhere], [2]).
% template([donde, vives,_], [flagWhere], [2]).

template([te, gusta, ver, peliculas, de, s(_)], [flagWatch], [5]).
template([alguna, vez, has, ido, a, s(_)], [flagVisit], [5]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar, '.'], []).

% Nuevas reglas
template([alguna, vez, has, _], [no, pero, me, gustaria, algun, dia], []).
template([te, puedo, preguntar, _], [claro, puedes, preguntar], []).
template([consideras , importante,  _], [claro, es, de, suma, importancia], []).
template([como, te, llamas,  _], [mucho, gusto, mi, nombre, es, eliza], []).
template([cual , es, tu, edad,  _], [no, tengo, una, edad, existo, desde, siempre], []).

template([eres , un, ser, humano,  _], [no, fisicamente, pero, lo, intento], []).
% terminan nuevas reglas

template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon, '.'], []).
template([i, have, s(_), with, s(_), '.'], ['You', have, to, deal, with, your, 0, and, your, 1, in, a, mature, way, '.'], [2, 4]).
template([i, s(_),  _], [i, can, recommend, you, a, book, about, that, issue], []).
template([please, s(_), _], ['No', i, can, not, help, ',', i, am, just, a, machine], []). 
		 template([tell, me, a, s(_), _], ['No', i, can, not, ',', i, am, bad, at, that], []).

% Templates de familia
template([quien, es, el, padre, de, s(_)], [flagfather], [5]).
template([quien, es, la, madre, de, s(_)], [flagmother], [5]).
template([quienes, son, los, hermanos, de, s(_)], [flagsiblings], [5]).
template([quienes, son, los, primos, de, s(_)], [flagcousins], [5]).
template([quien, es, el, abuelo, de, s(_)], [flaggrandfather], [5]).
template([quien, es, la, abuela, de, s(_)], [flaggrandmother], [5]).
template([quienes, son, los, tios, de, s(_)], [flaguncles], [5]).
template([quienes, son, las, tias, de, s(_)], [flagaunts], [5]).
template([quienes, son, los, nietos, de, s(_)], [flaggrandsons], [5]).
template([quienes, son, las, nietas, de, s(_)], [flaggranddaughters], [5]).
template([quienes, son, los, hijos, de, s(_)], [flagson], [5]).
template([quienes, son, las, hijas, de, s(_)], [flagdaughter], [5]).

template([quiero, encontrar, un, auto], [flagFindAuto], [3]).
template([que, marcas, producen, autos, deportivos], [flagFindDeportivos], [4]).
template([que, autos, mazda, hay, disponibles], [flagFindMazda], [4]).
template([que, autos, toyota, hay, disponibles], [flagFindToyota], [4]).
template([que, autos, honda, hay, disponibles], [flagFindHonda], [4]).
template([que, autos, nissan, hay, disponibles], [flagFindNissan], [4]).
template([que, autos, chevrolet, hay, disponibles], [flagFindChevrolet], [4]).
template([Marca, [s(Tipos)]], [flagFindAutoFinal], [0]).
template([s(Tipos)], [flagFindMarca], [0]).

template(_, ['No entiendo tu consulta. Por favor, intenta nuevamente.'], []).

% Hechos para la familia

% Hechos
hombre(francisco).
hombre(francisco_jr).
hombre(brayan).
hombre(gumercindo).
hombre(edy).
mujer(paola).
mujer(blanca).
mujer(karina).
mujer(valeria).
mujer(yocelyn).


padre(francisco, francisco_jr).
padre(francisco, brayan).
padre(gumercindo, paola).
padre(gumercindo, karina).
padre(edy, valeria).
padre(edy, yocelyn).
madre(paola, francisco_jr).
madre(paola, brayan).
madre(karina, valeria).
madre(karina, yocelyn).
madre(blanca, paola).
madre(blanca, karina).
esposos(francisco, paola).
esposos(edy, karina).
esposo(francisco, paola).
esposo(edy, karina).
esposa(paola,francisco).
esposa(karina,edy).


% Reglas
abuelo(X, Y) :- padre(X, Z), padres(Z, Y).
abuela(X, Y) :- madre(X, Z), padres(Z, Y).
hermanos(X, Y) :- (padre(Z, X), padre(Z, Y); madre(W, X), madre(W, Y)), X \= Y.
primos(X, Y) :- padres(A, X), padres(B, Y), hermanos(A, B).
padres(X, Y) :- padre(X, Y); madre(X, Y).
cunado(X, Y) :- esposos(X, Z), hermanos(Z, Y). 
cunado(X, Y) :- esposos(Y, Z), hermanos(Z, X). 
tio(X, Y) :- hermanos(X, Z), padres(Z, Y), hombre(X).          
tio(X, Y) :- esposos(X, W), tia(W, Y).                         
tia(X, Y) :- hermanos(X, Z), padres(Z, Y), mujer(X).           
tia(X, Y) :- esposos(X, W), tio(W, Y).   
sobrino(X, Y) :- tio(Y, X), hombre(X).
sobrino(X, Y) :- tia(Y, X), hombre(X).
sobrina(X, Y) :- tio(Y, X), mujer(X).
sobrina(X, Y) :- tia(Y, X), mujer(X).
nieto(X, Y) :- abuelo(Y, X), hombre(X).
nieto(X, Y) :- abuela(Y, X), hombre(X).
nieta(X, Y) :- abuelo(Y, X), mujer(X).
nieta(X, Y) :- abuela(Y, X), mujer(X).
hijo(X, Y) :- padre(Y, X), hombre(X).
hijo(X, Y) :- madre(Y, X), hombre(X).
hija(X, Y) :- padre(Y, X), mujer(X).
hija(X, Y) :- madre(Y, X), mujer(X).

% Respuestas específicas
respuesta(flagfather, [S], R) :- padre(R, S).
respuesta(flagmother, [S], R) :- madre(R, S).
respuesta(flagsiblings, [S], R) :- findall(X, hermanos(S, X), R).
respuesta(flagcousins, [S], R) :- findall(X, primos(S, X), R).
respuesta(flaggrandfather, [S], R) :- abuelo(R, S).
respuesta(flaggrandmother, [S], R) :- abuela(R, S).
respuesta(flaguncles, [S], R) :- findall(X, tio(X, S), R).
respuesta(flagaunts, [S], R) :- findall(X, tia(X, S), R).
respuesta(flaggrandsons, [S], R) :- findall(X, nieto(X, S), R).
respuesta(flaggranddaughters, [S], R) :- findall(X, nieta(X, S), R).
respuesta(flagson, [S], R) :- hijo(S, R).
respuesta(flagdaughter, [S], R) :- hija(S, R).


% Bandera: Hijos (son)
flagson(Person, Son) :-
    hijo(Person, Son).

% Bandera: Hijas (daughter)
flagdaughter(Person, Daughter) :-
    hija(Person, Daughter).

% Derivar hijo/hija a partir de padre/madre
hijo(PadreOMadre, Hijo) :-
    padre(PadreOMadre, Hijo), hombre(Hijo).
hijo(PadreOMadre, Hijo) :-
    madre(PadreOMadre, Hijo), hombre(Hijo).
hija(PadreOMadre, Hija) :-
    padre(PadreOMadre, Hija), mujer(Hija).
hija(PadreOMadre, Hija) :-
    madre(PadreOMadre, Hija), mujer(Hija).

% Hechos sobre los autos
auto(sedan, toyota, camry).
auto(suv, toyota, rav4).
auto(pickup, toyota, tacoma).
auto(deportivo, toyota, gt86).
auto(hatchback, toyota, yaris).

auto(sedan, honda, civic).
auto(suv, honda, crv).
auto(pickup, honda, ridgeline).
auto(deportivo, honda, civictypeR).
auto(hatchback, honda, city).

auto(sedan, mazda, tres).
auto(suv, mazda, cx90).
auto(pickup, mazda, bt50).
auto(deportivo, mazda, mx5).
auto(hatchback, mazda, dos).

auto(sedan, nissan, versa).
auto(suv, nissan, xtrail).
auto(pickup, nissan, frontier).
auto(deportivo, nissan, gtr).
auto(hatchback, nissan, march).

auto(sedan, chevrolet, civic).
auto(suv, chevrolet, blazer).
auto(pickup, chevrolet, s10).
auto(deportivo, chevrolet, camaro).
auto(hatchback, chevrolet, aveo).    

elizaFindAuto( R):- findall(Tipo, (auto(Tipo, _, _)), ListaTipos), list_to_set(ListaTipos, TiposUnicos),
                    R = ['Claro', que, tipo, de, auto, es, TiposUnicos].

elizaFindDeportivos( R):- findall([Marca, Modelo], auto(deportivo, Marca, Modelo), Deportivos),
                    R = ['Claro',  estos, son, los, autos, deportivos, Deportivos].

elizaFindMazda( R):- findall(Modelo, auto(_, mazda, Modelo), Mazdas),
                    R = ['Claro',  estos, son, los, autos, mazda, Mazdas].
                
elizaFindToyota( R):- findall(Modelo, auto(_, toyota, Modelo), Toyotas),
                    R = ['Claro',  estos, son, los, autos, toyota, Toyotas].

elizaFindHonda( R):- findall(Modelo, auto(_, honda, Modelo), Hondas),
                    R = ['Claro',  estos, son, los, autos, honda, Hondas].

elizaFindNissan( R):- findall(Modelo, auto(_, nissan, Modelo), Nissans),
                    R = ['Claro',  estos, son, los, autos, nissan, Nissans].

elizaFindChevrolet( R):- findall(Modelo, auto(_, chevrolet, Modelo), Chevrolets),
                    R = ['Claro',  estos, son, los, autos, chevrolet, Chevrolets].

elizaFindMarca(X,R) :- 
    findall(Marca, (auto(_, Marca, _)), ListaMarcas), 
    list_to_set(ListaMarcas, MarcasUnicas),
    R = ['Y', que, marca, de, auto, es, MarcasUnicas, escribe, el, tipo,tambien].

elizaFindAutoFinal(Marca, Tipo, R) :- 
    (auto(Tipo, Marca, Modelo) -> 
        R = ['Encontre tu auto:', Modelo]
    ;
        R = ['Lo siento, no encontré un auto que coincida con tu búsqueda.']).



template(_, ['Please', explain, a, little, more, '.'], []). 
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Yeah', i, like, X].
elizaLikes(X, R):- \+likes(X), R = ['Nope', i, do, not, like, X].
likes(apples).
likes(ponies).
likes(zombies).
likes(manzanas).
likes(computadoras).
likes(carros).

elizaLikesNew(_, R) :-
    findall(["A Eliza le gusta ", Gusto], likes(Gusto), Results), 
    flatten(Results, R). 
    


% lo que hace eliza: flagDo
elizaDoes(X, R):- does(X), R = ['Yes', i, X, and, i, love, it].
elizaDoes(X, R):- \+does(X), R = ['No', i, do, not, X ,'.', it, is, too, hard, for, me].
does(study).
does(cook).
does(work).

% lo que es eliza: flagIs
elizaIs(X, R):- is0(X), R = ['Yes', yo, soy, X].
elizaIs(X, R):- \+is0(X), R = ['No', i, am, not, X].
is0(dumb).
is0(weird).
is0(nice).
is0(fine).
is0(happy).
is0(redundant).

% Predicado para responder dónde vive Eliza
elizaLives(X, R):- livesIn(X), R = ['Yes', yo, vivo, en, X].
elizaLives(X, R):- \+livesIn(X), R = ['No', yo, no, vivo, en, X].
livesIn(puruandiro).
livesIn(nuevaYork).
livesIn(mexico).

elizaWatch(X, R):- watch(X), R = ['Yes', me ,gustan, las, peliculas, de, X].
elizaWatch(X, R):- \+watch(X), R = ['No', me ,gustan, las, peliculas, de, X].
watch(terror).
watch(anime).
watch(romance).
watch(comedia).

elizaVisit(X, R):- visit(X), R = ['Yes', he,ido ,a, X].
elizaVisit(X, R):- \+visit(X), R = ['No', he, ido, a, X].
visit(alemania).
visit(peru).
visit(argentina).

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
	atom(S), % si I es un s(X) devuelve falso
	S == I,
	match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
% I es un s(X), lo ignoro y continúo con el resto de la lista
	\+atom(S),
	match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

% Eliza likes:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagLike,
	elizaLikes(Atom, R).

	
% Eliza likesNew:
% Reemplazo en la respuesta
replace0([], _, _, Resp, Resp).
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, _), % No es relevante el contenido del token aquí
    Resp = [flagLikesNew | _],
    elizaLikesNew(_, R), !.


% Eliza does:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagDo,
	elizaDoes(Atom, R).

% Eliza is:
replace0([I|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagIs,
	elizaIs(Atom, R).

% ELiza lives:
replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),        
    nth0(0, Resp, X),  
	X == flagWhere,
	elizaLives(Atom, R).      

% Eliza watch

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),        
    nth0(0, Resp, X),  
	X == flagWatch,
	elizaWatch(Atom, R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),        
    nth0(0, Resp, X),  
	X == flagVisit,
	elizaVisit(Atom, R).

% Manejo específico para flagfather
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagfather | _], 
    ( padre(Padre, Atom) -> 
        format(atom(R), 'El padre de ~w es ~w.', [Atom, Padre]) % Respuesta formateada
    ; 
        format(atom(R), 'Lo siento, no se quien es el padre de ~w.', [Atom]) % Caso donde no hay datos
    ), !.

% Manejo específico para flagmother
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom), 
    Resp = [flagmother | _],
     ( madre(Madre, Atom) -> 
        format(atom(R), 'La madre de ~w es ~w.', [Atom, Madre]) % Respuesta formateada
    ; 
        format(atom(R), 'Lo siento, no se quien es la madre de ~w.', [Atom]) % Caso donde no hay datos
    ), !.

% Manejo específico para hermanos
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom), % Obtén el nombre de la persona
    Resp = [flagsiblings | _], 
    findall(Hermano, hermanos(Atom, Hermano), ListaHermanos), % Obtén todos los hermanos
    ( ListaHermanos \= [] -> 
        atomic_list_concat(ListaHermanos, ', ', ListaHermanosStr), % Convierte la lista a cadena
        format(atom(R), 'Los hermanos de ~w son: ~w.', [Atom, ListaHermanosStr]) % Respuesta formateada
    ; 
        format(atom(R), 'Lo siento, no se quienes son los hermanos de ~w.', [Atom]) % Caso donde no hay datos
    ), !.

% Manejo específico para primos
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom), % Obtén el nombre de la persona
    Resp = [flagcousins | _], % Verifica que sea la bandera correcta
    findall(Primo, primos(Atom, Primo), ListaPrimos), % Obtén todos los primos
    ( ListaPrimos \= [] -> 
        atomic_list_concat(ListaPrimos, ', ', ListaPrimosStr), % Convierte la lista a cadena
        format(atom(R), 'Los primos de ~w son: ~w.', [Atom, ListaPrimosStr]) % Respuesta formateada
    ; 
        format(atom(R), 'Lo siento, no sé quiénes son los primos de ~w.', [Atom]) % Caso donde no hay datos
    ), !.

% Manejo específico para cada relación
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flaggrandfather | _],
    ( abuelo(Abuelo, Atom) ->
        format(atom(R), 'El abuelo de ~w es ~w.', [Atom, Abuelo])
    ;
        format(atom(R), 'Lo siento, no se quien es el abuelo de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flaggrandmother | _],
    ( abuela(Abuela, Atom) ->
        format(atom(R), 'La abuela de ~w es ~w.', [Atom, Abuela])
    ;
        format(atom(R), 'Lo siento, no se quien es la abuela de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flaguncles | _],
    findall(Tio, tio(Tio, Atom), ListaTios),
    ( ListaTios \= [] ->
        atomic_list_concat(ListaTios, ', ', ListaTiosStr),
        format(atom(R), 'Los tios de ~w son: ~w.', [Atom, ListaTiosStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son los tios de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagaunts | _],
    findall(Tia, tia(Tia, Atom), ListaTias),
    ( ListaTias \= [] ->
        atomic_list_concat(ListaTias, ', ', ListaTiasStr),
        format(atom(R), 'Las tias de ~w son: ~w.', [Atom, ListaTiasStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son las tias de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flaggrandsons | _],
    findall(Nieto, nieto(Nieto, Atom), ListaNietos),
    ( ListaNietos \= [] ->
        atomic_list_concat(ListaNietos, ', ', ListaNietosStr),
        format(atom(R), 'Los nietos de ~w son: ~w.', [Atom, ListaNietosStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son los nietos de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flaggranddaughters | _],
    findall(Nieta, nieta(Nieta, Atom), ListaNietas),
    ( ListaNietas \= [] ->
        atomic_list_concat(ListaNietas, ', ', ListaNietasStr),
        format(atom(R), 'Las nietas de ~w son: ~w.', [Atom, ListaNietasStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son las nietas de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagbrotherinlaw | _],
    findall(BrotherInLaw, flagbrotherinlaw(Atom, BrotherInLaw), ListaHermanosPoliticos),
    ( ListaHermanosPoliticos \= [] ->
        atomic_list_concat(ListaHermanosPoliticos, ', ', ListaHermanosPoliticosStr),
        format(atom(R), 'Los hermanos políticos de ~w son: ~w.', [Atom, ListaHermanosPoliticosStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son los hermanos políticos de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagsisterinlaw | _],
    findall(SisterInLaw, flagsisterinlaw(Atom, SisterInLaw), ListaHermanasPoliticas),
    ( ListaHermanasPoliticas \= [] ->
        atomic_list_concat(ListaHermanasPoliticas, ', ', ListaHermanasPoliticasStr),
        format(atom(R), 'Las hermanas políticas de ~w son: ~w.', [Atom, ListaHermanasPoliticasStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son las hermanas políticas de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagnephews | _],
    findall(Nephew, flagnephews(Atom, Nephew), ListaSobrinos),
    ( ListaSobrinos \= [] ->
        atomic_list_concat(ListaSobrinos, ', ', ListaSobrinosStr),
        format(atom(R), 'Los sobrinos de ~w son: ~w.', [Atom, ListaSobrinosStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son los sobrinos de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagnieces | _],
    findall(Niece, flagnieces(Atom, Niece), ListaSobrinas),
    ( ListaSobrinas \= [] ->
        atomic_list_concat(ListaSobrinas, ', ', ListaSobrinasStr),
        format(atom(R), 'Las sobrinas de ~w son: ~w.', [Atom, ListaSobrinasStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son las sobrinas de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagson | _],
    findall(Son, flagson(Atom, Son), ListaHijos),
    ( ListaHijos \= [] ->
        atomic_list_concat(ListaHijos, ', ', ListaHijosStr),
        format(atom(R), 'Los hijos de ~w son: ~w.', [Atom, ListaHijosStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son los hijos de ~w.', [Atom])
    ), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagdaughter | _],
    findall(Daughter, flagdaughter(Atom, Daughter), ListaHijas),
    ( ListaHijas \= [] ->
        atomic_list_concat(ListaHijas, ', ', ListaHijasStr),
        format(atom(R), 'Las hijas de ~w son: ~w.', [Atom, ListaHijasStr])
    ;
        format(atom(R), 'Lo siento, no se quienes son las hijas de ~w.', [Atom])
    ), !.


replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagmother | _], madre(Atom, R), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagsiblings | _], findall(X, hermanos(Atom, X), R), !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagcousins | _], findall(X, primos(Atom, X), R), !.
	
replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindAuto,
    elizaFindAuto(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindDeportivos,
    elizaFindDeportivos(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindMazda,
    elizaFindMazda(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindToyota,
    elizaFindToyota(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindHonda,
    elizaFindHonda(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindNissan,
    elizaFindNissan(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),  
    X == flagFindChevrolet,
    elizaFindChevrolet(R).

replace0([I|_], Input, _, Resp, R):- 
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagFindMarca,
    elizaFindMarca(Atom,R).

replace0([I|_], [Marca, Tipo], _, Resp, R):- 
    nth0(I, Resp, X),
    X == flagFindAutoFinal,         % Verifica si la bandera es flagFindAutoFinal
    elizaFindAutoFinal(Marca, Tipo, R).

replace0([I|Index], Input, N, Resp, R):-
	length(Index, M), M =:= 0,
	nth0(I, Input, Atom),
	select(N, Resp, Atom, R1), append(R1, [], R),!.

replace0([I|Index], Input, N, Resp, R):-
	nth0(I, Input, Atom),
	length(Index, M), M > 0,
	select(N, Resp, Atom, R1),
	N1 is N + 1,
	replace0(Index, Input, N1, R1, R),!.

