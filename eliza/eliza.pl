
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
    findall(Gusto, likes(Gusto), Gustos), % Obtiene todos los gustos de la base
    atomic_list_concat(Gustos, ', ', GustosConcat), % Combina los gustos en una cadena
    atomic_list_concat(['Me gustan: ', GustosConcat, '.'], '', R). % Genera el mensaje



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
