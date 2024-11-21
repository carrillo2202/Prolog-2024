eliza :-
    writeln('Hola, mi nombre es Eliza, tu chatbot.'),
    writeln('Por favor, ingresa tu consulta (usar solo minúsculas sin punto al final):'),
    read_line_as_list(Input),
    eliza(Input), !.


%% Caso de salida: el usuario dice "adios"
eliza(Input) :-
    maplist(downcase_atom, Input, LowerInput),
    LowerInput = ['adios'],
    writeln('Adios. Espero poder haberte ayudado.'), !.


% Respuesta por defecto
eliza(Input) :-
    template(Stim, Resp, IndStim),
    match(Stim, Input),
    % Si se encuentra un template, procesar la respuesta
    replace0(IndStim, Input, 0, Resp, R),
    writeln(R),
    read_line_as_list(Input1),
    eliza(Input1), !.


% Templates definidos
template([eliza, quien, es, el, hijo, de, s(_)], [flagfam], [6]).
template(_, ['Por favor, explica un poco más.'], []).


% Base de datos
padre(juan, luis).
padre(juan, antonio).
padre(luis, carlos).


familia(X, R) :-
    findall(["El hijo de ", X, " es ", Y], padre(X, Y), Results),
    flatten(Results, R).


% Coincidencias
match([], []).
match([S|Stim], [I|Input]) :-
    atom(S), S == I,
    match(Stim, Input), !.
match([S|Stim], [_|Input]) :-
    \+atom(S),
    match(Stim, Input), !.


% Reemplazo en la respuesta
replace0([], _, _, Resp, Resp).
replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    Resp = [flagfam | _],
    familia(Atom, R), !.
replace0([I|Index], Input, N, Resp, R) :-
    nth0(I, Input, Atom),
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R), !.


% Lectura de línea como lista
read_line_as_list(Input) :-
    read_line_to_string(user_input, String),
    split_string(String, " ", "", InputList),
    maplist(atom_string, Input, InputList).
