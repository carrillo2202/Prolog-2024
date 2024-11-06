% Cada casa tiene las características: (Color, Nacionalidad, Bebida, Cigarro, Mascota)
solucion(Casas) :-
    Casas = [_, _, (CasaCentroColor, _, leche, _, _), _, _], % La casa del centro tiene leche.
    Casas = [(noruegoCasaColor, noruego, _, _, _), _, _, _, _], % El noruego vive en la primera casa.
    miembro((roja, britanico, _, _, _), Casas), % El británico vive en la casa roja.
    miembro((_, sueco, _, _, perros), Casas), % El sueco tiene perros.
    miembro((_, danes, te, _, _), Casas), % El danés bebe té.
    al_lado((verde, _, _, _, _), (blanca, _, _, _, _), Casas), % La casa verde está a la izquierda de la casa blanca.
    miembro((_, _, _, 'Pall Mall', pajaros), Casas), % Quien fuma Pall Mall tiene pájaros.
    miembro((amarilla, _, _, dunhill, _), Casas), % El dueño de la casa amarilla fuma Dunhill.
    miembro((verde, _, cafe, _, _), Casas), % El dueño de la casa verde toma café.
    al_lado((_, _, _, blends, _), (_, _, _, _, gatos), Casas), % Quien fuma Blends vive al lado de quien tiene gatos.
    al_lado((_, _, _, dunhill, _), (_, _, _, _, caballos), Casas), % Quien cuida caballos vive al lado de quien fuma Dunhill.
    miembro((_, _, cerveza, 'BlueMaster', _), Casas), % Quien fuma BlueMaster bebe cerveza.
    miembro((_, aleman, _, principes, _), Casas), % El alemán fuma Príncipes.
    al_lado((noruegoCasaColor, noruego, _, _, _), (azul, _, _, _, _), Casas), % El noruego vive junto a la casa azul.
    al_lado((_, _, _, blends, _), (_, _, agua, _, _), Casas), % Quien fuma Blends vive junto a quien bebe agua.

    % Definiciones auxiliares para evitar duplicados de características y garantizar unicidad
    % Cada casa tiene un color, nacionalidad, bebida, cigarro y mascota diferentes
    casa_unica(Casas).

% Regla para definir si algo está al lado de otra cosa en una lista
al_lado(A, B, Lista) :- nextto(A, B, Lista).
al_lado(A, B, Lista) :- nextto(B, A, Lista).

% Reglas para garantizar que todos los valores sean únicos en cada categoría
casa_unica([]).
casa_unica([H|T]) :- not(member(H, T)), casa_unica(T).

% Permite comprobar que un elemento es miembro de una lista
miembro(X, [X|_]).
miembro(X, [_|T]) :- miembro(X, T).

% Consulta para encontrar quién es el dueño del pez
dueño_del_pez(Nacionalidad) :-
    solucion(Casas),
    miembro((_, Nacionalidad, _, _, pez), Casas).
