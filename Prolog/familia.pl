% Hechos
hombre(francisco).
hombre(brayan).
hombre(gumercindo).
mujer(paola).
mujer(blanca).

padre(francisco, francisco_jr).
padre(francisco, brayan).
madre(paola, francisco_jr).
madre(paola, brayan).

% abuelos
padre(gumercindo, paola).
madre(blanca, paola).

% Regla para abuelos
abuelo(X, Y) :- padre(X, Z), padres(Z, Y).
abuela(X, Y) :- madre(X, Z), padres(Z, Y).

% Regla para padres 
padres(X, Y) :- padre(X, Y); madre(X, Y).

% Consultas de ejemplo:
% - hombre(francisco).
% - padre(francisco, brayan).
% - abuelo(francisco, francisco_jr).
