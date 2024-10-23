% Hechos
hombre(francisco).
hombre(francisco_jr).
hombre(brayan).
hombre(gumercindo).
mujer(paola).
mujer(blanca).

padre(francisco, francisco_jr).
padre(francisco, brayan).
madre(paola, francisco_jr).
madre(paola, brayan).

padrede(gumercindo, paola).
madrede(blanca, paola).

% Regla para abuelos
abuelo(X, Y) :- padrede(X, Z), padres(Z, Y).
abuela(X, Y) :- madrede(X, Z), padres(Z, Y).

% Regla para padres 
padres(X, Y) :- padre(X, Y); madre(X, Y).

