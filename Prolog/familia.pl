% Hechos
hombre(francisco).
hombre(francisco_jr).
hombre(brayan).
hombre(gumercindo).
mujer(paola).
mujer(blanca).
mujer(karina).
mujer(valeria).

padre(francisco, francisco_jr).
padre(francisco, brayan).
padre(gumercindo, paola).
padre(gumercindo, karina).
madre(paola, francisco_jr).
madre(paola, brayan).
madre(karina, valeria).
madre(blanca, paola).
madre(blanca, karina).


% Regla para abuelos
abuelo(X, Y) :- padre(X, Z), padres(Z, Y).
abuela(X, Y) :- madre(X, Z), padres(Z, Y).

% Regla para padres 
padres(X, Y) :- padre(X, Y); madre(X, Y).

% Regla modificada para hermanos
hermanos(X, Y) :- (padre(Z, X), padre(Z, Y); madre(W, X), madre(W, Y)), X \= Y.

% Regla para primos
primos(X, Y) :- padres(A, X), padres(B, Y), hermanos(A, B).