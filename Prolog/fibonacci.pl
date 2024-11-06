% Caso base: el primer número es 0
fibonacci(0, 0).
% Segundo caso base
fibonacci(1, 1).

% Caso recursivo: el número de Fibonacci de N es la suma de los dos anteriores
fibonacci(N, F) :-
    N > 1,
