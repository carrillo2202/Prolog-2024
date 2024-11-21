% Caso base: el primer número es 0
fibonacci(0, 0).
% Segundo caso base
fibonacci(1, 1).

% Caso recursivo: el número de Fibonacci de N es la suma de los dos anteriores
fibonacci(N, F) :-
    N > 1,
     N1 is N - 1, 
    N2 is N - 2, 
    fibonacci(N1, F1), 
    fibonacci(N2, F2), 
    F is F1 + F2. 

% Caso base: si el dividendo es menor que el divisor, el cociente es 0 y el residuo es el dividendo
division(Dividendo, Divisor, 0, Dividendo) :-
    Dividendo < Divisor.

% Caso recursivo: restamos el divisor del dividendo y contamos cuántas veces se puede hacer
division(Dividendo, Divisor, Cociente, Residuo) :-
    Dividendo >= Divisor,
    NuevoDividendo is Dividendo - Divisor, % Restamos el divisor del dividendo
    division(NuevoDividendo, Divisor, CocientePrevio, Residuo), % Llamada recursiva
    Cociente is CocientePrevio + 1. % Incrementamos el cociente

