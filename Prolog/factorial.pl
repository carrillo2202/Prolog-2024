#+begin_src prolog
% caso base: el factorial de 0 es 1
factorial(0, 1).

% Caso recursivo factorial de N es N *factorial de (N-1)
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.
    
#+end_src