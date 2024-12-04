#+begin_src prolog
lista([]).
aplanar(lista, R) :-
    flatten(lista, Resp). 



% EJercicio 1
flatten([1,[2,[3,4],5],[6]], R).

% EJercicio 2
permutacion([], []).
permutacion([H|T], P) :-
    permutacion(T, R),
    insertar(H, R, P).

insertar(X, L, [X|L]).
insertar(X, [H|T], [H|R]) :- insertar(X, T, R).

% EJercicio 3

% EJercicio 4
#+end_src
