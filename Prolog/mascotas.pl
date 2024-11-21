% Definimos los hermanos y las mascotas posibles
hermano(ana).
hermano(bruno).
hermano(carla).
hermano(daniel).

mascota(perro).
mascota(gato).
mascota(loro).
mascota(pez).

% Regla que establece que cada hermano tiene una mascota única
tiene_mascota(Hermano, Mascota) :-
    hermano(Hermano),
    mascota(Mascota).

% Restricciones específicas según las pistas
restricciones :-
    tiene_mascota(ana, MascotaAna),
    MascotaAna \= perro,
    MascotaAna \= pez,

    tiene_mascota(bruno, MascotaBruno),
    MascotaBruno \= gato,

    tiene_mascota(carla, MascotaCarla),
    MascotaCarla \= pez,

    tiene_mascota(daniel, MascotaDaniel),
    (MascotaDaniel = loro ; MascotaDaniel = pez).

% Regla para asegurar que todos tienen mascotas distintas
todos_diferentes(MascotaAna, MascotaBruno, MascotaCarla, MascotaDaniel) :-
    MascotaAna \= MascotaBruno,
    MascotaAna \= MascotaCarla,
    MascotaAna \= MascotaDaniel,
    MascotaBruno \= MascotaCarla,
    MascotaBruno \= MascotaDaniel,
    MascotaCarla \= MascotaDaniel.

% Mostrar resultados
mostrar_resultados(MascotaAna, MascotaBruno, MascotaCarla, MascotaDaniel) :-
    format('Ana tiene un ~w~n', [MascotaAna]),
    format('Bruno tiene un ~w~n', [MascotaBruno]),
    format('Carla tiene un ~w~n', [MascotaCarla]),
    format('Daniel tiene un ~w~n', [MascotaDaniel]).

% Regla principal que resuelve el problema
resolver :-
    tiene_mascota(ana, MascotaAna),
    tiene_mascota(bruno, MascotaBruno),
    tiene_mascota(carla, MascotaCarla),
    tiene_mascota(daniel, MascotaDaniel),
    restricciones,
    todos_diferentes(MascotaAna, MascotaBruno, MascotaCarla, MascotaDaniel),
    mostrar_resultados(MascotaAna, MascotaBruno, MascotaCarla, MascotaDaniel).