% Hechos: los hermanos y sus posibles mascotas
hermano(ana).
hermano(bruno).
hermano(carla).
hermano(daniel).

mascota(perro).
mascota(gato).
mascota(loro).
mascota(pez).

% Regla para asignar una mascota a cada quien
asignacion(Hermano, Mascota) :-
    hermano(Hermano),
    mascota(Mascota).

% Restricciones basadas en las pistas
solucion(Asignaciones) :-
    Asignaciones = [
        asignacion(ana, MascotaAna),
        asignacion(bruno, MascotaBruno),
        
        asignacion(carla, MascotaCarla),
        asignacion(daniel, MascotaDaniel)
    ],
    
    % Cada hermano tiene una mascota diferente
    Mascotas = [MascotaAna, MascotaBruno, MascotaCarla, MascotaDaniel],
    all_different(Mascotas),

    % Pistas dadas en el problema
    MascotaAna \= perro,
    MascotaAna \= pez,
    MascotaBruno \= gato,
    MascotaCarla \= pez,
    (MascotaDaniel = loro; MascotaDaniel = pez).

