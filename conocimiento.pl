:- encoding(utf8).

% ==============================================================
% conocimiento.pl
% Base de conocimiento generada automáticamente por el pipeline
% Haskell -> Prolog -> UI. NO editar a mano: se regenera desde
% problema.txt. Dominio: académico
% ==============================================================

:- dynamic persona/1, curso/1, docente/1, lleva/2, imparte/2, aprobo/2.

% ---------------------------- HECHOS ----------------------------

% linea 5: persona: Ana
persona(ana).

% linea 6: persona: Luis
persona(luis).

% linea 7: persona: María José
persona(maria_jose).

% linea 8: persona: Carlos
persona(carlos).

% linea 9: curso: Paradigmas
curso(paradigmas).

% linea 10: curso: Bases de Datos
curso(bases_de_datos).

% linea 11: curso: Redes
curso(redes).

% linea 12: docente: Maria
docente(maria).

% linea 13: docente: Pedro
docente(pedro).

% linea 16: lleva: Ana, Paradigmas
lleva(ana, paradigmas).

% linea 17: lleva: Luis, Paradigmas
lleva(luis, paradigmas).

% linea 18: lleva: María José, Paradigmas
lleva(maria_jose, paradigmas).

% linea 19: lleva: Ana, Bases de Datos
lleva(ana, bases_de_datos).

% linea 20: lleva: Carlos, Bases de Datos
lleva(carlos, bases_de_datos).

% linea 21: lleva: Luis, Redes
lleva(luis, redes).

% linea 24: imparte: Maria, Paradigmas
imparte(maria, paradigmas).

% linea 25: imparte: Pedro, Bases de Datos
imparte(pedro, bases_de_datos).

% linea 22: aprobo: Ana, Paradigmas
aprobo(ana, paradigmas).

% linea 23: aprobo: Luis, Paradigmas
aprobo(luis, paradigmas).

% ---------------------------- REGLAS ----------------------------

% linea 28: companeros
% Dos personas distintas son compañeros si llevan el mismo curso.
companero(X, Y) :-
    lleva(X, Curso),
    lleva(Y, Curso),
    X \= Y.

% linea 29: curso_con_docente
% Relaciona cada curso con el docente que lo imparte.
curso_con_docente(Curso, Docente) :-
    curso(Curso),
    imparte(Docente, Curso).

% linea 30: docente_de_estudiante
% Un docente lo es de una persona si imparte un curso que ella lleva.
docente_de(Persona, Docente) :-
    lleva(Persona, Curso),
    imparte(Docente, Curso).

% linea 31: curso_sin_docente
% Cursos declarados que ningún docente imparte.
curso_sin_docente(Curso) :-
    curso(Curso),
    \+ imparte(_, Curso).

% linea 32: aprobacion
% Una persona aprobó un curso que lleva; si no, queda pendiente.
aprobado(Persona, Curso) :-
    lleva(Persona, Curso),
    aprobo(Persona, Curso).

pendiente(Persona, Curso) :-
    lleva(Persona, Curso),
    \+ aprobo(Persona, Curso).

