:- encoding(utf8).

% ==============================================================
% consultas.pl
% Consultas sugeridas, generadas automáticamente.
%
% Uso:
%   swipl consultas.pl
%   ?- ejecutar_consultas.          % ejecuta todas
%   ?- consulta(Descripcion, Meta). % lista las sugeridas
% ==============================================================

:- style_check(-singleton).
:- consult('conocimiento.pl').

% consulta(+Descripcion, +Meta)
consulta('Listar personas', persona(X)).
consulta('Listar cursos', curso(X)).
consulta('Listar docentes', docente(X)).
consulta('Listar relaciones «lleva»', lleva(X, Y)).
consulta('Listar relaciones «imparte»', imparte(X, Y)).
consulta('Listar relaciones «aprobo»', aprobo(X, Y)).
consulta('Todos los pares de compañeros', companero(X, Y)).
consulta('Compañeros de Ana', companero(ana, X)).
consulta('Cursos con su docente', curso_con_docente(Curso, Docente)).
consulta('Docente del curso Paradigmas', curso_con_docente(paradigmas, Docente)).
consulta('Docente de cada persona', docente_de(Persona, Docente)).
consulta('Docentes de Ana', docente_de(ana, Docente)).
consulta('Cursos sin docente asignado', curso_sin_docente(Curso)).
consulta('Cursos aprobados', aprobado(Persona, Curso)).
consulta('Cursos pendientes', pendiente(Persona, Curso)).

% Ejecuta cada consulta sugerida e imprime todas sus soluciones.
ejecutar_consultas :-
    forall(
        consulta(Descripcion, Meta),
        (   format("~n~w~n", [Descripcion]),
            findall(Meta, Meta, Soluciones),
            (   Soluciones == []
            ->  format("  (sin resultados)~n")
            ;   forall(member(S, Soluciones), format("  ~q~n", [S]))
            )
        )
    ).
