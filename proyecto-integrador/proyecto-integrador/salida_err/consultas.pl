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
