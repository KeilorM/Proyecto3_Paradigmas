:- encoding(utf8).
% ==============================================================
% pruebas.pl - Pruebas de la base de conocimiento generada.
%
% Verifican hechos y reglas para el problema.txt de ejemplo del
% repositorio. Uso (desde la raiz del proyecto, tras generar salida/):
%
%     swipl -q test/pruebas.pl
%
% Termina con codigo 0 si todo pasa y 1 si falla alguna prueba.
% ==============================================================

:- initialization(main, main).
:- consult('../salida/conocimiento.pl').
:- dynamic falla/0.

prueba(Nombre, Meta) :-
    (   catch(Meta, Error, (print_message(error, Error), fail))
    ->  format("  [OK]    ~w~n", [Nombre])
    ;   format("  [FALLA] ~w~n", [Nombre]),
        assertz(falla)
    ).

cuantos(Meta, N) :- findall(x, Meta, L), length(L, N).

main :-
    % --- hechos
    prueba('hecho persona(ana)',            persona(ana)),
    prueba('hecho atomizado maria_jose',    persona(maria_jose)),
    prueba('hecho curso bases_de_datos',    curso(bases_de_datos)),
    prueba('hecho lleva(ana, paradigmas)',  lleva(ana, paradigmas)),
    prueba('hecho imparte(maria, paradigmas)', imparte(maria, paradigmas)),
    prueba('no hay hechos inventados',      \+ lleva(carlos, paradigmas)),
    prueba('total de personas = 4',         cuantos(persona(_), 4)),
    prueba('total de relaciones lleva = 6', cuantos(lleva(_, _), 6)),
    % --- companeros
    prueba('companero(ana, luis)',          companero(ana, luis)),
    prueba('companero es simetrico',        companero(luis, ana)),
    prueba('nadie es companero de si mismo', \+ companero(ana, ana)),
    prueba('companeros por otro curso',    companero(ana, carlos)),
    prueba('sin curso en comun no hay',    \+ companero(luis, carlos)),
    prueba('total de pares companero = 8',  cuantos(companero(_, _), 8)),
    % --- curso_con_docente / docente_de
    prueba('curso_con_docente(paradigmas, maria)', curso_con_docente(paradigmas, maria)),
    prueba('curso sin docente no aparece', \+ curso_con_docente(redes, _)),
    prueba('docente_de(ana, pedro)',        docente_de(ana, pedro)),
    prueba('docente_de(ana, maria)',        docente_de(ana, maria)),
    prueba('docente_de(luis, pedro) es falso', \+ docente_de(luis, pedro)),
    % --- curso_sin_docente
    prueba('curso_sin_docente(redes)',      curso_sin_docente(redes)),
    prueba('paradigmas tiene docente',      \+ curso_sin_docente(paradigmas)),
    % --- aprobacion
    prueba('aprobado(ana, paradigmas)',     aprobado(ana, paradigmas)),
    prueba('pendiente(ana, bases_de_datos)', pendiente(ana, bases_de_datos)),
    prueba('aprobado excluye pendiente',    \+ pendiente(ana, paradigmas)),
    prueba('total aprobados = 2',           cuantos(aprobado(_, _), 2)),
    prueba('total pendientes = 4',          cuantos(pendiente(_, _), 4)),
    (   falla
    ->  format("~nHubo pruebas fallidas.~n"), halt(1)
    ;   format("~nTodas las pruebas Prolog pasaron.~n"), halt(0)
    ).
