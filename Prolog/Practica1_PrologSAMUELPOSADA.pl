% Practica 1 - Parte B: Paradigma Logico (Prolog / SWI-Prolog)
% SI1002 Lenguajes y Paradigmas de Computacion - 2026-2

% 1. Divisores propios y suma aliquota

% es_divisor_propio(+Numero, ?Divisor)
% Genera por backtracking los divisores propios de Numero.
es_divisor_propio(Numero, Divisor) :-
    Numero > 1,
    Limite is Numero // 2,
    between(1, Limite, Divisor),
    0 is Numero mod Divisor.

% suma_aliquota(+Numero, -Suma)
% Recolecta las soluciones de es_divisor_propio/2 con findall/3.
suma_aliquota(Numero, Suma) :-
    findall(
        Divisor,
        es_divisor_propio(Numero, Divisor),
        Divisores
    ),
    sum_list(Divisores, Suma).

% 2. Clasificacion de Nicomaco

categoria(CategoriaNum, 'Administrative') :-
    suma_aliquota(CategoriaNum, Suma),
    Suma > CategoriaNum.

categoria(CategoriaNum, 'Engineering') :-
    suma_aliquota(CategoriaNum, Suma),
    Suma =:= CategoriaNum.

categoria(CategoriaNum, 'Humanities') :-
    suma_aliquota(CategoriaNum, Suma),
    Suma < CategoriaNum.

% 3. Periodos validos

periodo_valido(262).
periodo_valido(271).
periodo_valido(272).
periodo_valido(281).
periodo_valido(282).
periodo_valido(291).
periodo_valido(292).

% 4. Paridad

paridad(Numero, even) :-
    0 is Numero mod 2.

paridad(Numero, odd) :-
    1 is Numero mod 2.

% 5. Descomposicion y validacion del codigo

componentes_codigo(Codigo, Periodo, CategoriaNum, Consecutivo) :-
    integer(Codigo),
    Codigo >= 10000000,
    Codigo =< 99999999,

    Periodo is Codigo // 100000,
    periodo_valido(Periodo),

    CategoriaNum is (Codigo // 1000) mod 100,
    CategoriaNum >= 1,
    CategoriaNum =< 99,

    Consecutivo is Codigo mod 1000,
    Consecutivo >= 1,
    Consecutivo =< 999.

% 6. Predicado principal

student_id(Codigo, Output) :-
    componentes_codigo(Codigo, Periodo, CategoriaNum, Consecutivo),

    Anio is 2000 + Periodo // 10,
    Semestre is Periodo mod 10,

    categoria(CategoriaNum, Categoria),
    paridad(Codigo, Parity),

    format(
        string(Output),
        "~w-~w ~w num~w ~w",
        [Anio, Semestre, Categoria, Consecutivo, Parity]
    ).
    
% 7. Generacion de codigos

generar_codigo(Periodo, CategoriaNum, Consecutivo, Codigo) :-
    periodo_valido(Periodo),
    between(1, 99, CategoriaNum),
    between(1, 999, Consecutivo),
    Codigo is Periodo * 100000
             + CategoriaNum * 1000
             + Consecutivo.

generar_codigos_engineering_2029_2(Codigos) :-
    findall(
        Codigo,
        (
            generar_codigo(292, CategoriaNum, Consecutivo, Codigo),
            categoria(CategoriaNum, 'Engineering')
        ),
        Codigos
    ).

% 8. Casos de prueba

casos_de_prueba([
    26276002 - "2026-2 Humanities num2 even",
    27128112 - "2027-1 Engineering num112 even",
    27206025 - "2027-2 Engineering num25 odd",
    28124236 - "2028-1 Administrative num236 even",
    28299115 - "2028-2 Humanities num115 odd"
]).

verificar :-
    casos_de_prueba(Casos),
    forall(
        member(Codigo-Esperado, Casos),
        (
            student_id(Codigo, Obtenido)
            -> (
                Obtenido == Esperado
                -> format("OK    ~w -> ~w~n", [Codigo, Obtenido])
                ; format("FALLO ~w -> obtenido=~w esperado=~w~n",
                         [Codigo, Obtenido, Esperado])
            )
            ; format("FALLO ~w -> student_id no tuvo exito~n", [Codigo])
        )
    ).

codigos_invalidos([
    26200001,
    26201000,
    26301001,
    29301001,
    30201001,
    1234567
]).

verificar_rechazos :-
    codigos_invalidos(Invalidos),
    forall(
        member(Codigo, Invalidos),
        (
            \+ student_id(Codigo, _)
            -> format("OK    ~w correctamente rechazado~n", [Codigo])
            ; format("FALLO ~w NO fue rechazado~n", [Codigo])
        )
    ).

% 9. Ejecucion de pruebas y arranque automatico en OnlineGDB

main :-
    % Imprime la version de SWI-Prolog del servidor de OnlineGDB
    current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
    format("Version de SWI-Prolog: ~w.~w.~w~n~n", [Major, Minor, Patch]),

    format("== Casos de prueba de la guia ==~n"),
    verificar,
    format("~n== Rechazo de codigos invalidos ==~n"),
    verificar_rechazos,
    format("~n== Clasificacion Engineering entre 1 y 99 ==~n"),
    findall(N, (between(1, 99, N), categoria(N, 'Engineering')), Categorias),
    writeln(Categorias),
    format("~n== Modo generate: Engineering en 2029-2 ==~n"),
    generar_codigos_engineering_2029_2(Codigos),
    length(Codigos, Total),
    nth1(1, Codigos, Primero),
    last(Codigos, Ultimo),
    format("Total generado: ~w~n", [Total]),
    format("Primero: ~w~n", [Primero]),
    format("Ultimo: ~w~n~n", [Ultimo]),
    % Finaliza la ejecucion en OnlineGDB
    halt.

% Esta directiva le ordena a SWI-Prolog ejecutar 'main' automaticamente
:- initialization(main).
