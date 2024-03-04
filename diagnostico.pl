:- use_module(library(plunit)).

:- include('doencas.pl').


% age_type(+Age, ?Type) is semidet
% é true se Type corresponde à faixa estária determinada por Age:
% crianca < 12 = jovem < 30 = adulto < 60 = idoso < inf.
:- begin_tests(agetype).
test(t0) :- age_type(0, crianca).
test(t1) :- age_type(11, crianca).
test(t2) :- age_type(12, jovem).
test(t3) :- age_type(29, jovem).
test(t4) :- age_type(30, adulto).
test(t5) :- age_type(59, adulto).
test(t6) :- age_type(60, idoso).
test(t7) :- age_type(100, idoso).
test(t8, fail) :- age_type(1000, teste).
:- end_tests(agetype).

age_type(Age, crianca) :- Age < 12, !.
age_type(Age, jovem) :- Age >= 12, Age < 30, !.
age_type(Age, adulto) :- Age >= 30, Age < 60, !.
age_type(Age, idoso) :- Age >= 60, !.


likely(Chance, NewChance) :- NewChance is Chance * 1.2.
indiferent(Chance, NewChance) :- NewChance is Chance.
unlikely(Chance, NewChance) :- NewChance is Chance * 0.8.
very_unlikely(Chance, NewChance) :- NewChance is Chance * 0.5.
impossible(_, NewChance) :- NewChance is 0.

likely_older(Age,     Chance, NewChance) :- integer(Age), age_type(Age, Type), likely_older(Type, Chance, NewChance).
likely_older(crianca, Chance, NewChance) :- impossible(Chance, NewChance).
likely_older(jovem,   Chance, NewChance) :- very_unlikely(Chance, NewChance).
likely_older(adulto,  Chance, NewChance) :- unlikely(Chance, NewChance).
likely_older(idoso,   Chance, NewChance) :- indiferent(Chance, NewChance).

likely_younger(Age,     Chance, NewChance) :- integer(Age), age_type(Age, Type), likely_younger(Type, Chance, NewChance).
likely_younger(crianca, Chance, NewChance) :- indiferent(Chance, NewChance).
likely_younger(jovem,   Chance, NewChance) :- unlikely(Chance, NewChance).
likely_younger(adulto,  Chance, NewChance) :- very_unlikely(Chance, NewChance).
likely_younger(idoso,   Chance, NewChance) :- impossible(Chance, NewChance).

likely_male(masculino, Chance, NewChance) :- likely(Chance, NewChance).
likely_male(feminino,  Chance, NewChance) :- unlikely(Chance, NewChance).
only_male(masculino, _, _).
only_male(feminino,  _, NewChance) :- NewChance is 0.

likely_female(masculino, Chance, NewChance) :- unlikely(Chance, NewChance).
likely_female(feminino,  Chance, NewChance) :- likely(Chance, NewChance).
only_female(masculino, _, NewChance) :- NewChance is 0.
only_female(feminino,  _, _).


diagnostico_comuns(Idade, Genero, Sintomas, R) :-
    findall(D, doenca(D), Doencas),
    diagnostico_todas(Doencas, Idade, Genero, Sintomas, R).
diagnostico_raras(Idade, Genero, Sintomas, R) :-
    findall(D, doenca_rara(D), Doencas),
    diagnostico_todas(Doencas, Idade, Genero, Sintomas, R).


% diagnostico_todas(+Doencas, +Idade, +Genero, +Sintomas, ?R) is nondet
% Dadas as informações do paciente, dá o Resultado que consiste em uma
% lista, onde cada elemento é outra lista, com os elementos nesta ordem:
% [Doenca, Probabilidade, CntSintP, CntSintD, CntSintCarac]
% onde: CntSintP é a quantidade de sintomas de Doenca que o paciente possui
% CntSintD é a quantidade de sintomas da doenca
% CntSintCarac é a quantidade de sintomas não comuns de Doenca que o paciente possui
:- begin_tests(diag).
test(t0) :- diagnostico_comuns(3, masculino, ['chiado ao respirar', 'dificultade de respirar', 'tosse frequente', 'falta de ar'], R),
    reverse(R, Reversed),
    assertion(Reversed = [[asma | _] | _]).

test(t1) :- diagnostico_comuns(3, feminino, ['chiado ao respirar'], R),
    reverse(R, Reversed),
    assertion(Reversed = [[asma | _] | _]).

:- end_tests(diag).

diagnostico_todas(Doencas, Idade, Genero, Sintomas, Resultado) :-
    diagnostico(Doencas, Idade, Genero, Sintomas, Unsorted),
    predsort(compare_second_element, Unsorted, Sorted),
    Resultado = Sorted.

diagnostico([], _, _, _, []).
diagnostico([D | Rest], Idade, Genero, Sintomas, Resultado) :-
    diagnostico(Rest, Idade, Genero, Sintomas, ResultadoRest),

    diagnostico(D, Idade, Genero, 0.5, BasicChance),
    count_sintomas(D, Sintomas, CntSintComum, CntSintCarac),
    count_sintomas_ausentes(D, Sintomas, CntAusenteComum, CntAusenteCarac),

    (doenca_rara(D) ->
        Calc is 100 * BasicChance
                * (1.1 ** CntSintComum)
                * (1.3 ** CntSintCarac)
                * (0.7 ** CntAusenteComum)
                * (0.2 ** CntAusenteCarac)
    ;   Calc is 100 * BasicChance
                * (1.2 ** CntSintComum)
                * (1.5 ** CntSintCarac)
                * (0.8 ** CntAusenteComum)
                * (0.4 ** CntAusenteCarac)),

    (Calc > 100 -> FinalChance is 100 ; truncate(Calc, FinalChance)),
    CntTotalDoenca is CntSintComum + CntSintCarac + CntAusenteComum + CntAusenteCarac,
    CntTotalPac is CntSintComum + CntSintCarac,

    append(ResultadoRest,
           [[D, FinalChance, CntTotalPac, CntTotalDoenca, CntSintCarac]],
           Resultado).


% Diagnósticos básicos de cada doença. É básico pois considera apenas idade e genero.
diagnostico('malaria', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('sarampo', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('covid19', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('dengue', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('diabetes', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('hipertensao', Age, _, Chance, NewChance) :- likely_older(Age, Chance, NewChance).
diagnostico('cancer', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('avc', Age, _, Chance, NewChance) :- likely_older(Age, Chance, NewChance).
diagnostico('asma', Age, _, Chance, NewChance) :- likely_younger(Age, Chance, NewChance).
diagnostico('osteoporose', Age, Gender, Chance, NewChance) :- likely_older(Age, Chance, NewChance1), likely_female(Gender, NewChance1, NewChance).
diagnostico('febre amarela', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('chikungunya', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('leishmaniose', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('acromegalia', Age, _, Chance, NewChance) :- likely_older(Age, Chance, NewChance).
diagnostico('fenilcetonuria', Age, _, Chance, NewChance) :- likely_younger(Age, Chance, NewChance).
diagnostico('fibrose cistica', _, _, Chance, NewChance) :- indiferent(Chance, NewChance).
diagnostico('cistinose', Age, _, Chance, NewChance) :- likely_younger(Age, Chance, NewChance).

compare_second_element(Order, [_, X | _], [_, Y | _]) :-
    compare(O, X, Y),
    (O = (=) -> Order = (>) ; Order = O).

 
% trucate(+Num, ?R) is semidet
% é true se R é o número Num arredondado e com apenas dois dígitos após a vírgula
:- begin_tests(trunc).
test(t0) :- truncate(1, 1).
test(t1) :- truncate(1.2, 1.2).
test(t2) :- truncate(1.239, 1.24).
test(t3) :- truncate(28.8471008, 28.85).
:- end_tests(trunc).

truncate(Num, R) :-
    R is round(Num * 100) / 100.


% count_sintomas(+Doenca, +Sintomas, ?CntComum, ?CntCarac) is nondet
% é true se CntComum e CntCarac são a quantidade de sintomas comuns
% e a quantidade de sintomas nãocomuns de Doenca, respectivamente,
% presentes na lista Sintomas
:- begin_tests(cntsint).
test(t0) :- count_sintomas(asma, ['chiado ao respirar'], 0, 1).
test(t1) :- count_sintomas(asma, ['chiado ao respirar', 'falta de ar'], 1, 1).
test(t2) :- count_sintomas(hipertensao, ['visao embacada', 'dor de cabeca', 'tontura'], 2, 1).
:- end_tests(cntsint).

count_sintomas(_, [], 0, 0) :- !.
count_sintomas(Doenca, [Sintoma | R], CntSintComum, CntSintCarac) :-
    count_sintomas(Doenca, R, CntSintComumR, CntSintCaracR),
    (sintoma(Doenca, Sintoma) ->
        (sintoma_comum(Sintoma) ->
            CntSintComum is CntSintComumR + 1,
            CntSintCarac is CntSintCaracR
        ;   CntSintComum is CntSintComumR,
            CntSintCarac is CntSintCaracR + 1)
        ;   CntSintComum is CntSintComumR,
            CntSintCarac is CntSintCaracR).


% count_sintomas_ausentes(+Doenca, +Sintomas, ?CntComum, ?CntCarac) is nondet
% é true se CntComum e CntCarac são a quantidade de sintomas comuns de Doenca
% e a quantidade de sintomas não comuns de Doenca, respectivamente, tal que
% o sintoma NAO aparece na lista Sintomas e sendo, portanto, um sintoma ausente.
:- begin_tests(cntaus).
test(t0) :- count_sintomas_ausentes(asma, ['chiado ao respirar'], 2, 1).
test(t1) :- count_sintomas_ausentes(asma, ['chiado ao respirar', 'falta de ar'], 1, 1).
test(t2) :- count_sintomas_ausentes(hipertensao, [], 2, 3).
test(t3) :- count_sintomas_ausentes(hipertensao, ['dor de cabeca', 'tontura', 'dor no peito', 'fraqueza', 'visao embacada'], 0, 0).
:- end_tests(cntaus).

count_sintomas_ausentes(D, Sintomas, CntComum, CntCarac) :-
    atom(D),
    findall(S, sintoma(D, S), SintomasD),
    count_sintomas_ausentes(SintomasD, Sintomas, CntComum, CntCarac).

count_sintomas_ausentes([], _, 0, 0).
count_sintomas_ausentes([SintD | Rd], SintP, CntComum, CntCarac) :-
    count_sintomas_ausentes(Rd, SintP, CntComumR, CntCaracR),
    (member(SintD, SintP) ->
        CntComum is CntComumR,
        CntCarac is CntCaracR
    ;   (sintoma_comum(SintD) ->
            CntComum is CntComumR + 1,
            CntCarac is CntCaracR
        ;   CntComum is CntComumR,
            CntCarac is CntCaracR + 1)).


