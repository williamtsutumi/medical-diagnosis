:- use_module(library(plunit)).
:- use_module(library(clpfd)).


% todo -> teste
% Classifica pessoas em faixas etárias de acordo com a idade. Requer que Age >= 0
age_type(Age, crianca) :- Age >= 0, Age < 12.
age_type(Age, jovem) :- Age >= 12, Age < 30.
age_type(Age, adulto) :- Age >= 30, Age < 60.
age_type(Age, idoso) :- Age >= 60.

initial_chance(Chance) :- Chance is 0.5.

likely(Chance, NewChance) :- NewChance is Chance * 1.2.
indiferent(Chance, NewChance) :- NewChance is Chance.
unlikely(Chance, NewChance) :- NewChance is Chance * 0.8.
very_unlikely(Chance, NewChance) :- NewChance is Chance * 0.5.
impossible(_, NewChance) :- NewChance is 0.

likely_older(Age, Chance, NewChance) :- integer(Age), age_type(Age, Type), likely_older(Type, Chance, NewChance).
likely_older(crianca, Chance, NewChance) :- impossible(Chance, NewChance).
likely_older(jovem, Chance, NewChance) :- very_unlikely(Chance, NewChance).
likely_older(adulto, Chance, NewChance) :- unlikely(Chance, NewChance).
likely_older(idoso, Chance, NewChance) :- indiferent(Chance, NewChance).

likely_younger(Age, Chance, NewChance) :- integer(Age), age_type(Age, Type), likely_younger(Type, Chance, NewChance).
likely_younger(crianca, Chance, NewChance) :- indiferent(Chance, NewChance).
likely_younger(jovem, Chance, NewChance) :- unlikely(Chance, NewChance).
likely_younger(adulto, Chance, NewChance) :- very_unlikely(Chance, NewChance).
likely_younger(idoso, Chance, NewChance) :- impossible(Chance, NewChance).

likely_male(masculino, Chance, NewChance) :- NewChance is Chance * 1.3.
likely_male(feminino, Chance, NewChance) :- NewChance is Chance * 0.7.
only_male(masculino, _, _).
only_male(feminino, _, NewChance) :- NewChance is 0.

likely_female(masculino, Chance, NewChance) :- NewChance is Chance * 0.7.
likely_female(feminino, Chance, NewChance) :- NewChance is Chance * 1.3.
only_female(masculino, _, NewChance) :- NewChance is 0.
only_female(feminino, _, _).

% todo -> teste
:- begin_tests(diag).

test(t0) :- diagnostico(3, masculino,
    ['chiado ao respirar', 'dificultade de respirar', 'tosse frequente', 'falta de ar'],
    [[asma, 100] | _]).

test(t1) :- diagnostico(3, masculino,
    ['chiado ao respirar'],
    [[asma, _] | _]).

:- end_tests(diag).

% Dadas as informações do paciente, dá o Resultado que consiste em uma
% lista de listas de tamanho dois, onde o primeiro elemento é o nome da doença
% e o segundo elemento é a probabilidade do paciente ter a doença.
diagnostico(Idade, Genero, Sintomas, Resultado) :-
    findall(D, doenca(D), Doencas),
    diagnostico(Doencas, Idade, Genero, Sintomas, Unsorted),
    predsort(compare_second_element, Unsorted, Resultado).

diagnostico([], _, _, _, []).
diagnostico([D | Rest], Idade, Genero, Sintomas, Resultado) :-
    diagnostico(Rest, Idade, Genero, Sintomas, ResultadoRest),

    initial_chance(ChanceI),
    diagnostico(D, Idade, Genero, ChanceI, BasicChance),
    count_sintomas(D, Sintomas, CntSintComum, CntSintCarac),
    count_sintomas_ausentes(D, Sintomas, CntAusenteComum, CntAusenteCarac),

    Calc is 100 * BasicChance
                * (1.2 ** CntSintComum)
                * (1.5 ** CntSintCarac)
                * (0.9 ** CntAusenteComum)
                * (0.5 ** CntAusenteCarac),
    (Calc > 100 -> FinalChance is 100 ; truncate(Calc, FinalChance)),

    append(ResultadoRest, [[D, FinalChance]], Resultado).


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

compare_second_element(Order, [_, X], [_, Y]) :-
    compare(O, X, Y),
    (O = (=) -> Order = (>) ; Order = O).

truncate(Num, R) :-
    R is round(Num * 100) / 100.

% todo -> teste
% Para uma dada Doenca e uma lista de sintomas:
% CntSintComum é a quantidade de sintomas comuns da doença que aparecem na lista de sintomas.
% CntSintCarac é a quantidade de sintomas não comuns da doença que aparecem na lista de sintomas.
count_sintomas(_, [], 0, 0).
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

% todo -> teste
% Dada a doença D e os SintomasPaciente:
% CntAusenteComum é a quantidade de sintomas comuns da doença que não aparecem na lista de sintomas.
% CntAusenteCarac é a quantidade de sintomas não comuns da doença que não aparecem na lista de sintomas.
count_sintomas_ausentes(D, SintomasPaciente, CntAusenteComum, CntAusenteCarac) :-
    atom(D),
    findall(S, sintoma(D, S), SintomasDoenca),
    count_sintomas_ausentes(SintomasDoenca, SintomasPaciente, CntAusenteComum, CntAusenteCarac).

count_sintomas_ausentes([], _, 0, 0).
count_sintomas_ausentes([SintDoenca | Rdoenca], SintomasPaciente, CntAusenteComum, CntAusenteCarac) :-
    count_sintomas_ausentes(Rdoenca, SintomasPaciente, CntAusenteComumR, CntAusenteCaracR),
    (member(SintDoenca, SintomasPaciente) ->
        CntAusenteComum is CntAusenteComumR,
        CntAusenteCarac is CntAusenteCaracR
    ;   (sintoma_comum(SintDoenca) ->
            CntAusenteComum is CntAusenteComumR + 1,
            CntAusenteCarac is CntAusenteCaracR
        ;   CntAusenteComum is CntAusenteComumR,
            CntAusenteCarac is CntAusenteCaracR + 1)).


doenca('malaria').
doenca('sarampo').
doenca('covid19').
doenca('dengue').
doenca('diabetes').
doenca('hipertensao').
doenca('cancer').
doenca('avc').
doenca('asma').
doenca('osteoporose').
doenca('febre amarela').
doenca('chikungunya').
doenca('leishmaniose').

sintoma('calafrio').
sintoma('chiado ao respirar').
sintoma('confusao').
sintoma('conjuntivite').
sintoma('coriza').
sintoma('dificuldade de respirar').
sintoma('dor muscular').
sintoma('dor atras dos olhos').
sintoma('dor de cabeca').
sintoma('dor no peito').
sintoma('exantema').
sintoma('formigamento').
sintoma('falta de ar').
sintoma('fraqueza').
sintoma('fraqueza extrema').
sintoma('febre alta').
sintoma('manchas vermelhas').
sintoma('mudanca de humor').
sintoma('nausea').
sintoma('perda de paladar').
sintoma('perda de olfato').
sintoma('perda de visao').
sintoma('perda de peso').
sintoma('sudorese').
sintoma('sede').
sintoma('tremor').
sintoma('tosse frequente').
sintoma('tontura').
sintoma('ictericia').
sintoma('erupcao cutanea').
sintoma('dor articular').
sintoma('anemia').
sintoma('retracao gengival').

sintoma_comum('dor de cabeca').
sintoma_comum('tosse').
sintoma_comum('coriza').
sintoma_comum('febre').
sintoma_comum('nausea').
sintoma_comum('tontura').

% Relação entre doencas e sintomas        
sintoma('febre amarela', 'febre alta').
sintoma('febre amarela', 'dor muscular').
sintoma('febre amarela', 'dor de cabeca').
sintoma('febre amarela', 'calafrio').
sintoma('febre amarela', 'nausea').
sintoma('febre amarela', 'fraqueza').
sintoma('febre amarela', 'ictericia').
sintoma('chikungunya', 'febre').
sintoma('chikungunya', 'dor articular').
sintoma('chikungunya', 'dor de cabeca').
sintoma('chikungunya', 'fraqueza').
sintoma('chikungunya', 'erupcao cutanea').
sintoma('chikungunya', 'dor muscular').
sintoma('leishmaniose', 'febre').
sintoma('leishmaniose', 'fraqueza').
sintoma('leishmaniose', 'perda de peso').
sintoma('leishmaniose', 'anemia').
sintoma('malaria', 'febre alta').
sintoma('malaria', 'dor de cabeca').
sintoma('malaria', 'calafrio').
sintoma('malaria', 'tremor').
sintoma('malaria', 'sudorese').
sintoma('sarampo', 'febre alta').
sintoma('sarampo', 'tosse').
sintoma('sarampo', 'conjuntivite').
sintoma('sarampo', 'coriza').
sintoma('sarampo', 'exantema').
sintoma('covid19', 'tosse').
sintoma('covid19', 'perda de olfato').
sintoma('covid19', 'perda de paladar').
sintoma('covid19', 'falta de ar').
sintoma('covid19', 'dor muscular').
sintoma('dengue', 'febre alta').
sintoma('dengue', 'fraqueza extrema').
sintoma('dengue', 'dor atras dos olhos').
sintoma('dengue', 'dor muscular').
sintoma('dengue', 'dor de cabeca').
sintoma('dengue', 'manchas vermelhas').
sintoma('diabetes', 'perda de peso').
sintoma('diabetes', 'fraqueza').
sintoma('diabetes', 'nausea').
sintoma('diabetes', 'sede').
sintoma('diabetes', 'mudanca de humor').
sintoma('hipertensao', 'dor de cabeca').
sintoma('hipertensao', 'tontura').
sintoma('hipertensao', 'dor no peito').
sintoma('hipertensao', 'fraqueza').
sintoma('hipertensao', 'visao embacada').
sintoma('cancer', 'tudo').
sintoma('avc', 'dor de cabeca').
sintoma('avc', 'formigamento').
sintoma('avc', 'perda de visao').
sintoma('avc', 'tontura').
sintoma('avc', 'confusao').
sintoma('asma', 'tosse frequente').
sintoma('asma', 'dificuldade de respirar').
sintoma('asma', 'chiado ao respirar').
sintoma('asma', 'falta de ar').
sintoma('osteoporose', 'retracao gengival').


