age_type(crianca).
age_type(jovem).
age_type(adulto).
age_type(idoso).

age_type(Age, crianca) :- Age >= 0, Age < 12.
age_type(Age, jovem) :- Age >= 12, Age < 30.
age_type(Age, adulto) :- Age >= 30, Age < 60.
age_type(Age, idoso) :- Age >= 60.

age_to_type(Age, Type) :-
    age_type(Age, Type).

initial_chance(Chance) :- Chance is 0.5.

likely(Chance) :- NewChance is Chance * 1.2, Chance is NewChance.
indiferent(Chance) :- true.
unlikely(Chance) :- NewChance is Chance * 0.8, Chance is NewChance.
very_unlikely(Chance) :- NewChance is Chance * 0.5, Chance is NewChance.
impossible(Chance) :- Chance is 0.

likely_older(crianca, Chance) :- impossible(Chance).
likely_older(jovem, Chance) :- very_unlikely(Chance).
likely_older(adulto, Chance) :- unlikely(Chance).
likely_older(idoso, Chance) :- indiferent(Chance).

likely_younger(crianca, Chance) :- indiferent(Chance).
likely_younger(jovem, Chance) :- unlikely(Chance).
likely_younger(adulto, Chance) :- very_unlikely(Chance).
likely_younger(idoso, Chance) :- impossible(Chance).

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
sintoma('mal estar').
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

sintoma_comum('dor de cabeca').
sintoma_comum('tosse').
sintoma_comum('mal estar').
sintoma_comum('coriza').
sintoma_comum('febre').
sintoma_comum('nausea').
sintoma_comum('tontura').

doenca(malaria).
doenca(sarampo).
doenca(covid19).
doenca(dengue).
doenca(diabetes).
doenca(hipertensao).
doenca(cancer).
doenca(avc).
doenca(asma).
doenca(osteoporose).

diagnostico('malaria', _, _, Chance) :- indiferent(Chance).
diagnostico('sarampo', _, _, Chance) :- indiferent(Chance).
diagnostico('covid19', _, _, Chance) :- indiferent(Chance).
diagnostico('dengue', _, _, Chance) :- indiferent(Chance).
diagnostico('diabetes', _, _, Chance) :- indiferent(Chance).
diagnostico('hipertensao', _, _, Chance) :- indiferent(Chance).
diagnostico('cancer', _, _, Chance) :- indiferent(Chance).
diagnostico('avc', _, _, Chance) :- indiferent(Chance).
diagnostico('asma', _, _, Chance) :- indiferent(Chance).
%diagnostico('osteoporose', feminino, AgeNumber, Chance) :- age_to_type(Age, Type), likely_older(Type, Chance).
%diagnostico('osteoporose', masculino, AgeNumber, Chance) :- age_to_type(Age, Type), likely_older(Type, Chance).

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
sintoma('hipertensao', 'mal estar').
sintoma('hipertensao', 'tontura').
sintoma('hipertensao', 'dor no peito').
sintoma('hipertensao', 'fraqueza').

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

