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
indiferent(_) :- true.
unlikely(Chance) :- NewChance is Chance * 0.8, Chance is NewChance.
very_unlikely(Chance) :- NewChance is Chance * 0.5, Chance is NewChance.
impossible(Chance) :- Chance is 0.

likely_older(Age, Chance) :- integer(Age), age_to_type(Type, Age), likely_older(Type, Chance).
likely_older(crianca, Chance) :- impossible(Chance).
likely_older(jovem, Chance) :- very_unlikely(Chance).
likely_older(adulto, Chance) :- unlikely(Chance).
likely_older(idoso, Chance) :- indiferent(Chance).

likely_younger(Age, Chance) :- integer(Age), age_to_type(Type, Age), likely_younger(Type, Chance).
likely_younger(crianca, Chance) :- indiferent(Chance).
likely_younger(jovem, Chance) :- unlikely(Chance).
likely_younger(adulto, Chance) :- very_unlikely(Chance).
likely_younger(idoso, Chance) :- impossible(Chance).

likely_male(masculino, Chance) :- NewChance is Chance * 1.3, Chance is NewChance.
likely_male(feminino, Chance) :- NewChance is Chance * 0.7, Chance is NewChance.
only_male(masculino, _).
only_male(feminino, Chance) :- Chance is 0.

likely_female(masculino, Chance) :- NewChance is Chance * 0.7, Chance is NewChance.
likely_female(feminino, Chance) :- NewChance is Chance * 1.3, Chance is NewChance.
only_female(masculino, Chance) :- Chance is 0.
only_female(feminino, _).

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

sintoma_comum('dor de cabeca').
sintoma_comum('tosse').
sintoma_comum('coriza').
sintoma_comum('febre').
sintoma_comum('nausea').
sintoma_comum('tontura').

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

diagnostico('malaria', _, _, Chance) :- indiferent(Chance).
diagnostico('sarampo', _, _, Chance) :- indiferent(Chance).
diagnostico('covid19', _, _, Chance) :- indiferent(Chance).
diagnostico('dengue', _, _, Chance) :- indiferent(Chance).
diagnostico('diabetes', _, _, Chance) :- indiferent(Chance).
diagnostico('hipertensao', Age, _, Chance) :- likely_older(Age, Chance).
diagnostico('cancer', _, _, Chance) :- indiferent(Chance).
diagnostico('avc', Age, _, Chance) :- likely_older(Age, Chance).
diagnostico('asma', Age, _, Chance) :- likely_younger(Age, Chance).
diagnostico('osteoporose', Age, Gender, Chance) :- likely_older(Age, Chance), likely_female(Gender, Chance).
diagnostico('febre amarela', _, _, Chance) :- indiferent(Chance).
diagnostico('chikungunya', _, _, Chance) :- indiferent(Chance).
diagnostico('leishmaniose', _, _, Chance) :- indiferent(Chance).

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

