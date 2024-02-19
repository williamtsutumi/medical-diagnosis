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


likely(Chance) :- Chance is 0.4.
indiferent(Chance) :- Chance is 0.3.
unlikely(Chance) :- Chance is 0.2.
very_unlikely(Chance) :- Chance is 0.1.
impossible(Chance) :- Chance is 0.

likely_older(crianca, Chance) :- impossible(Chance).
likely_older(jovem, Chance) :- very_unlikely(Chance).
likely_older(adulto, Chance) :- unlikely(Chance).
likely_older(idoso, Chance) :- indiferent(Chance).

likely_younger(crianca, Chance) :- indiferent(Chance).
likely_younger(jovem, Chance) :- unlikely(Chance).
likely_younger(adulto, Chance) :- very_unlikely(Chance).
likely_younger(idoso, Chance) :- impossible(Chance).

sintoma_comum('dor de cabeca').
sintoma_comum('tosse').
sintoma_comum('mal estar').
sintoma_comum('coriza').
sintoma_comum('febre').
sintoma_comum('nausea').
sintoma_comum('tontura').

doenca('malaria').
diagnostico('malaria', _, _, Chance) :- indiferent(Chance).
sintoma('malaria', 'febre alta').
sintoma('malaria', 'dor de cabeca').
sintoma('malaria', 'calafrio').
sintoma('malaria', 'tremor').
sintoma('malaria', 'sudorese').

doenca('sarampo').
diagnostico('sarampo', _, _, Chance) :- indiferent(Chance).
sintoma('sarampo', 'febre alta').
sintoma('sarampo', 'tosse').
sintoma('sarampo', 'conjuntivite').
sintoma('sarampo', 'coriza').
sintoma('sarampo', 'exantema').

doenca('covid19').
diagnostico('covid19', _, _, Chance) :- indiferent(Chance).
sintoma('covid19', 'tosse').
sintoma('covid19', 'perda de olfato').
sintoma('covid19', 'perda de paladar').
sintoma('covid19', 'falta de ar').
sintoma('covid19', 'dor muscular').

doenca('dengue').
diagnostico('dengue', _, _, Chance) :- indiferent(Chance).
sintoma('dengue', 'febre alta').
sintoma('dengue', 'fraqueza extrema').
sintoma('dengue', 'dor atras dos olhos').
sintoma('dengue', 'dor muscular').
sintoma('dengue', 'dor de cabeca').
sintoma('dengue', 'manchas vermelhas').

doenca('diabetes').
diagnostico('diabetes', _, _, Chance) :- indiferent(Chance).
sintoma('diabetes', 'perda de peso').
sintoma('diabetes', 'fraqueza').
sintoma('diabetes', 'nausea').
sintoma('diabetes', 'sede').
sintoma('diabetes', 'mudanca de humor').

doenca('hipertensao').
diagnostico('hipertensao', _, _, Chance) :- indiferent(Chance).
sintoma('hipertensao', 'dor de cabeca').
sintoma('hipertensao', 'mal estar').
sintoma('hipertensao', 'tontura').
sintoma('hipertensao', 'dor no peito').
sintoma('hipertensao', 'fraqueza').

doenca('cancer').
diagnostico('cancer', _, _, Chance) :- indiferent(Chance).
sintoma('cancer', 'tudo').

doenca('avc').
diagnostico('avc', _, _, Chance) :- indiferent(Chance).
sintoma('avc', 'dor de cabeca').
sintoma('avc', 'formigamento').
sintoma('avc', 'perda de visao').
sintoma('avc', 'tontura').
sintoma('avc', 'confusao').

doenca('asma').
diagnostico('asma', _, _, Chance) :- indiferent(Chance).
sintoma('asma', 'tosse frequente').
sintoma('asma', 'dificuldade de respirar').
sintoma('asma', 'chiado ao respirar').
sintoma('asma', 'falta de ar').

doenca('osteoporose').
diagnostico('osteoporose', feminino, AgeNumber, Chance) :- age_to_type(Age, Type), likely_older(Type, Chance).
diagnostico('osteoporose', masculino, AgeNumber, Chance) :- age_to_type(Age, Type), likely_older(Type, Chance).
