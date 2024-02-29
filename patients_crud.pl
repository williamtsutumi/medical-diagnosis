

save_patient(FileName, Id, Nome, Idade, Genero, Sintomas) :-
    Id =:= -1,
    get_max_id(Max),
    NewId is Max + 1,
    assert(patient(NewId, Nome, Idade, Genero)),
    assert_symptoms(NewId, Sintomas),

    save_patients(FileName),
    save_symptoms(FileName).

save_patient(FileName, Id, Nome, Idade, Genero, Sintomas) :-
    Id =\= -1,
    delete_patient(Id),
    consult(FileName),
    assert(patient(Id, Nome, Idade, Genero)),
    retractall(sintoma_paciente(Id, _)),
    assert_symptoms(Id, Sintomas),

    save_patients(FileName),
    save_symptoms(FileName).



save_patients(FileName) :-
    open(FileName, write, Stream),
    write_patients(Stream),
    close(Stream).

write_patients(Stream) :-
    findall(patient(Id, Name, Age, Gender), patient(Id, Name, Age, Gender), Patients),
    write_patients_list(Patients, Stream).

write_patients_list([], _).
write_patients_list([patient(Id, Nome, Idade, Genero) | Rest], Stream) :-
    format(atom(Text), 'patient(~w, \'~w\', ~w, \'~w\').\n', [Id, Nome, Idade, Genero]),
    write(Stream, Text),
    write_patients_list(Rest, Stream).


assert_symptoms(_, []).
assert_symptoms(Id, [F | R]) :-
    assert(sintoma_paciente(Id, F)),
    assert_symptoms(Id, R).


delete_patient(IdString) :-
    (integer(IdString) -> Id is IdString; atom_number(IdString, Id)),
    retractall(patient(Id, _, _, _)),
    save_patients('patients.txt').


get_max_id(Max) :-
    findall(patient(Id, Name, Age, Gender), patient(Id, Name, Age, Gender), Patients),
    max(Patients, Max).

max([], Max) :- Max is 0.
max([patient(Id, _, _, _) | R], Max) :-
    max(R, RestMax),
    (RestMax > Id ->
        Max is RestMax
      ; Max is Id).


save_symptoms(FileName) :-
    open(FileName, append, Stream),
    write_symptoms(Stream),
    close(Stream).

write_symptoms(Stream) :-
    findall(sintoma_paciente(Id, Simp), sintoma_paciente(Id, Simp), Sintomas),
    write_symp_list(Sintomas, Stream).

write_symp_list([], _).
write_symp_list([sintoma_paciente(Id, S) | Rest], Stream) :-
    format(atom(Text), 'sintoma_paciente(~w, \'~w\').\n', [Id, S]),
    write(Stream, Text),
    write_symp_list(Rest, Stream).


