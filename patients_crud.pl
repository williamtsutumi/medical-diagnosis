gender('masculino').
gender('feminino').


create(Id, Name, Age, Gender) :-
    gender(Gender),
    open('patients.txt', append, Stream),
    format(Stream, 'patient(~w, \'~w\', ~w, \'~w\').\n', [Id, Name, Age, Gender]),
    close(Stream).


% E true caso 'Num' seja igual a quantidade de pacientes cadastrados.
read_patients_file(File, Num) :-
    open(File, read, Stream),
    read_patient_facts(Stream, Count),
    !,
    close(Stream),
    Num is Count.

read_patient_facts(Stream, Count) :-
    read_patient_facts(Stream, 0, Count).

read_patient_facts(Stream, Acc, Count) :-
    repeat,
    read(Stream, Term),
    (   Term \= end_of_file -> assert(Term),
        NewAcc is Acc + 1,
        read_patient_facts(Stream, NewAcc, Count)
    ;   Count is Acc
    ).


read_patients(PatientsString, Count) :-
    read_patients(PatientsString, Count, 'patients.txt').
    
read_patients(PatientsString, Count, File) :-
    read_patients_file(File, Count),
    findall(PatientString,
        (   patient(Id, Name, Age, Gender),
            format(atom(PatientString), 'Id: ~w, Name: ~w, Age: ~w, Gender: ~w\n', [Id, Name, Age, Gender])
        ),
        PatientStrings),
    atomic_list_concat(PatientStrings, '\n', PatientsString).


update(Name, NewAge, NewGender) :-
    open('patients.txt', read, ReadStream),
    open('temp.txt', write, WriteStream),
    repeat,
    read_line_to_codes(ReadStream, Line),
    Line = end_of_file,
    !,
    close(ReadStream),
    close(WriteStream),
    delete_file('patients.txt'),
    rename_file('temp.txt', 'patients.txt'),
    create(Name, NewAge, NewGender).


delete(Name) :-
    open('patients.txt', read, ReadStream),
    open('temp.txt', write, WriteStream),
    repeat,
    read_line_to_codes(ReadStream, Line),
    Line = end_of_file,
    !,
    close(ReadStream),
    close(WriteStream),
    delete_file('patients.txt'),
    rename_file('temp.txt', 'patients.txt').


