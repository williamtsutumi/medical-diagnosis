:- use_module(library(pce)).
:- include('patients_crud.pl').



main :-
    consult('patients.txt'),

    new(MainDialog, dialog('Register')),

    new(BtnCadastro, button('Cadastrar paciente')),
        ActionCadastro = message(@prolog, cadastro,
                                 MainDialog),
        send(BtnCadastro, message, ActionCadastro),
    send(MainDialog, append, BtnCadastro),

    send(MainDialog, append, new(IdSelected, text_item(id))),
    new(BtnEdit, button('Visualizar')),
        ActionEdit = message(@prolog, cadastro,
                             MainDialog,
                             IdSelected?selection),
        send(BtnEdit, message, ActionEdit),
    send(MainDialog, append, BtnEdit),

    new(BtnDelete, button('Deletar')),
        ActionDelete = message(@prolog, delete,
                               MainDialog,
                               IdSelected?selection),
        send(BtnDelete, message, ActionDelete),
    send(MainDialog, append, BtnDelete),

    list_patients(MainDialog),

    send(MainDialog, open).


cadastro(MainDialog) :-
    cadastro(MainDialog, '-1').


cadastro(MainDialog, IdString) :-
    new(Dialog, dialog('Cadastro de paciente')),
    send(Dialog, append, new(Nome, text_item(nome))),
    send(Dialog, append, new(Idade, text_item(idade))),

    send(Dialog, append, new(Genero, menu(genero, marked))), 
        send(Genero, append, masculino),
        send(Genero, append, feminino), 

    atom_number(IdString, Id),
    (patient(Id, Name, Age, Gender) ->
        send(Nome, selection, Name),
        send(Idade, selection, Age),
        send(Genero, selection, Gender)
    ; true),

    send(Dialog, append, 
        button(create, message(@prolog, on_create_click,
                               MainDialog,
                               Dialog,
                               'patients.txt',
                               Id,
                               Nome?selection,
                               Idade?selection,
                               Genero?selection))),

    send(Dialog, open).


on_create_click(MainDialog, Dialog, FileName, Id, Nome, Idade, Genero) :-
    save_patient(FileName, Id, Nome, Idade, Genero),
    send(MainDialog, destroy),
    send(Dialog, destroy),
    main.


list_patients(MainDialog) :-
    retractall(patient(_,_,_,_)),
    consult('patients.txt'),
    findall(patient(Id, Name, Age, Gender), patient(Id, Name, Age, Gender), Patients),
    get_patients_string(Patients, String),
    send(MainDialog, append, new(_, text(String))).

get_patients_string([], '').
get_patients_string([patient(Id, Nome, Idade, Genero) | R], String) :-
    format(atom(Text), 'Id: ~w, Nome: ~w, Idade: ~w, Genero: ~w\n', [Id, Nome, Idade, Genero]),
    get_patients_string(R, RestString),
    atom_concat(Text, RestString, String).


delete(MainDialog, Id) :-
    delete_patient(Id),
    send(MainDialog, destroy),
    main.


