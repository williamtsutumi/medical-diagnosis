:- use_module(library(pce)).
:- include('patients_crud.pl').


main :-
    retract_and_read(Patients, _),

    new(Dialog, dialog('Register')),

    new(BtnCadastro, button('Cadastrar paciente')),
        ActionCadastro = message(@prolog, cadastro),
        send(BtnCadastro, message, ActionCadastro),
    send(Dialog, append, BtnCadastro),

    send(Dialog, append, new(IdSelected, text_item(id))),
    new(BtnEdit, button('Vizualizar informacoes')),
        ActionEdit = message(@prolog, cadastro,
                             IdSelected?selection),
        send(BtnEdit, message, ActionEdit),
    send(Dialog, append, BtnEdit),
    new(BtnDelete, button('Deletar')),
        ActionDelete = message(@prolog, delete),
        send(BtnDelete, message, ActionDelete),
    send(Dialog, append, BtnDelete),

    send(Dialog, append, new(text(Patients))),

    send(Dialog, open).


cadastro(Id) :-

    new(Dialog, dialog('Cadastro de paciente')),
    send(Dialog, append, new(Nome, text_item(nome))),
    send(Dialog, append, new(Idade, text_item(idade))),

    send(Dialog, append, new(Genero, menu(genero, marked))), 
        send(Genero, append, masculino),
        send(Genero, append, feminino), 

    (patient(Id, OldNome, OldIdade, OldGenero) -> Nome?selection is OldNome, Idade?selection is OldIdade, Genero?selection is OldGenero
    ; true),

    send(Dialog, append, 
        button(create, message(@prolog, create,
                               Nome?selection,
                               Idade?selection,
                               Genero?selection))),

    send(Dialog, open).


retract_and_read(Patients, Count) :-
    retractall(patient(_,_,_,_)),
    read_patients(Patients, Count).

