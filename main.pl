:- use_module(library(pce)).
:- include('patients_crud.pl').


:- dynamic(patient/4).
:- dynamic(sintoma_paciente/2).

main :-
    consult('patients.txt'),
    consult('doencas.pl'),

    new(MainDialog, dialog('Register')),
    send(MainDialog, scrollbars, both),

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

    new(BtnDiagnostico, button('Diagnosticar')),
        ActionDiagnostico = message(@prolog, diagnosticar,
                                    IdSelected?selection),
        send(BtnDiagnostico, message, ActionDiagnostico),
    send(MainDialog, append, BtnDiagnostico),

    new(BtnDelete, button('Deletar')),
        ActionDelete = message(@prolog, on_delete_click,
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
    send(Dialog, scrollbars, both),
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

    send(Dialog, append, new(T, text('Sintomas'))),
    send(T, font, bold),
    Selections = [],
    forall(sintoma(S), (
        send(Dialog, append, new(Menu, menu(., marked))), 
        send(Menu, multiple_selection, @on),

        new(Item, menu_item(S)),
        send(Item, message, message(@prolog, update_selection, S, Selections)),

        send(Menu, append, Item)
    )),

    new(BtnSalvar, button('Salvar')),
    send(BtnSalvar, message, message(@prolog, on_create_click,
                               MainDialog,
                               Dialog,
                               'patients.txt',
                               Id,
                               Nome?selection,
                               Idade?selection,
                               Genero?selection,
                               Selections)),
    send(Dialog, append, BtnSalvar),

    send(Dialog, open).

update_selection(Label, Selections) :-
    writeln(Label),
    append(Selections, [Label], Selections).

on_create_click(MainDialog, Dialog, FileName, Id, Nome, Idade, Genero, Simp) :-
    save_patient(FileName, Id, Nome, Idade, Genero, Simp),
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



on_delete_click(MainDialog, Id) :-
    delete_patient(Id),
    send(MainDialog, destroy),
    main.


