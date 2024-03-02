:- use_module(library(pce)).
:- include('patients_crud.pl').


:- dynamic(patient/4).
:- dynamic(sintoma_paciente/2).

main :-
    consult('patients.txt'),
    consult('doencas.pl'),

    new(MainDialog, dialog('Register')),
    send(MainDialog, scrollbars, both),

    send(MainDialog, append, new(T, text('O resultado do protótipo é apenas informativo e o paciente deve\nconsultar um médico para obter um diagnóstico correto e preciso.'))),
    send(T, font, bold),

    new(BtnCadastro, button('Cadastrar paciente')),
        ActionCadastro = message(@prolog, cadastro,
                                 MainDialog, '-1'),
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
        ActionDiagnostico = message(@prolog, on_diagnostico_click,
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

    send(Dialog, append, new(T, text('Sintomas'))),
    send(T, font, bold),

    send(Dialog, append, new(List, list_browser)), 
    forall(sintoma(S), send(List, append, dict_item(S, S))),
    send(List, multiple_selection, @on),

    new(BtnSalvar, button('Salvar')),
    send(BtnSalvar, message, message(@prolog, on_create_click,
                               MainDialog,
                               Dialog,
                               'patients.txt',
                               Id,
                               Nome?selection,
                               Idade?selection,
                               Genero?selection,
                               List)),
    send(Dialog, append, BtnSalvar),
    send(Dialog, open).

on_create_click(MainDialog, Dialog, FileName, Id, Nome, Idade, Genero, List) :-
    get(List, selection, SelectionChain),
    chain_list(SelectionChain, References),
    maplist(get_item_label, References, SelectedItems),

    save_patient(FileName, Id, Nome, Idade, Genero, SelectedItems),
    send(MainDialog, destroy),
    send(Dialog, destroy),
    main.

get_item_label(Reference, Label) :-
    get(Reference, label, Label).


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


on_diagnostico_click(IdAtom) :-
    atom_number(IdAtom, Id),
    patient(Id, Nome, Idade, Genero),

    new(Dialog, dialog('Diagnostico')),
    format(atom(PatString), 'Paciente:\nNome: ~w\nIdade: ~w\nGenero: ~w', [Nome, Idade, Genero]),
    send(Dialog, append, new(_, text(PatString))),

    findall(S, sintoma_paciente(Id, S), Sintomas),
    send(Dialog, append, new(T, text('Sintomas:'))),
    send(T, font, bold),
    atomic_list_concat(Sintomas, '\n', SintomaText),
    send(Dialog, append, new(_, text(SintomaText))),

    diagnostico(Idade, Genero, Sintomas, Result),
    format_sintomas(Result, DiagResultList),
    atomic_list_concat(DiagResultList, '\n', Probs),
    atomic_concat('Probabilidades (%):\n', Probs, DiagText),
    send(Dialog, append, new(_, text(DiagText))),

    send(Dialog, open).

format_sintomas([], []).
format_sintomas([[D, Prob] | R], Result) :-
    format_sintomas(R, RestResult),
    format(atom(Text), '~w: ~w', [D, Prob]),
    append(RestResult, [Text], Result).


:- main.


