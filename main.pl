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
    send(T, colour, red),
    send(T, font, font(arial, bold, 10)),

    send(MainDialog, append, new(BtnGroup, dialog_group('Selecione'))),
    send(BtnGroup, append, new(IdSelected, text_item(id))),
    new(BtnEdit, button('Visualizar')),
        ActionEdit = message(@prolog, cadastro,
                             MainDialog,
                             IdSelected?selection),
        send(BtnEdit, message, ActionEdit),
        send(BtnEdit, colour, dark_blue),
    send(BtnGroup, append, BtnEdit),

    new(BtnDiagnostico, button('Diagnosticar')),
        ActionDiagnostico = message(@prolog, on_diagnostico_click,
                                    IdSelected?selection),
        send(BtnDiagnostico, message, ActionDiagnostico),
        send(BtnDiagnostico, colour, dark_blue),
    send(BtnGroup, append, BtnDiagnostico),

    new(BtnDelete, button('Deletar')),
        ActionDelete = message(@prolog, on_delete_click,
                               MainDialog,
                               IdSelected?selection),
        send(BtnDelete, message, ActionDelete),
    send(BtnDelete, colour, red),
    send(BtnGroup, append, BtnDelete),

    new(BtnCadastro, button('Cadastrar')),
        ActionCadastro = message(@prolog, cadastro,
                                 MainDialog, '-1'),
        send(BtnCadastro, message, ActionCadastro),
        send(BtnCadastro, colour, dark_blue),
    send(MainDialog, append, BtnCadastro),
    send(MainDialog, display, BtnCadastro, point(300, 57)),

    list_patients(MainDialog, ListGroup),

    new(BtnCancel, button('Sair')),
        send(BtnCancel, colour, red),
        send(BtnCancel, message, message(@prolog, on_cancel_click, MainDialog)),
    send(MainDialog, append, BtnCancel),
    send(MainDialog, display, BtnCancel, point(200, ListGroup?height + ListGroup?y + 20)),

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

    send(Dialog, append, new(G, dialog_group('Sintomas'))),
    send(G, append, new(List, list_browser)), 
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
    send(BtnSalvar, colour, dark_blue),

    new(BtnCancel, button('Cancelar')),
        send(BtnCancel, colour, red),
        send(BtnCancel, message, message(@prolog, on_cancel_click, Dialog)),
    send(Dialog, append, BtnCancel),

    send(List, size, size(50, List?height)),
    send(Dialog, append, BtnSalvar),
    send(Dialog, open).

on_cancel_click(Dialog) :-
    send(Dialog, destroy).

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


list_patients(MainDialog, G) :-
    retractall(patient(_,_,_,_)),
    consult('patients.txt'),
    send(MainDialog, append, new(G, dialog_group('Pacientes'))),
    findall(patient(Id, Name, Age, Gender), patient(Id, Name, Age, Gender), Patients),
    get_patients_string(Patients, String),
    send(G, append, new(_, text(String))),
    send(MainDialog, display, G, point(0, 115)).

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
    send(Dialog, scrollbars, both),

    send(Dialog, append, new(PatGroup, dialog_group('Paciente'))),
    format(atom(PatString), 'Nome: ~w\nIdade: ~w\nGenero: ~w', [Nome, Idade, Genero]),
    send(PatGroup, append, new(_, text(PatString))),

    send(PatGroup, append, new(SintGroup, dialog_group('Sintomas'))),
    findall(S, sintoma_paciente(Id, S), Sintomas),
    atomic_list_concat(Sintomas, '\n', SintomaText),
    send(SintGroup, append, new(_, text(SintomaText))),

    diagnostico(Idade, Genero, Sintomas, Diagnostico),
    writeln(Diagnostico),
    send(Dialog, append, new(ProbGroup, dialog_group('Probabilidades (%):'))),
    append_sint(Diagnostico, ProbGroup),
    send(Dialog, display, ProbGroup, point(230,8)),

    send(Dialog, open).


append_sint(Sin, G) :-
    length(Sin, Len),
    Y is 35 * Len,
    send(G, append, new(T, text('Escolha um item para ver detalhes'))),
    send(G, display, T, point(0, 0)),
    send(T, colour, red),
    append_sint(Sin, G, Y, T).

append_sint([], _, _, _).
append_sint([[D, Prob, CntTotalP, CntTotalD, CntCarac] | R], G, Y, T) :-
    NewY is Y - 35,
    append_sint(R, G, NewY, T),
    format(atom(Text), '~w: ~w', [D, Prob]),
    send(G, append, new(Label, text(Text))),
    send(G, display, Label, point(0,Y)),
    new(Btn, button('?')),
    send(G, append, Btn),
    send(G, display, Btn, point(230, Label?y)),
    send(Btn, message, message(@prolog, on_interrogation_click,
                               T, D, CntTotalP, CntTotalD, CntCarac)).

on_interrogation_click(Text, D, CntTotalP, CntTotalD, CntCarac) :-
    format(atom(T), '~w/~w sintomas, ~w sao carac. de ~w', [CntTotalP, CntTotalD, CntCarac, D]),
    send(Text, string, T).

:- main.


