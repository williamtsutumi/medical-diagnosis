:- use_module(library(pce)).
:- use_module(library(plunit)).


:- dynamic(patient/4).
:- dynamic(sintoma_paciente/2).
:- dynamic(diag_comum/1).
:- dynamic(diag_raras/1).

:- include('patients_crud.pl').
:- include('diagnostico.pl').

% GUI
main :-
    consult('patients.txt'),

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


% GUI
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

% GUI
on_cancel_click(Dialog) :-
    send(Dialog, destroy).

% GUI
on_create_click(MainDialog, Dialog, FileName, Id, Nome, Idade, Genero, List) :-
    get(List, selection, SelectionChain),
    chain_list(SelectionChain, References),
    maplist(get_item_label, References, SelectedItems),

    save_patient(FileName, Id, Nome, Idade, Genero, SelectedItems),
    send(MainDialog, destroy),
    send(Dialog, destroy),
    main.

% GUI
get_item_label(Reference, Label) :-
    get(Reference, label, Label).


% GUI
list_patients(MainDialog, G) :-
    retractall(patient(_,_,_,_)),
    consult('patients.txt'),
    send(MainDialog, append, new(G, dialog_group('Pacientes'))),
    findall(patient(Id, Name, Age, Gender), patient(Id, Name, Age, Gender), Patients),
    get_patients_string(Patients, String),
    send(G, append, new(_, text(String))),
    send(MainDialog, display, G, point(0, 115)).

% GUI
get_patients_string([], '').
get_patients_string([patient(Id, Nome, Idade, Genero) | R], String) :-
    format(atom(Text), 'Id: ~w, Nome: ~w, Idade: ~w, Genero: ~w\n', [Id, Nome, Idade, Genero]),
    get_patients_string(R, RestString),
    atom_concat(Text, RestString, String).


% GUI
on_delete_click(MainDialog, Id) :-
    delete_patient(Id),
    send(MainDialog, destroy),
    main.


% GUI
on_diagnostico_click(IdAtom) :-
    atom_number(IdAtom, Id),
    patient(Id, Nome, Idade, Genero),

    new(D, dialog('Diagnostico')),
    send(D, scrollbars, both),

    send(D, append, new(PatGroup, dialog_group('Paciente'))),
    format(atom(PatString), 'Nome: ~w\nIdade: ~w\nGenero: ~w', [Nome, Idade, Genero]),
    send(PatGroup, append, new(_, text(PatString))),

    send(PatGroup, append, new(SintGroup, dialog_group('Sintomas'))),
    findall(S, sintoma_paciente(Id, S), Sintomas),
    atomic_list_concat(Sintomas, '\n', SintomaText),
    send(SintGroup, append, new(_, text(SintomaText))),

    diagnostico_comuns(Idade, Genero, Sintomas, Diag),
    retractall(diag_comum(_)),
    assert(diag_comum(Diag)),

    diagnostico_raras(Idade, Genero, Sintomas, DiagRaras),
    retractall(diag_raras(_)),
    assert(diag_raras(DiagRaras)),

    send(D, append, new(G, dialog_group('Diagnostico comum:'))),
    send(D, display, G, point(230, 0)),
    send(G, append, new(B, list_browser)),
    send(B, size, size(30, 20)),
    append_prob(Diag, B),

    send(G, append, new(T, text('Selecione uma doença\npara ver mais detalhes'))),
    send(T, colour, red),
    send(G, append, new(Q, button('Explicar'))),
    send(Q, message, message(@prolog, on_explicar_click, T, B, false)),

    send(D, append, new(Graras, dialog_group('Diagnostico raras:'))),
    send(D, display, Graras, point(500, 0)),
    send(Graras, append, new(Braras, list_browser)),
    send(Braras, size, size(30, 20)),
    append_prob(DiagRaras, Braras),

    send(Graras, append, new(Traras, text('Selecione uma doença\npara ver mais detalhes'))),
    send(Traras, colour, red),
    send(Graras, append, new(Qraras, button('Explicar'))),
    send(Qraras, message, message(@prolog, on_explicar_click, Traras, Braras, true)),

    new(BtnCancel, button('Sair')),
    send(BtnCancel, colour, red),
    send(BtnCancel, message, message(@prolog, on_cancel_click, D)),
    send(D, append, BtnCancel),
    send(D, display, BtnCancel, point(250, 430)),

    send(D, open).

    
% GUI
append_prob([], _).
append_prob([[D, Prob, _, _, _] | R], B) :-
    append_prob(R, B),
    format(atom(Text), ' ~w : ~w%', [D, Prob]),
    send(B, append, Text).

% GUI
on_explicar_click(T, B, IsRares) :-
    (IsRares -> diag_raras(Diag) ; diag_comum(Diag)),
    get(B, selection, Reference),
    get(B, member, Reference, Selection),
    get(Selection, label, Item),

    split_string(Item, ":", none, [D | _]),
    trim(D, Doenca),
    find_sint(Diag, [Doenca, _, CntSintP, CntSintD, CntCarac]),
    format(string(New), 'Possui ~w/~w sintomas de ~w.\n~w são características.', [CntSintP, CntSintD, Doenca, CntCarac]),
    send(T, clear),
    send(T, append, New).


% find_sint(Lst, X)
% é true se X é algum elemento qualquer da lista Lst.
:- begin_tests(finds).
test(t0) :- find_sint([a, b, c], a).
test(t1, fail) :- find_sint([a, b, c], d).
test(t2) :- find_sint([[asma, 10, 0, 1, 0], [osteoporose, 5, 0, 1, 0]], [asma, 10, 0, 1, 0]).
:- end_tests(finds).

find_sint([F | _], F) :- !.
find_sint([_ | R], X) :-
    find_sint(R, X).


% trim(+Str, ?R)
% é true se R é um átomo em que seu conteúdo é a string Str sem espaços a esquerda e a direita.
:- begin_tests(trimstring).
test(t0) :- trim("febre amarela  ", 'febre amarela').
test(t1) :- trim("   febre amarela", 'febre amarela').
test(t2) :- trim("   febre amarela  ", 'febre amarela').
:- end_tests(trimstring).

trim(Str, Result) :-
    string(Str),
    string_chars(Str, List),
    triml(List, Tl),
    trimr(Tl, Trimed),
    atomic_list_concat(Trimed, '', Result).

triml([], []).
triml([F | R], Trimed) :-
    (code_type(F, space) -> triml(R, Trimed) ; append([F], R, Trimed)).

trimr([], []).
trimr([F | R], Trimed) :-
    trimr(R, Rtrim),
    length(Rtrim, Len),
    (Len > 0 -> append([F], Rtrim, Trimed)
    ;   (code_type(F, space) -> Trimed = Rtrim ; append([F], Rtrim, Trimed))).


