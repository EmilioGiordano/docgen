% docgen.pl
% Uso:
%   swipl -q -s docgen.pl -g "main('facts.pl','doc.md')" -t halt
%
% Espera un archivo de hechos con términos:
%   module(Id, ModuleAtom, NameString).
%   ej.: module(118, 'nocodb:create', "Insert order").

:- module(docgen, [main/2]).
:- use_module(library(lists)).

:- dynamic module/3.

main(FactsFile, OutMarkdown) :-
    load_facts(FactsFile),
    open(OutMarkdown, write, S, [encoding(utf8)]),
    put_code(S, 0xFEFF),
    render_markdown(S),
    close(S).

load_facts(F) :-
    retractall(module(_,_,_)),
    ensure_loaded(F).

render_markdown(S) :-
    format(S, "# Documentacion (muy simple)~n~n", []),
    format(S, "## Módulos~n~n", []),
    list_modules(S).

list_modules(S) :-
    findall((Id, Mod, Name), module(Id, Mod, Name), Raw),
    sort(Raw, Mods),
    ( Mods == [] ->
        format(S, "_No hay módulos._~n", [])
    ;   forall(member((Id, Mod, Name0), Mods),
           ( pretty_name(Name0, Name),
             format(S, "- **[~w]** `~w` - ~w~n", [Id, Mod, Name])
           )
        ),
        nl(S)
    ).

pretty_name("", "(sin nombre)") :- !.
pretty_name(Name, Name).
