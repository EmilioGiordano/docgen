:- module(docgen_html, [main/2]).
:- use_module(library(lists)).

:- dynamic module/4, router/4, path/4.
:- discontiguous module/4, router/4, path/4.

main(Facts, Out) :-
  load_facts(Facts),
  open(Out, write, S, [encoding(utf8)]),
  render_html(S),
  close(S).

load_facts(File) :-
  retractall(module(_,_,_,_)),
  retractall(router(_,_,_,_)),
  retractall(path(_,_,_,_)),
  ensure_loaded(File).

render_html(S) :-
  format(S, "<!doctype html>~n", []),
  format(S, "<html>~n", []),
  format(S, "<head>~n", []),
  format(S, "<meta charset=\"utf-8\">~n", []),
  scenario_title_tag(S),
  format(S, "<link rel=\"stylesheet\" href=\"file:///C:/Users/giord/Desktop/docgen/public/css/theme-light.css\">~n", []),
  format(S, "<link rel=\"stylesheet\" href=\"file:///C:/Users/giord/Desktop/docgen/public/css/ref-links.css\">~n", []),
  format(S, "</head>~n", []),
  format(S, "<script src=\"file:///C:/Users/giord/Desktop/docgen/public/js/toc-toggle.js\" defer></script>~n", []),
  format(S, "<script src=\"file:///C:/Users/giord/Desktop/docgen/public/js/ref-links.js\" defer></script>~n", []),
  format(S, "<body>~n", []),
  scenario_header(S),
  format(S, "<ul>~n", []),

  forall(pre_root_module(Id), node_li(S, Id)),

  forall( (router(Rid, Type, Name, Attrs), \+ appears_in_any_path_router(Rid)),
          router_li(S, Rid, Type, Name, Attrs) ),

  format(S, "</ul>~n", []),
  format(S, "</body>~n", []),
  format(S, "</html>~n", []).

pre_root_module(Id) :-
  module(Id, _, _, _),
  \+ appears_in_any_path_id(Id).

appears_in_any_path_id(Id) :-
  path(_, _, _, Nodes),
  memberchk(Id, Nodes).

appears_in_any_path_router(Rid) :-
  path(_, _, _, Nodes),
  memberchk(rid(Rid), Nodes).

has_paths(Rid) :- path(Rid, _, _, _).

node_li(S, Id) :-
  module(Id, ModType, Name, _),
  nz(Name, Nice),
  format(S, "<li>[~w] ~w - <code>~w</code></li>~n", [Id, Nice, ModType]).

router_li(S, Rid, Type, Name, _Attrs) :-
  nz(Name, Nice),
  format(S, "<li>ROUTER [~w] ~w", [Rid, Nice]),
  ( has_paths(Rid) ->
      format(S, "<ul>~n", []),
      render_router_paths(S, Rid),
      format(S, "</ul>", [])
    ; true ),
  format(S, "</li>~n", []).

render_router_paths(S, Rid) :-
  forall(path(Rid, Label, Cond, Nodes),
    ( format(S, "<li><strong>Ruta: ~w</strong>", [Label]),
      ( Cond == none ; Cond == [] -> true
      ; format(S, " - <em>Condicion:</em> ~w", [Cond]) ),
      format(S, "</li>~n<ul>~n", []),
      render_nodes_list(S, Nodes),
      format(S, "</ul>~n", [])
    )).

render_nodes_list(_, []) :- !.
render_nodes_list(S, [rid(SubRid)|T]) :- !,
  router(SubRid, Tp, Nm, Attrs),
  router_li(S, SubRid, Tp, Nm, Attrs),
  render_nodes_list(S, T).
render_nodes_list(S, [Id|T]) :-
  node_li(S, Id),
  render_nodes_list(S, T).

nz("", "(sin nombre)") :- !.
nz(X, X).

% -- Optional scenario support --
% If a fact scenario/1 exists (e.g., scenario('Name - CODE')), add <title> and a header.
scenario_title_tag(S) :-
  ( current_predicate(scenario/1), scenario(Full), scenario_parts(Full, Name, _Code)
  -> format(S, "<title>~w</title>~n", [Name])
  ; true ).

scenario_header(S) :-
  ( current_predicate(scenario/1), scenario(Full), scenario_parts(Full, Name, Code)
  -> format(S, "<div class=\"scenario-title\" data-scenario-id=\"~w\">~n", [Code]),
     format(S, "  <span class=\"scenario-badge\">SCENARIO</span>~n", []),
     format(S, "  <span class=\"scenario-name\">~w</span>~n", [Name]),
     ( Code \= "" -> format(S, "  <span class=\"scenario-code\">— ~w</span>~n", [Code]) ; true ),
     format(S, "</div>~n", [])
  ; true ).

% scenario_parts('Name - CODE', 'Name', 'CODE'). If no code, return Code="".
scenario_parts(Full, Name, Code) :-
  atom(Full),
  ( atomic_list_concat(Parts, ' - ', Full), Parts = [Only]
  -> Name = Only, Code = ""
  ; atomic_list_concat(Parts, ' - ', Full), append(NameParts, [Code], Parts), atomic_list_concat(NameParts, ' - ', Name)
  ).
