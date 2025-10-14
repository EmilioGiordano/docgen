:- module(docgen_html, [main/2]).
:- use_module(library(lists)).

:- dynamic module/4, router/4, path/4.
:- discontiguous module/4, router/4, path/4.

main(Facts, Out) :-
  load_facts(Facts),
  open(Out, write, S, [encoding(utf8)]),
  put_code(S, 0xFEFF),
  render_html(S),
  close(S).

load_facts(File) :-
  retractall(module(_,_,_,_)),
  retractall(router(_,_,_,_)),
  retractall(path(_,_,_,_)),
  ensure_loaded(File).

render_html(S) :-
  format(S, "<!doctype html><meta charset=\"utf-8\"><ul>~n", []),


  forall(pre_root_module(Id), node_li(S, Id)),

  % 2) routers raíz (no referenciados como rid/1)
  forall( (router(Rid, Type, Name, Attrs), \+ appears_in_any_path_router(Rid)),
          router_li(S, Rid, Type, Name, Attrs) ),

  format(S, "</ul>~n", []).


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
  format(S, "<li>[~w] ~w — <code>~w</code></li>~n", [Id, Nice, ModType]).

router_li(S, Rid, Type, Name, _Attrs) :-
  nz(Name, Nice),
  % abrimos el <li> del router
  format(S, "<li>ROUTER [~w] ~w", [Rid, Nice]),
  ( has_paths(Rid) ->
      % hijos (rutas) DENTRO del mismo <li>
      format(S, "<ul>~n", []),
      render_router_paths(S, Rid),
      format(S, "</ul>", [])
    ; true ),
  % cerramos el <li> del router
  format(S, "</li>~n", []).

render_router_paths(S, Rid) :-
  forall(path(Rid, Label, Cond, Nodes),
    ( % abrimos el <li> de la ruta
      format(S, "<li><strong>Ruta: ~w</strong>", [Label]),
      ( Cond == none ; Cond == [] -> true
      ; format(S, " — <em>Condición:</em> ~w", [Cond]) ),
      % hijos (nodos y/o sub-routers) DENTRO del mismo <li>
      format(S, "<ul>~n", []),
      render_nodes_list(S, Nodes),
      format(S, "</ul></li>~n", [])
    )).

render_nodes_list(_, []) :- !.
render_nodes_list(S, [rid(SubRid)|T]) :- !,
  router(SubRid, Tp, Nm, Attrs),
  % imprimimos el sub-router AQUÍ (como hijo de la ruta actual)
  router_li(S, SubRid, Tp, Nm, Attrs),
  render_nodes_list(S, T).
render_nodes_list(S, [Id|T]) :-
  node_li(S, Id),
  render_nodes_list(S, T).

nz("", "(sin nombre)") :- !.
nz(X, X).
