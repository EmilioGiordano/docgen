% doctree.pl
% Uso:
%   swipl -q -s doctree.pl -g "main('facts2.pl','doc.html')" -t halt
%
% Hechos esperados en facts2.pl:
%   module(Id, ModuleType, Name, AttrsList).
%   router(Id, ModuleType, Name, AttrsList).
%   path(RouterId, Label, Cond, Nodes).
%     - Nodes = [ Id | rid(SubRouterId) | ... ]
%   AttrsList: lista de términos clave(Valor), ej: [table('invoices'), method('POST')]

:- module(docgen_html, [main/2]).
:- use_module(library(lists)).

:- dynamic module/4, router/4, path/4.
:- discontiguous module/4, router/4, path/4.

% ===== Entrypoint =====
main(Facts, Out) :-
  load_facts(Facts),
  open(Out, write, S, [encoding(utf8)]),
  put_code(S, 0xFEFF),  % BOM UTF-8 (evita mojibake en algunos visores)
  render_html(S),
  close(S).

% ===== Carga de hechos =====
load_facts(File) :-
  retractall(module(_,_,_,_)),
  retractall(router(_,_,_,_)),
  retractall(path(_,_,_,_)),
  ensure_loaded(File).

% ===== Render raíz =====
render_html(S) :-
  format(S, "<!doctype html><html><head><meta charset=\"utf-8\"><title>docgen</title>~n", []),
  css(S),
  format(S, "</head><body>~n<ul class=\"tree\">~n", []),


  forall(pre_root_node(Id),
         node_li(S, Id)),

  % 2) Routers raíz (todos los definidos)
  forall(router(Rid, RType, RName, RAttrs),
         ( format(S, "<li>", []),
           router_card(S, Rid, RType, RName, RAttrs),
           ( has_paths(Rid) ->
               format(S, "<ul>~n", []),
               render_router_paths(S, Rid),
               format(S, "</ul>~n", [])
             ; true ),
           format(S, "</li>~n", [])
         )),

  format(S, "</ul></body></html>~n", []).

% ===== Utilidades para decidir qué mostrar arriba =====
pre_root_node(Id) :-
  module(Id, _, _, _),
  \+ appears_in_any_path(Id).

appears_in_any_path(Id) :-
  path(_, _, _, Nodes),
  memberchk(Id, Nodes).

has_paths(Rid) :- path(Rid, _, _, _).

% ===== CSS inline =====
css(S) :-
  format(S, "<style>~n", []),
  format(S, ".tree, .tree ul {list-style:none;margin:0;padding-left:1rem;position:relative}~n", []),
  format(S, ".tree ul{margin-left:.6rem}.tree li{margin:.35rem 0;padding-left:.85rem;position:relative}~n", []),
  format(S, ".tree li::before{content:\"\";position:absolute;left:0;top:1.05rem;width:.85rem;height:0;border-top:1px solid #c9c9c9}~n", []),
  format(S, ".tree li::after{content:\"\";position:absolute;left:.38rem;top:-.45rem;width:0;height:1.7rem;border-left:1px solid #c9c9c9}~n", []),
  format(S, ".tree>li::after{display:none}~n", []),
  format(S, ".node{display:inline-block;background:#fafafa;border:1px solid #e3e3e3;border-radius:.45rem;padding:.45rem .6rem;min-width:18rem;box-shadow:0 1px 0 rgba(0,0,0,.02)}~n", []),
  format(S, ".node .title{font-weight:600}.node code{background:#f3f3f3;padding:.1rem .3rem;border-radius:.25rem}~n", []),
  format(S, ".router{background:#f6f7fb;border-color:#cfd6ff}.router .badge{display:inline-block;font-size:.75rem;font-weight:700;color:#223;background:#e9edff;border:1px solid #cfd6ff;padding:.05rem .4rem;border-radius:.4rem;margin-left:.35rem}~n", []),
  format(S, ".kv{margin:.35rem 0 0;border-collapse:collapse;width:100%}.kv th,.kv td{font-size:.85rem;padding:.18rem .25rem;border-bottom:1px dashed #e8e8e8}.kv th{text-align:left;color:#666;width:8rem}.kv td{color:#222}~n", []),
  format(S, "details>summary{cursor:pointer;list-style:none;outline:none;display:inline-block;margin-bottom:.35rem}details>summary::-webkit-details-marker{display:none}~n", []),
  format(S, "summary .twisty{display:inline-block;width:.9rem;text-align:center;margin-right:.3rem;transition:transform .15s ease-in-out}details[open]>summary .twisty{transform:rotate(90deg)}~n", []),
  format(S, ".path-label{font-size:.8rem;color:#555;margin-left:.4rem}.cond{font-size:.78rem;color:#666;margin-top:.2rem}~n", []),
  format(S, "</style>~n", []).


node_li(S, Id) :-
  format(S, "<li>", []),
  node_card(S, Id),
  format(S, "</li>~n", []).

node_card(S, Id) :-
  module(Id, MType, Name, Attrs),
  nz(Name, NameNZ),
  format(S, "<div class=\"node\"><div class=\"title\">[~w] ~w — <code>~w</code></div>~n", [Id, NameNZ, MType]),
  attrs_table(S, [id(Id), module(MType), name(Name)|Attrs]),
  format(S, "</div>~n", []).

router_card(S, Rid, RType, RName, Attrs) :-
  nz(RName, RNameNZ),
  format(S, "<div class=\"node router\"><div class=\"title\">ROUTER [~w] ~w <span class=\"badge\">router</span></div>~n", [Rid, RNameNZ]),
  attrs_table(S, [id(Rid), module(RType), name(RName)|Attrs]),
  format(S, "</div>~n", []).

attrs_table(S, Attrs) :-
  format(S, "<table class=\"kv\">~n", []),
  forall(member(KV, Attrs), attr_row(S, KV)),
  format(S, "</table>~n", []).

attr_row(S, Term) :-
  Term =.. [K, V],
  map_key(K, K2),
  term_string(V, Vs),
  format(S, "<tr><th>~w</th><td>~w</td></tr>~n", [K2, Vs]).

map_key(module, 'module') :- !.
map_key(name,   'name')   :- !.
map_key(id,     'id')     :- !.
map_key(K, K).

nz("", "(sin nombre)") :- !.
nz(X, X).

% ===== Render de rutas (plegables) =====
render_router_paths(S, Rid) :-
  forall(path(Rid, Label, Cond, Nodes),
    ( format(S, "<li><details open><summary><span class=\"twisty\">▶</span><strong>~w</strong></summary>~n", [Label]),
      ( Cond == none -> true ; format(S, "<div class=\"cond\">Condición: ~w</div>~n", [Cond]) ),
      format(S, "<ul>~n", []),
      render_nodes_list(S, Nodes),
      format(S, "</ul>~n</details></li>~n", [])
    )).

render_nodes_list(S, []) :- !.
render_nodes_list(S, [rid(SubRid)|T]) :- !,
  format(S, "<li>", []),
  router(SubRid, RT, RN, RAttrs),
  router_card(S, SubRid, RT, RN, RAttrs),
  ( has_paths(SubRid) ->
      format(S, "<ul>~n", []),
      render_router_paths(S, SubRid),
      format(S, "</ul>~n", [])
    ; true ),
  format(S, "</li>~n", []),
  render_nodes_list(S, T).
render_nodes_list(S, [Id|T]) :-
  node_li(S, Id),
  render_nodes_list(S, T).
