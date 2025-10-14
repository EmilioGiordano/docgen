# ---------- helpers ----------

# Prolog atom string with single-quote escaping
def q($s): "'" + ($s|tostring|gsub("'"; "\\'")) + "'";

# Render any JSON value as a Prolog term argument
def val_to_term:
  if type == "string" then q(.)
  elif type == "number" then tostring
  elif type == "boolean" then (if . then "true" else "false" end)
  elif type == "null" then "null"
  elif type == "array" then "[" + (map(val_to_term)|join(", ")) + "]"
  elif type == "object" then q(.|tojson)
  else q(.|tostring)
  end;

# Convierte {k:v} -> "k(v)" preservando tipos
def obj_kv_to_functors:
  to_entries
  | map(.key + "(" + (.value|val_to_term) + ")");

# Devuelve el primer objeto no-vacío entre varias claves conocidas
def first_nonempty_obj($keys):
  reduce $keys[] as $k (null;
    if . == null and (has($k) and (.[$k]|type=="object") and ((.[$k]|length)>0))
    then .[$k] else . end
  ) // {};

# Merge con prioridad entre varias claves típicas de exports
def first_options:
  # prioriza: options > config > params > settings > parameters > fields > args > arguments
  first_nonempty_obj(["options","config","params","settings","parameters","fields","args","arguments"]);

# Lista de opciones Prolog: [key(val), ...] o []
def options_list_to_prolog:
  (first_options) as $O
  | if ($O|type) == "object" then
      "[" + ($O | obj_kv_to_functors | join(", ")) + "]"
    elif ($O|type) == "array" then
      "[" + (
        $O
        | map(
            (if has("key") then .key
             elif has("name") then .name
             else empty end)
            + "(" +
            (if has("value") then (.value|val_to_term) else "null" end)
            + ")"
          )
        | join(", ")
      ) + "]"
    else "[]"
    end;

# Determina el término de condición para path/4:
# - 'texto' si viene como string en .condition / .cond / .expr
# - otherwise si .otherwise == true
# - none si no hay condición
def cond_field:
  if (has("otherwise") and (.otherwise|type=="boolean") and .otherwise) then "otherwise"
  elif (has("condition") and (.condition|type=="string") and ((.condition|length)>0)) then q(.condition)
  elif (has("cond") and (.cond|type=="string") and ((.cond|length)>0)) then q(.cond)
  elif (has("expr") and (.expr|type=="string") and ((.expr|length)>0)) then q(.expr)
  else "none" end;

# Recorre recursivamente el árbol para reunir objetos con campos interesantes
def all_objects:
  recurse(.[]? // empty) | objects;

# Un objeto es router si tiene paths o si su module dice builtin:BasicRouter (o variantes)
def is_router_obj:
  (has("paths") and (.paths|type=="array")) or (.module == "builtin:BasicRouter");

# ---------- precomputados ----------
# Conjunto de IDs de routers para lookup rápido
def router_ids:
  [ all_objects
    | select(has("id") and is_router_obj)
    | .id
  ];

# helper: ¿id ∈ router_ids ?
def is_router_id($RID; $id):
  ($RID | index($id)) != null;

# Lista de nodos con rid(ID) cuando el destino es router
def nodes_list_to_prolog($RID):
  "[" +
  ( ( .nodes // [] )
    | map(
        if type=="number" then
          (if is_router_id($RID; .) then "rid(" + (.|tostring) + ")" else tostring end)
        elif type=="string" then
          # si los IDs vinieran como string
          (if (try (.|tonumber) catch null) as $n
             | ($n!=null and is_router_id($RID; $n))
           then "rid(" + (tostring) + ")" else . end)
        elif (type=="object") and has("id") then
          (if (is_router_id($RID; .id)) then "rid(" + (.id|tostring) + ")" else (.id|tostring) end)
        else empty end
      )
    | join(", ")
  )
  + "]";

# ---------- emitters ----------

# Todos los módulos NO-routers como module/4 con lista de opciones
def emit_modules($RID):
  ( [ all_objects
      | select(has("id") and has("module"))
      | select((is_router_obj|not))         # <- excluir routers aquí
      | { id, module, name: (.name // ""), _opt: (.) }
    ]
    | unique_by(.id) | sort_by(.id)
  )
  | map(
      "module(" + (.id|tostring) + ", " + q(.module) + ", " + q(.name) + ", " +
      (.["_opt"] | options_list_to_prolog) + ")."
    )
  | .[];

# Solo routers como router/4 (sin opciones)
def emit_routers:
  ( [ all_objects
      | select(has("id") and is_router_obj)
      | { id, module, name: (.name // "") }
    ]
    | unique_by(.id) | sort_by(.id)
  )
  | map("router(" + (.id|tostring) + ", " + q(.module) + ", " + q(.name) + ", []).")
  | .[];

# Paths como path/4:
#   path(RouterId, 'Ruta ...', <cond_field>, [nodos])
def emit_paths($RID):
  all_objects
  | select(is_router_obj and has("paths"))
  | . as $r
  | ($r.paths // [])
  | ( if (.[0]? | has("index")) then sort_by(.index) else . end )
  | .[]
  | "path(" + ($r.id|tostring) + ", "
           + q(.route) + ", "
           + ( . | cond_field ) + ", "
           + ( . | nodes_list_to_prolog($RID) )
           + ").";

# ---------- salida ----------
( router_ids ) as $RID
| (emit_modules($RID), emit_routers, emit_paths($RID))
