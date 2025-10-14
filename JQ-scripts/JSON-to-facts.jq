# ---------- helpers ----------
def q($s): "'" + ($s|tostring|gsub("'"; "\\'")) + "'";

def cond_term:
  "cond(" + q(.a // "") + "," + q(.o // "") + "," + q(.b // "") + ")";

def conds_to_prolog:
  ( .conditions // [] ) as $C
  | if ($C|length)==0 then "[]"
    else
      "[" +
      ( $C
        | map("[" + ( map(cond_term) | join(", ") ) + "]")
        | join(", ")
      )
      + "]"
    end;

def nodes_list_to_prolog:
  "[" +
  ( ( .nodes // [] )
    | map(
        if (has("paths")) then "rid(" + (.id|tostring) + ")"
        else (.id|tostring)
        end
      )
    | join(", ")
  ) + "]";

# recorrer recursivamente el árbol
def all_objects:
  recurse(.[]? // empty) | objects;

# ---------- emitters ----------
# Todos los módulos (routers incluidos) como module/4
def emit_modules:
  ( [ all_objects
      | select(has("id") and has("module"))
      | { id, module, name: (.name // "") }
    ]
    | unique_by(.id) | sort_by(.id)
  )
  | map("module(" + (.id|tostring) + ", " + q(.module) + ", " + q(.name) + ", []).")
  | .[];

# Solo routers como router/4
def emit_routers:
  ( [ all_objects
      | select(.module == "builtin:BasicRouter" and has("id"))
      | { id, module, name: (.name // "") }
    ]
    | unique_by(.id) | sort_by(.id)
  )
  | map("router(" + (.id|tostring) + ", " + q(.module) + ", " + q(.name) + ", []).")
  | .[];

# Paths (usa el orden por index si existe; si no, el orden en el array)
def emit_paths:
  all_objects
  | select(.module=="builtin:BasicRouter" and has("paths"))
  | . as $r
  | ($r.paths // [])
  | ( if (.[0]? | has("index")) then sort_by(.index) else . end )
  | .[]
  | "path(" + ($r.id|tostring) + ", "
           + q(.route) + ", "
           + ( . | conds_to_prolog ) + ", "
           + ( . | nodes_list_to_prolog )
           + ").";

# ---------- salida ----------
(emit_modules, emit_routers, emit_paths)