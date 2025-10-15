# ---------- helpers ----------

def q($s): "'" + ($s|tostring|gsub("'"; "\\'")) + "'";

def val_to_term:
  if type == "string" then q(.)
  elif type == "number" then tostring
  elif type == "boolean" then (if . then "true" else "false" end)
  elif type == "null" then "null"
  elif type == "array" then "[" + (map(val_to_term)|join(", ")) + "]"
  elif type == "object" then q(.|tojson)
  else q(.|tostring)
  end;

def obj_kv_to_functors:
  to_entries
  | map(.key + "(" + (.value|val_to_term) + ")");

def first_nonempty_obj($keys):
  reduce $keys[] as $k (null;
    if . == null and (has($k) and (.[$k]|type=="object") and ((.[$k]|length)>0))
    then .[$k] else . end
  ) // {};

def first_options:
  first_nonempty_obj(["options","config","params","settings","parameters","fields","args","arguments"]);

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

# ---------- conditions ----------

def cond_term:
  "cond(" +
  q(.a // "") + ", " +
  q(.o // "") +
  (if has("b") and .b != null then ", " + q(.b) else "" end) +
  ")";

def group_and:
  "and([" + ( map(cond_term) | join(", ") ) + "])";

def cond_expr:
  if (has("otherwise") and (.otherwise|type=="boolean") and .otherwise) then
    "otherwise"
  elif (has("conditions") and (.conditions|type=="array")) then
    ( .conditions ) as $C
    | if ($C|length) == 0 then "none"
      elif ($C|length) == 1 then ( $C[0] | group_and )
      else "or([" + ( $C | map(group_and) | join(", ") ) + "])"
      end
  elif (has("condition") and (.condition|type=="string") and (.condition|length>0)) then
    q(.condition)
  elif (has("cond") and (.cond|type=="string") and (.cond|length>0)) then
    q(.cond)
  elif (has("expr") and (.expr|type=="string") and (.expr|length>0)) then
    q(.expr)
  else
    "none"
  end;

# ---------- utilities ----------

def all_objects:
  recurse(.[]? // empty) | objects;

def is_router_obj:
  (has("paths") and (.paths|type=="array")) or (.module == "builtin:BasicRouter");

# ---------- precomputed ----------

def router_ids:
  [ all_objects
    | select(has("id") and is_router_obj)
    | .id
  ];

def is_router_id($RID; $id):
  ($RID | index($id)) != null;

def nodes_list_to_prolog($RID):
  "[" +
  ( ( .nodes // [] )
    | map(
        if type=="number" then
          (if is_router_id($RID; .) then "rid(" + (.|tostring) + ")" else tostring end)
        elif type=="string" then
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

def get_scenario_name:
  ( [ all_objects | select(has("scenario-name")) | .["scenario-name"] ]
    | .[0]? // ""
  );

def emit_scenario:
  "scenario(" + q(get_scenario_name) + ").";

def emit_modules($RID):
  ( [ all_objects
      | select(has("id") and has("module"))
      | select((is_router_obj|not))
      | { id, module, name: (.name // ""), _opt: (.) }
    ]
    | unique_by(.id) | sort_by(.id)
  )
  | map(
      "module(" + (.id|tostring) + ", " + q(.module) + ", " + q(.name) + ", " +
      (.["_opt"] | options_list_to_prolog) + ")."
    )
  | .[];

def emit_routers:
  ( [ all_objects
      | select(has("id") and is_router_obj)
      | { id, module, name: (.name // "") }
    ]
    | unique_by(.id) | sort_by(.id)
  )
  | map("router(" + (.id|tostring) + ", " + q(.module) + ", " + q(.name) + ", []).")
  | .[];

def emit_paths($RID):
  all_objects
  | select(is_router_obj and has("paths"))
  | . as $r
  | ($r.paths // [])
  | ( if (.[0]? | has("index")) then sort_by(.index) else . end )
  | .[]
  | "path(" + ($r.id|tostring) + ", "
           + q(.route) + ", "
           + ( . | cond_expr ) + ", "
           + ( . | nodes_list_to_prolog($RID) )
           + ").";

# ---------- output ----------
( router_ids ) as $RID
| (emit_scenario, emit_modules($RID), emit_routers, emit_paths($RID))
