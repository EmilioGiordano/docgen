def route_label($idx; $r):
  ($r.flow[0].filter.name? // ("Ruta " + (["A","B","C","D","E","F","G","H","I","J"][$idx])));

def route_conditions($r):
  ($r.flow[0].filter.conditions? // []);

def node($n):
  if $n.module == "builtin:BasicRouter" then
    {
      id: $n.id,
      module: $n.module,
      name: ($n.metadata.designer.name? // "Router"),
      paths: (
        ($n.routes // [])
        | to_entries
        | map({
            route: route_label((.key | tonumber); .value),
            index: ((.key | tonumber) + 1),
            conditions: route_conditions(.value),
            nodes: ((.value.flow // []) | map(node(.)))
          })
        | sort_by(.index)
      )
    }

  elif $n.module == "util:SetVariables" then
    {
      id: $n.id,
      module: $n.module,
      name: ($n.metadata.designer.name? // "Set Variables"),
      variables: (
        ($n.mapper.variables? // [])
        | map({ name: (.name // ""), value: (.value // "") })
      )
    }

  elif $n.module == "json:ParseJSON" then
    {
      id: $n.id,
      module: $n.module,
      name: ($n.metadata.designer.name? // "Parse JSON"),
      JSONstring: ($n.mapper.json? // "")
    }

  elif $n.module == "http:ActionSendData" then
    {
      id: $n.id,
      module: $n.module,
      name: ($n.metadata.designer.name? // "HTTP"),
      method: (($n.mapper.method? | ascii_downcase) // ""),
      url: ($n.mapper.url? // ""),
      headers: ($n.mapper.headers? // []),
      qs: ($n.mapper.qs? // []),
      contentType: ($n.mapper.contentType? // ""),
      bodyType: ($n.mapper.bodyType? // ""),
      data: ($n.mapper.data? // ""),
      timeout: ($n.mapper.timeout? // ""),
      parseResponse: ($n.mapper.parseResponse? // false)
    }

  elif $n.module == "http:ActionSendDataBasicAuth" then
    {
      id: $n.id,
      module: $n.module,
      name: ($n.metadata.designer.name? // "HTTP Basic Auth"),
      method: (($n.mapper.method? | ascii_downcase) // ""),
      url: ($n.mapper.url? // ""),
      headers: ($n.mapper.headers? // []),
      qs: ($n.mapper.qs? // []),
      contentType: ($n.mapper.contentType? // ""),
      bodyType: ($n.mapper.bodyType? // ""),
      data: ($n.mapper.data? // ""),
      timeout: ($n.mapper.timeout? // ""),
      parseResponse: ($n.mapper.parseResponse? // false),
      authLabel: ($n.metadata.restore.parameters.key.label? // "")
    }

  else
    { id: $n.id, module: $n.module, name: ($n.metadata.designer.name? // "") }
  end;

[
  (.flow // [])[] | node(.)
]