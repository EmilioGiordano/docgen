# 1. Exportar escenario de Make a JSON

# 2. Transformar JSON 

```bash
jq -f ./jq-scripts/make-reducer.jq ./data/1-blueprint/input.json > ./data/2-reduced-jsons/output.json
```
**Limpiar archivo**
```bash
dos2unix ./data/2-reduced-jsons/output.json
```


# 3. Transformar JSON a base de hechos en Prolog
```c
jq -f -r ./jq-scripts/json-to-facts.jq ./data/2-reduced-jsnos/escenario-reducido.json > ./data/3-facts-base/escenario-facts.pl
```

**Limpiar archivo**
```bash
dos2unix ./data/3-facts-base/escenario-facts.pl
```

# 4. Procesar base de hechos y generar HTML
```
swipl -q -s ./prolog/docgen.pl -g "docgen_html:main('./data/3-facts-base/facts-base-scenario.pl','./out/scenario-page.html')" -t halt
```