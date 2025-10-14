# 1. Exportar escenario de Make a JSON

# 2. Transformar JSON 

```c
jq -f make-reducer.jq input.json > output.json
```

# 3. Transformar JSON a base de hechos en Prolog

```c
jq -f -r json-to-facts.jq escenario-reducido.json > facts.pl
```

# 4. Procesar base de hechos y generar HTML
```
swipl -q -s .\doctree.pl -g "docgen_html:main('facts.pl','doc.html')" -t halt
```