(function(){
    // 1) Envolver el UL raíz dentro de un contenedor bonito (no modifica contenido)
    document.addEventListener('DOMContentLoaded', function(){
      const rootUL = document.querySelector('body > ul');
      if(!rootUL) return;
      // Crear container si no existe
      let container = document.querySelector('.container');
      if(!container){
        container = document.createElement('div');
        container.className = 'container';
        document.body.prepend(container);
      }
      // Crear card-doc y mover UL adentro
      const doc = document.createElement('div');
      doc.className = 'doc';
      container.appendChild(doc);
      // Controles globales
      addGlobalControls(doc);
      doc.appendChild(rootUL);
  
      // 2) Marcar routers y rutas, añadir botón toggle
      enhanceToggles(rootUL);
    });
  
    function enhanceToggles(root) {
      // A) ROUTER: <li> cuyo primer texto empieza con "ROUTER ["
      const liAll = root.querySelectorAll('li');
      liAll.forEach(li=>{
        const firstNode = li.firstChild;
        const text = (firstNode && firstNode.nodeType===Node.TEXT_NODE) ? firstNode.nodeValue.trim() : '';
        const isRouter = text.startsWith('ROUTER [');
  
        // B) Ruta: <li> que contenga <strong> cuyo texto empieza con "Ruta:"
        const strong = li.querySelector(':scope > strong');
        const isRoute = strong && strong.textContent.trim().startsWith('Ruta');
  
        if(isRouter){
          // Estructura: li -> "ROUTER [id] label" + (opcional) <ul> hijos
          const childUL = nextDirectUL(li);
          const row = wrapHeader(li, text, 'router-title'); // remueve texto inicial
          addToggle(row, childUL);
          if(childUL) childUL.classList.add('block');
        } else if(isRoute){
          const childUL = nextDirectUL(li);
          const row = wrapHeader(li, strong, 'route-title'); // mueve <strong> a cabecera
          addToggle(row, childUL);
          if(childUL) childUL.classList.add('block');
          // Condiciones minimalistas
          enhanceConditions(li, row);
        } else {
          // Hoja (módulo simple): darle estilo sutil
          li.classList.add('leaf');
        }
      });
    }

    // Busca "Condicion:" o "Condición:" y lo mueve a un bloque <details>
    function enhanceConditions(li, headerRow){
      const em = li.querySelector(':scope > em');
      if(!em) return;
      const label = (em.textContent || '').trim().toLowerCase();
      if(label !== 'condicion:' && label !== 'condición:') return;

      // Recolectar el texto de la expresión a continuación del <em>
      let expr = '';
      let n = em.nextSibling;
      while(n){
        if(n.nodeType === Node.ELEMENT_NODE && n.tagName === 'UL') break;
        if(n.nodeType === Node.TEXT_NODE) expr += n.nodeValue;
        const toRemove = n;
        n = n.nextSibling;
        li.removeChild(toRemove);
      }
      // Limpiar el propio <em>
      em.remove();
      expr = (expr || '').trim();
      if(!expr) return;

      // Crear bloque <details> minimal
      const details = document.createElement('details');
      details.className = 'condition';
      const summary = document.createElement('summary');
      summary.textContent = 'Condición';
      details.appendChild(summary);

      const parsed = tryParseConditions(expr);
      if(parsed){
        const list = document.createElement('div');
        list.className = 'condition-list';
        parsed.items.forEach((item, idx)=>{
          if(idx>0){
            const sep = document.createElement('div');
            sep.className = 'logic-sep';
            sep.textContent = parsed.logic;
            list.appendChild(sep);
          }

          const row = document.createElement('div');
          row.className = 'cond-item';

          const left = document.createElement('code');
          left.className = 'cond-left';
          left.textContent = item.left;

          const op = document.createElement('span');
          op.className = 'op-badge';
          op.textContent = item.operatorLabel || item.operator;

          row.appendChild(left);
          row.appendChild(op);

          if(item.right){
            const right = document.createElement('code');
            right.className = 'cond-right';
            right.textContent = item.right;
            row.appendChild(right);
          }

          list.appendChild(row);
        });
        details.appendChild(list);
      } else {
        const pre = document.createElement('pre');
        const code = document.createElement('code');
        code.textContent = expr;
        pre.appendChild(code);
        details.appendChild(pre);
      }

      // Insertar justo después del header
      headerRow.after(details);
    }

    // Intenta parsear expresiones del tipo: and([cond(...),cond(...),...]) o or([...])
    function tryParseConditions(expr){
      if(!expr) return null;
      const logicMatch = expr.match(/^(and|or)\s*\(/i);
      if(!logicMatch) return null;
      const logic = logicMatch[1].toUpperCase();
      // Extraer todos los cond(...) de forma simple
      const condMatches = expr.match(/cond\s*\(([^)]*)\)/gi);
      if(!condMatches || !condMatches.length) return null;
      const items = condMatches.map(raw=>{
        const inner = raw.replace(/^cond\s*\(|\)$/gi,'');
        // Separar por comas de primer nivel (no muy estricto, pero suficiente)
        const parts = splitTopLevel(inner, ',');
        const left = (parts[0]||'').trim();
        const op = (parts[1]||'').trim();
        const right = (parts[2]||'').trim();
        return normalizeCond(left, op, right);
      });
      return { logic, items };
    }

    function splitTopLevel(str, sep){
      const out = [];
      let buf = '';
      let depthPar = 0; // ()
      let depthBrk = 0; // []
      let depthBr  = 0; // {}
      for(let i=0;i<str.length;i++){
        const c = str[i];
        if(c==='(') depthPar++;
        else if(c===')') depthPar = Math.max(0, depthPar-1);
        else if(c==='[') depthBrk++;
        else if(c===']') depthBrk = Math.max(0, depthBrk-1);
        else if(c==='{') depthBr++;
        else if(c==='}') depthBr = Math.max(0, depthBr-1);
        if(c===sep && depthPar===0 && depthBrk===0 && depthBr===0){
          out.push(buf);
          buf = '';
          continue;
        }
        buf += c;
      }
      if(buf) out.push(buf);
      return out;
    }

    function normalizeCond(left, op, right){
      const clean = s => s
        .replace(/^\{\{\s*/,'').replace(/\s*\}\}$/,'') // {{ ... }}
        .trim();
      const leftExpr = left ? clean(left) : '';
      const operator = op || 'exist';
      const rightExpr = right ? clean(right) : '';
      const opMap = {
        'exist':'EXISTS',
        'text:equal':'=',
        'number:greater':'>',
        'number:less':'<',
        'text:contain':'CONTAINS',
        'text:regex':'MATCHES'
      };
      const operatorLabel = opMap[operator] || operator;
      if(operator.toLowerCase()==='exist'){
        return { left: leftExpr, operator, operatorLabel };
      }
      return { left: leftExpr, operator, operatorLabel, right: rightExpr };
    }

    // Toolbar global: expandir/contraer todos
    function addGlobalControls(doc){
      const bar = document.createElement('div');
      bar.className = 'controls';

      const btnExpand = document.createElement('button');
      btnExpand.type = 'button';
      btnExpand.className = 'ctrl-btn';
      btnExpand.textContent = 'Expandir todo';
      btnExpand.addEventListener('click', expandAll);

      const btnCollapse = document.createElement('button');
      btnCollapse.type = 'button';
      btnCollapse.className = 'ctrl-btn';
      btnCollapse.textContent = 'Contraer todo';
      btnCollapse.addEventListener('click', collapseAll);

      bar.appendChild(btnExpand);
      bar.appendChild(btnCollapse);
      doc.appendChild(bar);
    }

    function expandAll(){
      const rows = document.querySelectorAll('.toggle-row');
      rows.forEach(row=>{
        const li = row.parentElement;
        const ul = nextDirectUL(li);
        if(!ul) return;
        ul.classList.remove('hidden');
        const btn = row.querySelector('.toggle-btn');
        if(btn) btn.setAttribute('data-open','true');
      });
      document.querySelectorAll('details.condition').forEach(d=>d.open = true);
    }

    function collapseAll(){
      const rows = document.querySelectorAll('.toggle-row');
      rows.forEach(row=>{
        const li = row.parentElement;
        const ul = nextDirectUL(li);
        if(!ul) return;
        ul.classList.add('hidden');
        const btn = row.querySelector('.toggle-btn');
        if(btn) btn.setAttribute('data-open','false');
      });
      document.querySelectorAll('details.condition').forEach(d=>d.open = false);
    }
  
    // Devuelve el <ul> hermano directo (o el primero inmediato) que contenga los hijos de ese li
    function nextDirectUL(li){
      // Puede estar como li > ul o como hermano siguiente (según cómo se generó)
      let ul = li.querySelector(':scope > ul');
      if(ul) return ul;
      let n = li.nextSibling;
      while(n && n.nodeType===Node.TEXT_NODE) n = n.nextSibling;
      if(n && n.tagName==='UL') return n;
      return null;
    }
  
    // Envuelve la cabecera en .toggle-row
    // Si 'label' es string -> crea span; si es elemento (p.ej. <strong>) lo mueve dentro.
    function wrapHeader(li, label, titleClass){
      const row = document.createElement('div');
      row.className = 'toggle-row';
  
      const btn = document.createElement('button');
      btn.className = 'toggle-btn';
      btn.setAttribute('data-open','true');
  
      const title = document.createElement('span');
      title.className = titleClass;
  
      if(typeof label === 'string'){
        // Meter badge si detectamos patrón "ROUTER [id]"
        const m = label.match(/^ROUTER\s+\[(\d+)\]\s*(.*)$/);
        if(m){
          const [ , id, rest ] = m;
          const badge = document.createElement('span');
          badge.className = 'badge';
          badge.textContent = `ROUTER #${id}`;
          title.appendChild(badge);
          if(rest && rest.trim()){
            const space = document.createTextNode(' ');
            title.appendChild(space);
            title.appendChild(document.createTextNode(rest.trim()));
          }
        } else {
          title.textContent = label;
        }
        // limpiar texto original del li
        li.firstChild && li.removeChild(li.firstChild);
      } else {
        // label es un elemento (e.g., <strong>Ruta: ...>)
        title.appendChild(label);
      }
  
      row.appendChild(btn);
      row.appendChild(title);
      li.prepend(row);
      return row;
    }
  
    // Agrega funcionalidad de plegado
    function addToggle(row, contentUL){
      const btn = row.querySelector('.toggle-btn');
      if(!contentUL){ 
        btn.disabled = true; 
        btn.setAttribute('data-open','true');
        return;
      }
      btn.setAttribute('data-open','true');
      btn.addEventListener('click', ()=>{
        const open = btn.getAttribute('data-open') === 'true';
        btn.setAttribute('data-open', open ? 'false' : 'true');
        contentUL.classList.toggle('hidden', open);
      });
    }
  })();
  