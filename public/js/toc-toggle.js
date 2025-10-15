(function(){
    // 1) Wrap the root UL inside a nice container (doesn't modify content)
    document.addEventListener('DOMContentLoaded', function(){
      const rootUL = document.querySelector('body > ul');
      if(!rootUL) return;
      // Create container if it doesn't exist
      let container = document.querySelector('.container');
      if(!container){
        container = document.createElement('div');
        container.className = 'container';
        document.body.prepend(container);
      }
      // Create card-doc and move UL inside
      const doc = document.createElement('div');
      doc.className = 'doc';
      container.appendChild(doc);
      // Global controls
      addGlobalControls(doc);
      doc.appendChild(rootUL);
  
      // 2) Mark routers and routes, add toggle button
      enhanceToggles(rootUL);
    });
  
    function enhanceToggles(root) {
      // A) ROUTER: <li> whose first text starts with "ROUTER ["
      const liAll = root.querySelectorAll('li');
      liAll.forEach(li=>{
        const firstNode = li.firstChild;
        const text = (firstNode && firstNode.nodeType===Node.TEXT_NODE) ? firstNode.nodeValue.trim() : '';
        const isRouter = text.startsWith('ROUTER [');
  
        // B) Route: <li> containing <strong> whose text starts with "Ruta:"
        const strong = li.querySelector(':scope > strong');
        const isRoute = strong && strong.textContent.trim().startsWith('Ruta');
  
        if(isRouter){
          // Structure: li -> "ROUTER [id] label" + optional <ul> children
          const childUL = nextDirectUL(li);
          const row = wrapHeader(li, text, 'router-title'); // removes initial text
          addToggle(row, childUL);
          if(childUL) childUL.classList.add('block');
        } else if(isRoute){
          const childUL = nextDirectUL(li);
          const row = wrapHeader(li, strong, 'route-title'); // moves <strong> to header
          addToggle(row, childUL);
          if(childUL) childUL.classList.add('block');
          // Minimal conditions
          enhanceConditions(li, row);
        } else {
          // Leaf (simple module): give subtle styling
          li.classList.add('leaf');
        }
      });
    }

    // Find "Condicion:" or "Condición:" and move it to a <details> block
    function enhanceConditions(li, headerRow){
      const em = li.querySelector(':scope > em');
      if(!em) return;
      const label = (em.textContent || '').trim().toLowerCase();
      if(label !== 'condicion:' && label !== 'condición:') return;

      // Remove residual dash " - " or other text between the header and the <em>
      let probe = headerRow.nextSibling;
      while(probe && probe !== em){
        const next = probe.nextSibling;
        if(probe.nodeType === Node.TEXT_NODE){
          const txt = (probe.nodeValue || '').trim();
          if(txt === '-' || txt === '') li.removeChild(probe);
        }
        probe = next;
      }

      // Gather the expression text following the <em>
      let expr = '';
      let n = em.nextSibling;
      while(n){
        if(n.nodeType === Node.ELEMENT_NODE && n.tagName === 'UL') break;
        if(n.nodeType === Node.TEXT_NODE) expr += n.nodeValue;
        const toRemove = n;
        n = n.nextSibling;
        li.removeChild(toRemove);
      }
      // Remove the <em> itself
      em.remove();
      expr = (expr || '').trim();
      if(!expr) return;

      // Create minimal <details> block
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

      // Insert right after the header
      headerRow.after(details);
    }

    // Try to parse expressions like: and([cond(...),cond(...),...]) or or([...])
    function tryParseConditions(expr){
      if(!expr) return null;
      const logicMatch = expr.match(/^(and|or)\s*\(/i);
      if(!logicMatch) return null;
      const logic = logicMatch[1].toUpperCase();
      // Extract all cond(...) in a simple way
      const condMatches = expr.match(/cond\s*\(([^)]*)\)/gi);
      if(!condMatches || !condMatches.length) return null;
      const items = condMatches.map(raw=>{
        const inner = raw.replace(/^cond\s*\(|\)$/gi,'');
        // Split by top-level commas (not very strict, but sufficient)
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

    // Global toolbar: expand/collapse all
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
  
    // Return the direct sibling <ul> (or the first immediate one) that contains the children of that li
    function nextDirectUL(li){
      // It may be as li > ul or as the next sibling (depending on how it was generated)
      let ul = li.querySelector(':scope > ul');
      if(ul) return ul;
      let n = li.nextSibling;
      while(n && n.nodeType===Node.TEXT_NODE) n = n.nextSibling;
      if(n && n.tagName==='UL') return n;
      return null;
    }
  
    // Wrap the header in .toggle-row
    // If 'label' is a string -> create span; if it's an element (e.g., <strong>) move it inside.
    function wrapHeader(li, label, titleClass){
      const row = document.createElement('div');
      row.className = 'toggle-row';
  
      const btn = document.createElement('button');
      btn.className = 'toggle-btn';
      btn.setAttribute('data-open','true');
  
      const title = document.createElement('span');
      title.className = titleClass;
  
      if(typeof label === 'string'){
        // Insert badge if we detect the pattern "ROUTER [id]"
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
        // clear original li text
        li.firstChild && li.removeChild(li.firstChild);
      } else {
        // label is an element (e.g., <strong>Ruta: ...>)
        title.appendChild(label);
      }
  
      row.appendChild(btn);
      row.appendChild(title);
      li.prepend(row);
      return row;
    }
  
    // Add collapsing functionality
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
  