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
        } else {
          // Hoja (módulo simple): darle estilo sutil
          li.classList.add('leaf');
        }
      });
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
  