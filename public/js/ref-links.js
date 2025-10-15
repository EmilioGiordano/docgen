(function(){
  document.addEventListener('DOMContentLoaded', function(){
    indexModulesById();
    linkifyConditionReferences();
  });

  // Asigna id="mod-<N>" a cada módulo hoja cuyo texto empieza con "[N]"
  function indexModulesById(){
    const lis = document.querySelectorAll('li');
    lis.forEach(li=>{
      const first = li.firstChild;
      const raw = (first && first.nodeType===Node.TEXT_NODE) ? first.nodeValue.trim() : '';
      const m = raw.match(/^\[(\d+)\]/);
      if(m){
        li.id = `mod-${m[1]}`;
      }
    });
  }

  // Marca referencias en condiciones (e.g., "1.attachments[]...") y permite navegar
  function linkifyConditionReferences(){
    const doc = document.querySelector('.doc') || document;

    // Señal visual y tooltip para referencias detectadas
    document.querySelectorAll('code.cond-left, code.cond-right').forEach(code=>{
      const ref = extractLeadingModuleRef(code.textContent);
      if(ref){
        code.classList.add('ref');
        code.setAttribute('title', `Ir al módulo #${ref}`);
      }
    });

    // Delegación de eventos: click para desplazarse al módulo
    doc.addEventListener('click', function(ev){
      const el = ev.target;
      if(!(el instanceof Element)) return;
      if(!el.matches('code.cond-left, code.cond-right')) return;
      const refId = extractLeadingModuleRef(el.textContent);
      if(!refId) return;
      const target = document.getElementById(`mod-${refId}`);
      if(!target) return;
      const controls = document.querySelector('.controls');
      const offset = (controls ? controls.offsetHeight : 0) + 12; // compensar barra sticky
      const y = target.getBoundingClientRect().top + window.pageYOffset - offset;
      window.scrollTo({ top: y, behavior: 'smooth' });
      // activar efecto tras un pequeño delay para coincidir con el fin del scroll
      setTimeout(()=>{
        target.classList.add('flash-target');
        setTimeout(()=> target.classList.remove('flash-target'), 4500);
      }, 220);
    });
  }

  // Extrae el número de módulo al inicio de la expresión, p.ej. "9.`$1`" o "1.attachments[]"
  function extractLeadingModuleRef(text){
    if(!text) return null;
    const m = String(text).trim().match(/^(\d+)\./);
    return m ? m[1] : null;
  }
})();


