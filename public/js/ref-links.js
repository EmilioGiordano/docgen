(function(){
  document.addEventListener('DOMContentLoaded', function(){
    indexModulesById();
    linkifyConditionReferences();
    moveScenarioTitleIntoDoc();
  });

  // Assign id="mod-<N>" to each leaf module whose text starts with "[N]"
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

  // Mark references in conditions (e.g., "1.attachments[]...") and enable navigation
  function linkifyConditionReferences(){
    const doc = document.querySelector('.doc') || document;

    // Visual cue and tooltip for detected references
    document.querySelectorAll('code.cond-left, code.cond-right').forEach(code=>{
      const ref = extractLeadingModuleRef(code.textContent);
      if(ref){
        code.classList.add('ref');
        code.setAttribute('title', `Ir al módulo #${ref}`);
      }
    });

    // Event delegation: click to scroll to the module
    doc.addEventListener('click', function(ev){
      const el = ev.target;
      if(!(el instanceof Element)) return;
      if(!el.matches('code.cond-left, code.cond-right')) return;
      const refId = extractLeadingModuleRef(el.textContent);
      if(!refId) return;
      const target = document.getElementById(`mod-${refId}`);
      if(!target) return;
      const controls = document.querySelector('.controls');
      const offset = (controls ? controls.offsetHeight : 0) + 12; // compensate for sticky bar
      const y = target.getBoundingClientRect().top + window.pageYOffset - offset;
      window.scrollTo({ top: y, behavior: 'smooth' });
      // activate highlight effect after a short delay to align with scroll end
      setTimeout(()=>{
        target.classList.add('flash-target');
        setTimeout(()=> target.classList.remove('flash-target'), 4500);
      }, 220);
    });
  }

  // Move scenario header (if present) into the .doc container as the top element
  function moveScenarioTitleIntoDoc(){
    const title = document.querySelector('.scenario-title');
    if(!title) return;
    const doc = document.querySelector('.doc');
    if(!doc) return;
    doc.prepend(title);
  }

  // Extract the module number at the start of the expression, e.g., "9.`$1`" or "1.attachments[]"
  function extractLeadingModuleRef(text){
    if(!text) return null;
    const m = String(text).trim().match(/^(\d+)\./);
    return m ? m[1] : null;
  }
})();


