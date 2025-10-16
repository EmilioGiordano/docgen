// JavaScript minimalista para DocGen
document.addEventListener('DOMContentLoaded', function() {
    const fileInput = document.getElementById('file-input');
    const fileInputText = document.querySelector('.file-input-text');
    const submitButton = document.querySelector('.submit-button');
    const buttonText = document.querySelector('.button-text');

    if (fileInput && fileInputText) {
        fileInput.addEventListener('change', function(e) {
            if (e.target.files.length > 0) {
                const fileName = e.target.files[0].name;
                fileInputText.textContent = fileName;
                
                // Validar extensión
                if (!fileName.toLowerCase().endsWith('.json')) {
                    fileInputText.textContent = '⚠️ Archivo debe ser .json';
                    fileInputText.style.color = '#ef4444';
                } else {
                    fileInputText.style.color = '#10b981';
                }
            } else {
                fileInputText.textContent = 'Seleccionar archivo JSON';
                fileInputText.style.color = '#6b7280';
            }
        });
    }

    // Estado de procesamiento
    if (submitButton && buttonText) {
        submitButton.addEventListener('click', function(e) {
            const form = this.closest('form');
            if (form && form.checkValidity()) {
                buttonText.textContent = 'Procesando...';
                this.style.pointerEvents = 'none';
                this.style.opacity = '0.7';
            }
        });
    }
});
