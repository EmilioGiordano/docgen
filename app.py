from fastapi import FastAPI, UploadFile, File, HTTPException, Request
from fastapi.responses import FileResponse, HTMLResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from pathlib import Path
import uvicorn
from pipeline import run_pipeline, PipelineError, clean_runs


app = FastAPI(title="DocGen Pipeline")
ROOT = Path(__file__).parent.resolve()

# Configurar archivos estáticos y templates
app.mount("/static", StaticFiles(directory="static"), name="static")
templates = Jinja2Templates(directory="templates")


@app.get("/", response_class=HTMLResponse)
async def index(request: Request):
    return templates.TemplateResponse("index.html", {"request": request})

@app.post("/build")
async def build(file: UploadFile = File(...)):
    if not file.filename.lower().endswith(".json"):
        raise HTTPException(status_code=400, detail="El archivo debe ser .json")
    try:
        data = await file.read()

        # usar el nombre del archivo como título (sin extensión)
        blueprint_title = Path(file.filename).stem

        out_html = run_pipeline(
            blueprint_bytes=data,
            workdir=ROOT,
            blueprint_title=blueprint_title,  # <-- nuevo arg
        )
        # Remover prefijos tipo "<numero>-" solo para la descarga del usuario
        import re
        download_name = re.sub(r"^\d+-", "", out_html.name, count=1)
        return FileResponse(path=out_html, filename=download_name, media_type="text/html")
    except PipelineError as e:
        raise HTTPException(status_code=500, detail=str(e))


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)