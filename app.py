from fastapi import FastAPI, UploadFile, File, HTTPException
from fastapi.responses import FileResponse, HTMLResponse
from pathlib import Path
import uvicorn
from pipeline import run_pipeline, PipelineError, clean_runs


app = FastAPI(title="DocGen Pipeline")
ROOT = Path(__file__).parent.resolve()


HTML_FORM = """
<!doctype html>
<html>
<head><meta charset="utf-8"><title>DocGen</title></head>
<body style="font-family: system-ui; max-width: 720px; margin: 40px auto;">
<h1>Subir blueprint (.json)</h1>
<form action="/build" method="post" enctype="multipart/form-data">
<input type="file" name="file" accept="application/json" required />
<button type="submit">Procesar</button>
</form>
</body>
</html>
"""


@app.get("/", response_class=HTMLResponse)
async def index():
    return HTML_FORM


@app.post("/build")
async def build(file: UploadFile = File(...)):
    if not file.filename.lower().endswith(".json"):
        raise HTTPException(status_code=400, detail="El archivo debe ser .json")
    try:
        data = await file.read()
        out_html = run_pipeline(data, ROOT)
        # Limpieza opcional (mantener solo últimas ejecuciones)
        clean_runs(ROOT, keep_last=10)
        return FileResponse(path=out_html, filename=out_html.name, media_type="text/html")
    except PipelineError as e:
        raise HTTPException(status_code=500, detail=str(e))


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)