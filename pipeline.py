from pathlib import Path
import subprocess
import shutil


class PipelineError(Exception):
    pass


def _ensure_dir(path: Path):
    """Crear directorio si no existe"""
    path.mkdir(parents=True, exist_ok=True)


def _try_dos2unix(file_path: Path):
    """Intentar convertir terminaciones de línea DOS a Unix"""
    try:
        subprocess.run(["dos2unix", str(file_path)], check=True, capture_output=True)
    except (FileNotFoundError, subprocess.CalledProcessError):
        # dos2unix no disponible o falló, continuar sin error
        pass


def run_pipeline(blueprint_bytes: bytes, workdir: Path) -> Path:
    """Ejecutar pipeline completo: JSON → HTML"""
    
    # Rutas de archivos de configuración
    make_reducer_jq = workdir / "jq" / "make-reducer.jq"
    json_to_facts_jq = workdir / "jq" / "json-to-prolog-facts.jq"
    prolog_entry = workdir / "prolog" / "docgen.pl"
    
    # Subcarpetas por etapa
    p1 = workdir / "data/1-blueprint"
    p2 = workdir / "data/2-reduced-jsons"
    p3 = workdir / "data/3-facts-base"
    pout = workdir / "out"
    for p in (p1, p2, p3, pout):
        _ensure_dir(p)
    
    # Guardar blueprint
    input_json = p1 / "input.json"
    input_json.write_bytes(blueprint_bytes)
    
    # Validaciones básicas
    if not make_reducer_jq.exists():
        raise PipelineError(f"Falta {make_reducer_jq}")
    if not json_to_facts_jq.exists():
        raise PipelineError(f"Falta {json_to_facts_jq}")
    if not prolog_entry.exists():
        raise PipelineError(f"Falta {prolog_entry}")
    
    # 2) Reducir JSON con jq
    reduced_json = p2 / "output.json"
    with reduced_json.open("wb") as f:
        try:
            subprocess.run(
                ["jq", "-f", str(make_reducer_jq), str(input_json)],
                check=True,
                stdout=f,
            )
        except FileNotFoundError:
            raise PipelineError("jq no está instalado en el sistema (necesario)")
        except subprocess.CalledProcessError as e:
            raise PipelineError(f"jq (reduce) falló: {e.stderr}")
    
    _try_dos2unix(reduced_json)
    
    # 3) JSON → facts.pl con jq (-r)
    facts_pl = p3 / "escenario-facts.pl"
    with facts_pl.open("wb") as f:
        try:
            subprocess.run(
                ["jq", "-f", "-r", str(json_to_facts_jq), str(reduced_json)],
                check=True,
                stdout=f,
            )
        except subprocess.CalledProcessError as e:
            raise PipelineError(f"jq (facts) falló: {e.stderr}")
    
    _try_dos2unix(facts_pl)
    
    # 4) Prolog → HTML final
    out_html = pout / "scenario-page.html"
    goal = f"docgen_html:main('{facts_pl.as_posix()}','{out_html.as_posix()}')"
    try:
        subprocess.run(
            ["swipl", "-q", "-s", str(prolog_entry), "-g", goal, "-t", "halt"],
            check=True,
            capture_output=True,
        )
    except FileNotFoundError:
        raise PipelineError("swi-prolog (swipl) no está instalado en el sistema")
    except subprocess.CalledProcessError as e:
        raise PipelineError(
            "Prolog falló: " + (e.stderr.decode(errors="ignore") if e.stderr else str(e))
        )
    
    if not out_html.exists():
        raise PipelineError("No se generó el HTML esperado")
    
    return out_html


def clean_runs(project_root: Path, keep_last: int = 5):
    """Limpiar ejecuciones anteriores, manteniendo solo las últimas N"""
    runs_dir = project_root / "runs"
    if not runs_dir.exists():
        return
    entries = sorted([p for p in runs_dir.iterdir() if p.is_dir()])
    for p in entries[:-keep_last]:
        shutil.rmtree(p, ignore_errors=True)