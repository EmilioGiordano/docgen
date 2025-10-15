from pathlib import Path
import subprocess
import shutil
import unicodedata
import re
from datetime import datetime


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


def _sanitize_name(name: str) -> str:
    # Normaliza acentos → ASCII legible (opcional pero práctico)
    s = unicodedata.normalize("NFKD", name)
    s = "".join(c for c in s if not unicodedata.combining(c))

    # Quitar caracteres problemáticos en Windows y PowerShell:
    #  <>:"/\|?*   +  backtick `   +  tabs/nuevas líneas
    s = re.sub(r'[<>:"/\\|?*\t\r\n`]', '-', s)

    # Quitar dobles/triples guiones y colapsar espacios
    s = re.sub(r'\s+', ' ', s).strip()
    s = re.sub(r'-{2,}', '-', s)

    # Evitar trailing dots/spaces
    s = s.rstrip(' .')

    # Evitar nombres reservados (CON, PRN, etc.)
    RESERVED = {'CON','PRN','AUX','NUL','COM1','COM2','COM3','COM4','COM5','COM6','COM7','COM8','COM9',
                'LPT1','LPT2','LPT3','LPT4','LPT5','LPT6','LPT7','LPT8','LPT9'}
    if s.upper() in RESERVED:
        s += "-file"

    return s[:150]


def run_pipeline(blueprint_bytes: bytes, workdir: Path, blueprint_title: str) -> Path:
    """Ejecutar pipeline completo: JSON → HTML para un blueprint.
    Crea data/<nombre>/ con todos los artefactos con el mismo prefijo.
    """
    # Rutas de archivos de configuración (dentro del repo/app)
    make_reducer_jq = workdir / "jq" / "make-reducer.jq"
    json_to_facts_jq = workdir / "jq" / "json-to-prolog-facts.jq"
    prolog_entry = workdir / "prolog" / "docgen.pl"

    # Validaciones básicas
    if not make_reducer_jq.exists():
        raise PipelineError(f"Falta {make_reducer_jq}")
    if not json_to_facts_jq.exists():
        raise PipelineError(f"Falta {json_to_facts_jq}")
    if not prolog_entry.exists():
        raise PipelineError(f"Falta {prolog_entry}")

    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    base_name = _sanitize_name(blueprint_title)
    scenario_dir = workdir / "data" / f"{timestamp}_{base_name}"
    scenario_dir.mkdir(parents=True, exist_ok=True)

    input_json   = scenario_dir / f"1-{base_name}.blueprint.json"
    reduced_json = scenario_dir / f"2-{base_name}-reduced.json"
    facts_pl     = scenario_dir / f"3-{base_name}-facts.pl"
    out_html     = scenario_dir / f"4-{base_name}-document.html"

    # Guardar blueprint
    input_json.write_bytes(blueprint_bytes)

    # 2) Reducir JSON con jq
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
            "Prolog falló: "
            + (e.stderr.decode(errors="ignore") if e.stderr else str(e))
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
