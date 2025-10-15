import argparse
from pathlib import Path
from pipeline import run_pipeline, PipelineError


parser = argparse.ArgumentParser(description="Pipeline DocGen (un solo comando)")
parser.add_argument("--input", required=True, help="Ruta al blueprint .json")
parser.add_argument("--out", help="Ruta destino del HTML (opcional)")
args = parser.parse_args()


project_root = Path(__file__).parent.resolve()
input_path = Path(args.input).resolve()


if not input_path.exists():
    raise SystemExit(f"No existe el input: {input_path}")


try:
    html_path = run_pipeline(input_path.read_bytes(), project_root)
    if args.out:
        dst = Path(args.out).resolve()
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_bytes(html_path.read_bytes())
        print(f"OK → {dst}")
    else:
        print(f"OK → {html_path}")
except PipelineError as e:
    raise SystemExit(f"ERROR: {e}")