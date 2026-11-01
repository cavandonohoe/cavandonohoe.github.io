"""Reproduction commands: build offline from annotations; detect only private inputs."""

import argparse
import hashlib
import importlib.metadata
import json
from pathlib import Path

from .analysis import analyze
from .classification import candidates
from .ingestion import read_local_units
from .visualization import render_dashboard


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("command", choices=["build", "detect"])
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--input", type=Path)
    parser.add_argument("--offline", action="store_true", default=True)
    parser.add_argument(
        "--online",
        action="store_false",
        dest="offline",
        help="Use a pinned CDN script instead of embedding Plotly",
    )
    parser.add_argument("--export", action="store_true")
    args = parser.parse_args()
    root = args.root.resolve()
    config = json.loads((root / "configs/pipeline.json").read_text())
    if args.command == "detect":
        if not args.input:
            parser.error("detect needs a lawful local --input JSONL")
        output = root / "data/private/candidates.json"
        output.parent.mkdir(parents=True, exist_ok=True)
        result = candidates(read_local_units(args.input), config)
        output.write_text(json.dumps(result, indent=2) + "\n")
        print(f"{len(result)} candidate scenes; private output: {output}")
        return
    reports = root / "reports"
    reports.mkdir(exist_ok=True)
    (reports / "figures").mkdir(exist_ok=True)
    (reports / "results.json").write_text(
        json.dumps(analyze(root), indent=2, allow_nan=False) + "\n"
    )
    inputs = sorted(
        [
            *(root / "configs").glob("*.json"),
            *(root / "data").glob("*.json"),
            *(root / "data").glob("*.jsonl"),
            *(root / "data").glob("*.csv"),
            *(root / "src/smut_index").glob("*.py"),
            root / "pyproject.toml",
            root / "requirements.lock",
        ]
    )
    manifest = {
        "inputs": {
            str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest() for p in inputs
        },
        "versions": {
            name: importlib.metadata.version(name) for name in ["pandas", "pydantic", "plotly"]
        },
        "model": None,
        "rules": config["version"],
        "seed": config["seed"],
    }
    (reports / "reproduction_manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    print(render_dashboard(root, args.offline, args.export))


if __name__ == "__main__":
    main()
