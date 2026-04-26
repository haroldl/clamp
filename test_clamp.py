from pathlib import Path
import subprocess


ROOT = Path(__file__).resolve().parent
CLAMP = ROOT / "clamp"
SAMPLE = ROOT / "test" / "test1.py"


def run_clamp(*args):
    return subprocess.run(
        [str(CLAMP), *args, str(SAMPLE)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )


def test_default_run_is_quiet():
    result = run_clamp()
    assert '"hello, clamp"' in result.stdout
    assert "Preparing to compile:" not in result.stdout
    assert "Generated Lisp code:" not in result.stdout


def test_verbose_run_shows_compiler_diagnostics():
    result = run_clamp("--verbose")
    assert '"hello, clamp" \n' in result.stdout
    assert "Preparing to compile:" in result.stdout
    assert "Generated Lisp code:" in result.stdout
