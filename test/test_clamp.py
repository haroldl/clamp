from pathlib import Path
import subprocess

import pytest


TEST_DIR = Path(__file__).resolve().parent
ROOT = TEST_DIR.parent
CLAMP = ROOT / "clamp"
EXAMPLE_1 = TEST_DIR / "example_1.py"
EXAMPLE_12 = TEST_DIR / "example_12.py"
EXAMPLE_13 = TEST_DIR / "example_13.py"
EXAMPLE_14 = TEST_DIR / "example_14.py"
EXAMPLE_15 = TEST_DIR / "example_15.py"
EXAMPLE_16 = TEST_DIR / "example_16.py"
EXAMPLE_17 = TEST_DIR / "example_17.py"
EXAMPLE_18 = TEST_DIR / "example_18.py"
EXAMPLE_20 = TEST_DIR / "example_20.py"
EXAMPLE_21 = TEST_DIR / "example_21.py"
CPYTHON_314 = Path.home() / "local" / "Python-3.14.5" / "python"


def run_clamp(sample, *args):
    command = [str(CLAMP), *args, str(sample)]
    try:
        return subprocess.run(
            command,
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
    except subprocess.CalledProcessError as exc:
        compile_only_command = [str(CLAMP), "-c", str(sample)]
        compile_only_result = subprocess.run(
            compile_only_command,
            cwd=ROOT,
            capture_output=True,
            text=True,
        )
        raise AssertionError(
            "clamp command failed\n"
            f"command: {exc.cmd}\n"
            f"cwd: {ROOT}\n"
            f"sample: {sample}\n"
            f"exit code: {exc.returncode}\n"
            f"stdout:\n{exc.stdout}\n"
            f"stderr:\n{exc.stderr}\n"
            "compile-only follow-up:\n"
            f"command: {compile_only_command}\n"
            f"exit code: {compile_only_result.returncode}\n"
            f"stdout:\n{compile_only_result.stdout}\n"
            f"stderr:\n{compile_only_result.stderr}"
        ) from exc


def run_clamp_repl(input_text):
    command = [str(CLAMP)]
    return subprocess.run(
        command,
        cwd=ROOT,
        input=input_text,
        check=True,
        capture_output=True,
        text=True,
    )


def test_default_run_is_quiet():
    result = run_clamp(EXAMPLE_1)
    assert result.stdout == "hello, clamp\n"
    assert "Preparing to compile:" not in result.stdout
    assert "Generated Lisp code:" not in result.stdout


def test_verbose_run_shows_compiler_diagnostics():
    result = run_clamp(EXAMPLE_1, "--verbose")
    assert "hello, clamp\n\n" in result.stdout
    assert "Preparing to compile:" in result.stdout
    assert "Generated Lisp code:" in result.stdout


def test_compile_only_prints_generated_lisp_without_running_program():
    result = run_clamp(EXAMPLE_1, "--compile-only")
    assert '(common-lisp:funcall |CLAMP.__builtins__|:PRINT "hello, clamp")' in result.stdout
    assert "hello, clamp\n\n" not in result.stdout


EXAMPLES = sorted(TEST_DIR.glob("example_*.py"))


@pytest.mark.parametrize("sample", EXAMPLES, ids=lambda path: path.stem)
def test_example_matches_expected_output(sample):
    expected = sample.with_suffix(".expected")
    assert expected.exists(), f"missing expected output for {sample.name}"
    result = run_clamp(sample)
    assert result.stdout == expected.read_text()


@pytest.mark.parametrize("sample", [EXAMPLE_12, EXAMPLE_13, EXAMPLE_14, EXAMPLE_15, EXAMPLE_16, EXAMPLE_17, EXAMPLE_18, EXAMPLE_20, EXAMPLE_21], ids=lambda path: path.stem)
def test_examples_match_local_cpython_when_available(sample):
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_interactive_math_expression_prints_result():
    result = run_clamp_repl("1 + 2\nquit\n")
    assert result.stdout == ">>> 3\n>>> "


def test_next_raises_stop_iteration_after_exhaustion():
    command = [str(CLAMP)]
    result = subprocess.run(
        command,
        cwd=ROOT,
        input="it = iter([])\nnext(it)\n",
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "StopIteration" in result.stderr
