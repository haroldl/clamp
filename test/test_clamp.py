from pathlib import Path
import subprocess


TEST_DIR = Path(__file__).resolve().parent
ROOT = TEST_DIR.parent
CLAMP = ROOT / "clamp"
EXAMPLE_1 = TEST_DIR / "example_1.py"


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


def test_examples_match_expected_output():
    for sample in sorted(TEST_DIR.glob("example_*.py")):
        expected = sample.with_suffix(".expected")
        assert expected.exists(), f"missing expected output for {sample.name}"
        result = run_clamp(sample)
        assert result.stdout == expected.read_text()
