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
EXAMPLE_22 = TEST_DIR / "example_22.py"
EXAMPLE_23 = TEST_DIR / "example_23.py"
EXAMPLE_24 = TEST_DIR / "example_24.py"
EXAMPLE_25 = TEST_DIR / "example_25.py"
EXAMPLE_26 = TEST_DIR / "example_26.py"
EXAMPLE_27 = TEST_DIR / "example_27.py"
EXAMPLE_28 = TEST_DIR / "example_28.py"
EXAMPLE_29 = TEST_DIR / "example_29.py"
EXAMPLE_30 = TEST_DIR / "example_30.py"
EXAMPLE_31 = TEST_DIR / "example_31.py"
EXAMPLE_32 = TEST_DIR / "example_32.py"
EXAMPLE_33 = TEST_DIR / "example_33.py"
EXAMPLE_34 = TEST_DIR / "example_34.py"
EXAMPLE_35 = TEST_DIR / "example_35.py"
EXAMPLE_36 = TEST_DIR / "example_36.py"
EXAMPLE_37 = TEST_DIR / "example_37.py"
EXAMPLE_38 = TEST_DIR / "example_38.py"
EXAMPLE_39 = TEST_DIR / "example_39.py"
EXAMPLE_41 = TEST_DIR / "example_41.py"
EXAMPLE_42 = TEST_DIR / "example_42.py"
EXAMPLE_43 = TEST_DIR / "example_43.py"
EXAMPLE_44 = TEST_DIR / "example_44.py"
EXAMPLE_45 = TEST_DIR / "example_45.py"
EXAMPLE_46 = TEST_DIR / "example_46.py"
EXAMPLE_47 = TEST_DIR / "example_47.py"
EXAMPLE_49 = TEST_DIR / "example_49.py"
EXAMPLE_50 = TEST_DIR / "example_50.py"
EXAMPLE_51 = TEST_DIR / "example_51.py"
EXAMPLE_52 = TEST_DIR / "example_52.py"
EXAMPLE_54 = TEST_DIR / "example_54.py"
EXAMPLE_55 = TEST_DIR / "example_55.py"
EXAMPLE_56 = TEST_DIR / "example_56.py"
EXAMPLE_57 = TEST_DIR / "example_57.py"
EXAMPLE_58 = TEST_DIR / "example_58.py"
EXAMPLE_59 = TEST_DIR / "example_59.py"
EXAMPLE_60 = TEST_DIR / "example_60.py"
EXAMPLE_61 = TEST_DIR / "example_61.py"
EXAMPLE_62 = TEST_DIR / "example_62.py"
EXAMPLE_63 = TEST_DIR / "example_63.py"
EXAMPLE_65 = TEST_DIR / "example_65.py"
EXAMPLE_66 = TEST_DIR / "example_66.py"
EXAMPLE_67 = TEST_DIR / "example_67.py"
EXAMPLE_68 = TEST_DIR / "example_68.py"
EXAMPLE_70 = TEST_DIR / "example_70.py"
EXAMPLE_71 = TEST_DIR / "example_71.py"
EXAMPLE_72 = TEST_DIR / "example_72.py"
EXAMPLE_73 = TEST_DIR / "example_73.py"
EXAMPLE_74 = TEST_DIR / "example_74.py"
EXAMPLE_75 = TEST_DIR / "example_75.py"
EXAMPLE_77 = TEST_DIR / "example_77.py"
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


@pytest.mark.parametrize("sample", [EXAMPLE_12, EXAMPLE_13, EXAMPLE_14, EXAMPLE_15, EXAMPLE_16, EXAMPLE_17, EXAMPLE_18, EXAMPLE_20, EXAMPLE_21, EXAMPLE_22, EXAMPLE_23, EXAMPLE_24, EXAMPLE_25, EXAMPLE_26, EXAMPLE_27, EXAMPLE_28, EXAMPLE_29, EXAMPLE_30, EXAMPLE_31, EXAMPLE_32, EXAMPLE_33, EXAMPLE_34, EXAMPLE_35, EXAMPLE_36, EXAMPLE_37, EXAMPLE_38, EXAMPLE_39, EXAMPLE_41, EXAMPLE_42, EXAMPLE_43, EXAMPLE_44, EXAMPLE_45, EXAMPLE_46, EXAMPLE_47, EXAMPLE_49, EXAMPLE_50, EXAMPLE_51, EXAMPLE_52, EXAMPLE_54, EXAMPLE_55, EXAMPLE_56, EXAMPLE_57, EXAMPLE_58, EXAMPLE_59, EXAMPLE_60, EXAMPLE_61, EXAMPLE_62, EXAMPLE_63, EXAMPLE_65, EXAMPLE_66, EXAMPLE_67, EXAMPLE_68, EXAMPLE_70, EXAMPLE_71, EXAMPLE_72, EXAMPLE_73, EXAMPLE_74, EXAMPLE_75, EXAMPLE_77], ids=lambda path: path.stem)
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

def test_next_raises_stop_iteration_for_empty_tuple_iterator():
    command = [str(CLAMP)]
    result = subprocess.run(
        command,
        cwd=ROOT,
        input="it = iter(())\nnext(it)\n",
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "StopIteration" in result.stderr

def test_zip_raises_stop_iteration_at_shortest_iterable():
    command = [str(CLAMP)]
    result = subprocess.run(
        command,
        cwd=ROOT,
        input='it = zip([1], "ab")\nnext(it)\nnext(it)\n',
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "StopIteration" in result.stderr

