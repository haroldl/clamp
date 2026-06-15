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
EXAMPLE_78 = TEST_DIR / "example_78.py"
EXAMPLE_79 = TEST_DIR / "example_79.py"
EXAMPLE_80 = TEST_DIR / "example_80.py"
EXAMPLE_81 = TEST_DIR / "example_81.py"
EXAMPLE_82 = TEST_DIR / "example_82.py"
EXAMPLE_84 = TEST_DIR / "example_84.py"
EXAMPLE_85 = TEST_DIR / "example_85.py"
EXAMPLE_86 = TEST_DIR / "example_86.py"
EXAMPLE_88 = TEST_DIR / "example_88.py"
EXAMPLE_89 = TEST_DIR / "example_89.py"
EXAMPLE_90 = TEST_DIR / "example_90.py"
EXAMPLE_91 = TEST_DIR / "example_91.py"
EXAMPLE_92 = TEST_DIR / "example_92.py"
EXAMPLE_93 = TEST_DIR / "example_93.py"
EXAMPLE_94 = TEST_DIR / "example_94.py"
EXAMPLE_96 = TEST_DIR / "example_96.py"
EXAMPLE_97 = TEST_DIR / "example_97.py"
EXAMPLE_98 = TEST_DIR / "example_98.py"
EXAMPLE_99 = TEST_DIR / "example_99.py"
EXAMPLE_100 = TEST_DIR / "example_100.py"
EXAMPLE_102 = TEST_DIR / "example_102.py"
EXAMPLE_105 = TEST_DIR / "example_105.py"
EXAMPLE_107 = TEST_DIR / "example_107.py"
EXAMPLE_108 = TEST_DIR / "example_108.py"
EXAMPLE_109 = TEST_DIR / "example_109.py"
EXAMPLE_110 = TEST_DIR / "example_110.py"
EXAMPLE_113 = TEST_DIR / "example_113.py"
EXAMPLE_115 = TEST_DIR / "example_115.py"
EXAMPLE_116 = TEST_DIR / "example_116.py"
EXAMPLE_117 = TEST_DIR / "example_117.py"
EXAMPLE_118 = TEST_DIR / "example_118.py"
EXAMPLE_119 = TEST_DIR / "example_119.py"
EXAMPLE_120 = TEST_DIR / "example_120.py"
EXAMPLE_121 = TEST_DIR / "example_121.py"
EXAMPLE_122 = TEST_DIR / "example_122.py"
EXAMPLE_123 = TEST_DIR / "example_123.py"
EXAMPLE_124 = TEST_DIR / "example_124.py"
EXAMPLE_129 = TEST_DIR / "example_129.py"
EXAMPLE_131 = TEST_DIR / "example_131.py"
EXAMPLE_132 = TEST_DIR / "example_132.py"
EXAMPLE_134 = TEST_DIR / "example_134.py"
EXAMPLE_135 = TEST_DIR / "example_135.py"
EXAMPLE_137 = TEST_DIR / "example_137.py"
EXAMPLE_144 = TEST_DIR / "example_144.py"
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


@pytest.mark.parametrize("sample", [EXAMPLE_12, EXAMPLE_13, EXAMPLE_14, EXAMPLE_15, EXAMPLE_16, EXAMPLE_17, EXAMPLE_18, EXAMPLE_20, EXAMPLE_21, EXAMPLE_22, EXAMPLE_23, EXAMPLE_24, EXAMPLE_25, EXAMPLE_26, EXAMPLE_27, EXAMPLE_28, EXAMPLE_29, EXAMPLE_30, EXAMPLE_31, EXAMPLE_32, EXAMPLE_33, EXAMPLE_34, EXAMPLE_35, EXAMPLE_36, EXAMPLE_37, EXAMPLE_38, EXAMPLE_39, EXAMPLE_41, EXAMPLE_42, EXAMPLE_43, EXAMPLE_44, EXAMPLE_45, EXAMPLE_46, EXAMPLE_47, EXAMPLE_49, EXAMPLE_50, EXAMPLE_51, EXAMPLE_52, EXAMPLE_54, EXAMPLE_55, EXAMPLE_56, EXAMPLE_57, EXAMPLE_58, EXAMPLE_59, EXAMPLE_60, EXAMPLE_61, EXAMPLE_62, EXAMPLE_63, EXAMPLE_65, EXAMPLE_66, EXAMPLE_67, EXAMPLE_68, EXAMPLE_70, EXAMPLE_71, EXAMPLE_72, EXAMPLE_73, EXAMPLE_74, EXAMPLE_75, EXAMPLE_77, EXAMPLE_78, EXAMPLE_79, EXAMPLE_80, EXAMPLE_81, EXAMPLE_82, EXAMPLE_84, EXAMPLE_85, EXAMPLE_86, EXAMPLE_88, EXAMPLE_89, EXAMPLE_90, EXAMPLE_91, EXAMPLE_92, EXAMPLE_93, EXAMPLE_94, EXAMPLE_96, EXAMPLE_97, EXAMPLE_98, EXAMPLE_99, EXAMPLE_100, EXAMPLE_102, EXAMPLE_121], ids=lambda path: path.stem)
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


def test_str_replace_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_114.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_function_default_arguments_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_138.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_rsplit_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_140.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_range_reduce_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_141.py"
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



def test_math_module_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_200.py"
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


def test_math_module_full_api_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_201.py"
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


def test_math_module_ieee_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_202.py"
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


@pytest.mark.parametrize(
    ("source", "message"),
    [
        ("import math\nmath.sqrt(-1)\n", "expected a nonnegative input, got -1.0"),
        ("import math\nmath.log(0)\n", "expected a positive input"),
        ("import math\nmath.acos(2)\n", "expected a number in range from -1 up to 1, got 2.0"),
        ("import math\nmath.atanh(1)\n", "expected a number between -1 and 1, got 1.0"),
        ("import math\nmath.gamma(0)\n", "expected a noninteger or positive integer, got 0.0"),
        ("import math\nmath.pow(0, -1)\n", "math domain error"),
        ("import math\nmath.fmod(1, 0)\n", "math domain error"),
        ("import math\nmath.remainder(1, 0)\n", "math domain error"),
        ("import math\nmath.isqrt(-1)\n", "argument must be nonnegative"),
        ("import math\nmath.comb(-1, 2)\n", "n must be a non-negative integer"),
        ("import math\nmath.dist([1], [1, 2])\n", "same number of dimensions"),
        ("import math\nmath.sumprod([1], [1, 2])\n", "same length"),
    ],
)
def test_math_module_error_examples_match_local_cpython_when_available(tmp_path, source, message):
    sample = tmp_path / "math_error.py"
    sample.write_text(source)
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert message in cpython_result.stderr
    assert message in clamp_result.stderr

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


def test_str_index_raises_when_substring_is_missing():
    command = [str(CLAMP)]
    result = subprocess.run(
        command,
        cwd=ROOT,
        input='"abc".index("z")\n',
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "substring not found" in result.stderr


def test_str_rindex_raises_when_substring_is_missing():
    command = [str(CLAMP)]
    result = subprocess.run(
        command,
        cwd=ROOT,
        input='"abc".rindex("z")\n',
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert "substring not found" in result.stderr


def test_isinstance_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_101.py"
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


def test_next_default_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_104.py"
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


def test_str_removeprefix_removesuffix_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_105)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_105)
    assert clamp_result.stdout == cpython_result.stdout


def test_type_name_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_106.py"
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


def test_str_upper_lower_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_107)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_107)
    assert clamp_result.stdout == cpython_result.stdout

def test_str_swapcase_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_111.py"
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



def test_str_capitalize_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_113)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_113)
    assert clamp_result.stdout == cpython_result.stdout

def test_str_iter_dunder_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_115)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_115)
    assert clamp_result.stdout == cpython_result.stdout

def test_str_title_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_116)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_116)
    assert clamp_result.stdout == cpython_result.stdout

def test_str_join_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_108)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_108)
    assert clamp_result.stdout == cpython_result.stdout

def test_str_strip_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_109)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_109)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_contains_dunder_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_110)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_110)
    assert clamp_result.stdout == cpython_result.stdout



def test_type_slot_truth_and_len_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_112.py"
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

def test_str_startswith_endswith_tuple_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_117)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_117)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_isascii_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_118)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_118)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_isdecimal_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_119)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_119)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_isalpha_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_120)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_120)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_isdigit_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_122)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_122)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_isnumeric_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_142.py"
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


def test_str_casefold_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_143.py"
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


def test_str_splitlines_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_144)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_144)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_zfill_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_136.py"
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


def test_str_isalnum_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_123)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_123)
    assert clamp_result.stdout == cpython_result.stdout



def test_str_islower_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_125.py"
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



def test_str_isspace_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_126.py"
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


def test_str_isprintable_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_137)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_137)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_isupper_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_127.py"
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


def test_str_expandtabs_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_128.py"
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


def test_str_ljust_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_129)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_129)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_rjust_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_132)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_132)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_center_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_133.py"
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


def test_str_istitle_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_130.py"
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


def test_list_sizeof_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_131)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_131)
    assert clamp_result.stdout == cpython_result.stdout


def test_tuple_sizeof_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_134)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_134)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_partition_rpartition_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_135)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_135)
    assert clamp_result.stdout == cpython_result.stdout


def test_recursive_container_repr_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(EXAMPLE_124)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(EXAMPLE_124)
    assert clamp_result.stdout == cpython_result.stdout


def test_str_split_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_139.py"
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


def test_str_isidentifier_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_145.py"
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


def test_str_dunder_repr_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_146.py"
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


def test_str_capitalize_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_147.py"
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


def test_range_bool_dunder_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_148.py"
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


def test_str_add_mul_dunder_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_149.py"
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


def test_import_cached_metadata_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_156.py"
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


def test_import_spec_metadata_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_157.py"
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


def test_import_spec_uninitialized_submodules_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_158.py"
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


def test_import_spec_loader_state_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_159.py"
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


def test_import_spec_fileattr_cache_internals_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_160.py"
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


def test_import_source_file_loader_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_161.py"
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


def test_import_source_file_loader_get_filename_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_162.py"
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


def test_import_source_file_loader_is_package_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_163.py"
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


def test_import_module_spec_repr_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_164.py"
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


def test_import_package_path_aliases_spec_search_locations_like_local_cpython_when_available():
    sample = TEST_DIR / "example_165.py"
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


def test_import_source_file_loader_get_source_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_166.py"
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


def test_import_source_file_loader_default_name_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_167.py"
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


def test_import_source_file_loader_create_module_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_168.py"
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


@pytest.mark.parametrize(
    "sample_name",
    [
        "import_loader_name_mismatch.py",
        "import_loader_get_source_name_mismatch.py",
        "import_loader_is_package_name_mismatch.py",
    ],
)
def test_import_source_file_loader_name_mismatch_fails_like_local_cpython_when_available(sample_name):
    sample = TEST_DIR / sample_name
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert "loader for import_value cannot handle other" in cpython_result.stderr
    assert "loader for import_value cannot handle other" in clamp_result.stderr


def test_import_child_of_plain_module_fails_like_local_cpython_when_available():
    sample = TEST_DIR / "import_not_package_attempt.py"
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert "is not a package" in cpython_result.stderr
    assert "is not a package" in clamp_result.stderr
    assert "should not import" not in clamp_result.stdout


def test_import_type_module_metadata_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_169.py"
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


def test_import_source_file_loader_load_module_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_170.py"
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


def test_import_module_spec_cached_and_location_assignment_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_171.py"
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


def test_import_module_spec_parent_property_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_172.py"
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


def test_import_source_file_loader_mutated_name_mismatch_fails_like_local_cpython_when_available():
    sample = TEST_DIR / "import_loader_mutated_name_mismatch.py"
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert "loader for renamed_value cannot handle import_value" in cpython_result.stderr
    assert "loader for renamed_value cannot handle import_value" in clamp_result.stderr


def test_import_source_file_loader_mutated_attrs_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_173.py"
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


def test_import_module_spec_equality_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_174.py"
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


def test_import_direct_repr_methods_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_175.py"
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


def test_import_source_file_loader_hash_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_176.py"
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


def test_import_source_file_loader_extra_attrs_affect_equality_like_local_cpython_when_available():
    sample = TEST_DIR / "example_177.py"
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


def test_import_module_spec_hash_attribute_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_178.py"
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


def test_import_module_spec_hash_fails_like_local_cpython_when_available():
    sample = TEST_DIR / "import_module_spec_hash_attempt.py"
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert "unhashable type: 'ModuleSpec'" in cpython_result.stderr
    assert "unhashable type: 'ModuleSpec'" in clamp_result.stderr


def test_import_direct_eq_methods_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_179.py"
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


def test_import_direct_ne_methods_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_180.py"
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


def test_import_module_spec_cached_lazy_recompute_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_181.py"
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


def test_import_module_spec_direct_comparison_notimplemented_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_182.py"
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


def test_import_module_metadata_globals_match_local_cpython_when_available():
    sample = TEST_DIR / "example_183.py"
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


def test_import_source_file_loader_exec_module_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_184.py"
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


def test_import_source_file_loader_exec_module_name_mismatch_fails_like_local_cpython_when_available():
    sample = TEST_DIR / "import_loader_exec_module_name_mismatch.py"
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert (
        "loader for import_exec_module_target cannot handle renamed_exec_module_target"
        in cpython_result.stderr
    )
    assert (
        "loader for import_exec_module_target cannot handle renamed_exec_module_target"
        in clamp_result.stderr
    )
    assert "should not execute" not in clamp_result.stdout


def test_function_local_import_bindings_match_local_cpython_when_available():
    sample = TEST_DIR / "example_185.py"
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


def test_import_source_file_loader_get_data_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_186.py"
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


def test_import_builtin_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_187.py"
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


def test_import_source_file_loader_load_module_default_name_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_188.py"
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


def test_import_source_file_loader_set_data_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_189.py"
    output_path = Path("/tmp/clamp_import_loader_set_data.tmp")
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    output_path.unlink(missing_ok=True)
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    output_path.unlink(missing_ok=True)
    clamp_result = run_clamp(sample)
    output_path.unlink(missing_ok=True)
    assert clamp_result.stdout == cpython_result.stdout


def test_import_source_file_loader_path_stats_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_190.py"
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


def test_import_source_file_loader_cache_bytecode_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_191.py"
    output_path = Path("/tmp/clamp_import_loader_cache_bytecode.tmp")
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    output_path.unlink(missing_ok=True)
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    output_path.unlink(missing_ok=True)
    clamp_result = run_clamp(sample)
    output_path.unlink(missing_ok=True)
    assert clamp_result.stdout == cpython_result.stdout


def test_import_source_file_loader_path_stats_mtime_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_193.py"
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


def test_import_source_file_loader_get_source_normalizes_newlines_like_local_cpython_when_available():
    sample = TEST_DIR / "import_loader_get_source_newlines.py"
    source_path = Path("/tmp/clamp_import_loader_crlf_source.py")
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    source_path.write_bytes(b"left\r\nright\r\n")
    try:
        cpython_result = subprocess.run(
            [str(CPYTHON_314), str(sample)],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        )
        clamp_result = run_clamp(sample)
    finally:
        source_path.unlink(missing_ok=True)
    assert clamp_result.stdout == cpython_result.stdout


def test_import_source_file_loader_path_mtime_fails_like_local_cpython_when_available():
    sample = TEST_DIR / "import_loader_path_mtime_attempt.py"
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    clamp_result = subprocess.run(
        [str(CLAMP), str(sample)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert cpython_result.returncode != 0
    assert clamp_result.returncode != 0
    assert cpython_result.stdout == ""
    assert clamp_result.stdout == ""
    assert "OSError" in cpython_result.stderr
    assert "OSError" in clamp_result.stderr


def test_import_fromlist_star_uses_package_all_like_local_cpython_when_available():
    sample = TEST_DIR / "example_192.py"
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


def test_import_builtin_truthy_nonsequence_fromlist_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_194.py"
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


def test_from_import_star_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_195.py"
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


def test_import_source_file_loader_set_data_errors_match_local_cpython_when_available():
    sample = TEST_DIR / "example_196.py"
    output_path = Path("/tmp/clamp_import_loader_set_data_missing/child/data.pyc")
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    output_path.unlink(missing_ok=True)
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    output_path.unlink(missing_ok=True)
    clamp_result = run_clamp(sample)
    output_path.unlink(missing_ok=True)
    assert clamp_result.stdout == cpython_result.stdout

def test_import_source_file_loader_resource_reader_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_197.py"
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

def test_import_file_reader_resource_helpers_match_local_cpython_when_available():
    sample = TEST_DIR / "example_198.py"
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


def test_import_file_reader_open_resource_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_199.py"
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
