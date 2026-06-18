from pathlib import Path
import socket
import subprocess
import sys
import time

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
    assert '(|CLAMP.__CLAMP_INTERNALS__|:PY-INVOKE-CALLABLE |CLAMP.__builtins__|:PRINT "hello, clamp")' in result.stdout
    assert "hello, clamp\n\n" not in result.stdout


EXAMPLES = sorted(TEST_DIR.glob("example_*.py"))


@pytest.mark.parametrize("sample", EXAMPLES, ids=lambda path: path.stem)
def test_example_matches_expected_output(sample):
    expected = sample.with_suffix(".expected")
    assert expected.exists(), f"missing expected output for {sample.name}"
    result = run_clamp(sample)
    assert result.stdout == expected.read_text()


def test_aiohttp_plain_http_transport(tmp_path):
    root = tmp_path / "http-root"
    root.mkdir()
    (root / "hello.json").write_text('{"source":"http","count":3}\n')
    (root / "plain.txt").write_text("hello over http\n")

    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server = subprocess.Popen(
        [
            sys.executable,
            "-m",
            "http.server",
            str(port),
            "--bind",
            "127.0.0.1",
            "--directory",
            str(root),
        ],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"http.server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("http.server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_http.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            "    async with aiohttp.ClientSession() as session:\n"
            f"        async with session.get('http://127.0.0.1:{port}/hello.json') as resp:\n"
            "            print(resp.status, resp.ok)\n"
            "            print(resp.content_type)\n"
            "            data = await resp.json()\n"
            "            print(data['source'], data['count'])\n"
            f"        async with session.get('http://127.0.0.1:{port}/plain.txt') as resp:\n"
            "            print(await resp.text())\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == "200 True\napplication/json\nhttp 3\nhello over http\n\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()



def test_aiohttp_plain_http_json_and_params(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "json_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "import json\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def do_PATCH(self):\n"
        "        size = int(self.headers.get('Content-Length', '0'))\n"
        "        body = self.rfile.read(size).decode('utf-8')\n"
        "        response = json.dumps({\n"
        "            'method': self.command,\n"
        "            'path': self.path,\n"
        "            'content_type': self.headers.get('Content-Type'),\n"
        "            'body': body,\n"
        "        })\n"
        "        encoded = response.encode('utf-8')\n"
        "        self.send_response(202)\n"
        "        self.send_header('Content-Type', 'application/json')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"json server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("json server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_json_params.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            f"    async with aiohttp.patch('http://127.0.0.1:{port}/submit', params={{'q': 'hello world', 'n': 3}}, json={{'name': 'clamp', 'ok': True}}) as resp:\n"
            "        print(resp.status)\n"
            "        data = await resp.json()\n"
            "        print(data['method'], data['path'])\n"
            "        print(data['content_type'])\n"
            "        print(data['body'])\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == '202\nPATCH /submit?q=hello+world&n=3\napplication/json\n{"name":"clamp","ok":true}\n'
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_aiohttp_plain_http_headers_and_post(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "echo_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "import json\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def do_POST(self):\n"
        "        size = int(self.headers.get('Content-Length', '0'))\n"
        "        body = self.rfile.read(size).decode('utf-8')\n"
        "        response = json.dumps({\n"
        "            'method': self.command,\n"
        "            'token': self.headers.get('X-Token'),\n"
        "            'content_type': self.headers.get('Content-Type'),\n"
        "            'body': body,\n"
        "        })\n"
        "        encoded = response.encode('utf-8')\n"
        "        self.send_response(201)\n"
        "        self.send_header('Content-Type', 'application/json')\n"
        "        self.send_header('X-Server', 'clamp-test')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"echo server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("echo server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_post.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            "    async with aiohttp.ClientSession() as session:\n"
            f"        async with session.post('http://127.0.0.1:{port}/submit', data='payload', headers={{'X-Token': 'abc', 'Content-Type': 'text/plain'}}) as resp:\n"
            "            print(resp.status, resp.headers['x-server'])\n"
            "            data = await resp.json()\n"
            "            print(data['method'], data['token'], data['content_type'], data['body'])\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == "201 clamp-test\nPOST abc text/plain payload\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_aiohttp_basic_auth_and_form_data(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "form_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "import json\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def do_POST(self):\n"
        "        size = int(self.headers.get('Content-Length', '0'))\n"
        "        body = self.rfile.read(size).decode('utf-8')\n"
        "        response = json.dumps({\n"
        "            'auth': self.headers.get('Authorization'),\n"
        "            'content_type': self.headers.get('Content-Type'),\n"
        "            'body': body,\n"
        "        })\n"
        "        encoded = response.encode('utf-8')\n"
        "        self.send_response(200)\n"
        "        self.send_header('Content-Type', 'application/json')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"form server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("form server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_auth_form.py"
        sample.write_text(
            "import asyncio\n"
            "from aiohttp.client import BasicAuth, ClientSession, FormData\n"
            "\n"
            "async def main():\n"
            "    form = FormData({'name': 'clamp'})\n"
            "    form.add_field('space', 'hello world')\n"
            "    async with ClientSession() as session:\n"
            f"        async with session.post('http://127.0.0.1:{port}/submit', data=form, auth=BasicAuth('user', 'pass')) as resp:\n"
            "            data = await resp.json()\n"
            "            print(data['auth'])\n"
            "            print(data['content_type'])\n"
            "            print(data['body'])\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == "Basic dXNlcjpwYXNz\napplication/x-www-form-urlencoded\nname=clamp&space=hello+world\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_aiohttp_session_defaults_base_url_headers_and_auth(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "session_defaults_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "import json\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def do_GET(self):\n"
        "        response = json.dumps({\n"
        "            'path': self.path,\n"
        "            'auth': self.headers.get('Authorization'),\n"
        "            'token': self.headers.get('X-Token'),\n"
        "            'override': self.headers.get('X-Override'),\n"
        "        })\n"
        "        encoded = response.encode('utf-8')\n"
        "        self.send_response(200)\n"
        "        self.send_header('Content-Type', 'application/json')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"session defaults server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("session defaults server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_session_defaults.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            f"    async with aiohttp.ClientSession(base_url='http://127.0.0.1:{port}/api', headers={{'X-Token': 'session', 'X-Override': 'session'}}, auth=aiohttp.BasicAuth('user', 'pass')) as session:\n"
            "        async with session.get('/items', params={'q': 'hello world'}, headers={'X-Override': 'request'}) as resp:\n"
            "            data = await resp.json()\n"
            "            print(data['path'])\n"
            "            print(data['auth'])\n"
            "            print(data['token'])\n"
            "            print(data['override'])\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == "/api/items?q=hello+world\nBasic dXNlcjpwYXNz\nsession\nrequest\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_aiohttp_cookie_jar_and_session_cookies(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "cookie_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "import json\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def do_GET(self):\n"
        "        response = json.dumps({'path': self.path, 'cookie': self.headers.get('Cookie')})\n"
        "        encoded = response.encode('utf-8')\n"
        "        self.send_response(200)\n"
        "        self.send_header('Content-Type', 'application/json')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        if self.path.startswith('/login'):\n"
        "            self.send_header('Set-Cookie', 'session=xyz; Path=/')\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"cookie server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("cookie server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_cookies.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            "    jar = aiohttp.CookieJar()\n"
            "    jar.update_cookies({'jar': 'one'})\n"
            f"    async with aiohttp.ClientSession(base_url='http://127.0.0.1:{port}', cookie_jar=jar, cookies={{'initial': 'yes'}}) as session:\n"
            "        async with session.get('/login', cookies={'request': 'only'}) as resp:\n"
            "            data = await resp.json()\n"
            "            print(data['cookie'])\n"
            "        async with session.get('/next') as resp:\n"
            "            data = await resp.json()\n"
            "            print(data['cookie'])\n"
            "        cookies = session.cookie_jar.filter_cookies('/')\n"
            "        print(cookies['session'], cookies['initial'], cookies['jar'])\n"
            "        print(session.cookie_jar.clear())\n"
            "        async with session.get('/empty') as resp:\n"
            "            data = await resp.json()\n"
            "            print(data['cookie'])\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == "jar=one; initial=yes; request=only\njar=one; initial=yes; session=xyz\nxyz yes one\nNone\nNone\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_aiohttp_response_metadata_and_cookies(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "metadata_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def do_GET(self):\n"
        "        encoded = b'metadata'\n"
        "        self.send_response(200)\n"
        "        self.send_header('Content-Type', 'text/plain; charset=iso-8859-1')\n"
        "        self.send_header('Set-Cookie', 'token=abc; Path=/')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"metadata server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("metadata server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_response_metadata.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            "    async with aiohttp.ClientSession() as session:\n"
            f"        async with session.get('http://127.0.0.1:{port}/info', headers={{'X-Test': 'yes'}}) as resp:\n"
            "            print(resp.real_url)\n"
            "            print(resp.request_info['method'], resp.request_info['url'])\n"
            "            print(resp.request_info['headers']['X-Test'])\n"
            "            print(len(resp.history), resp.cookies['token'])\n"
            "            print(resp.content_length, resp.charset, resp.get_encoding())\n"
            "            print(await resp.text())\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == f"http://127.0.0.1:{port}/info\nGET http://127.0.0.1:{port}/info\nyes\n0 abc\n8 iso-8859-1 iso-8859-1\nmetadata\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_aiohttp_redirects_and_history(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "redirect_server.py"
    server_script.write_text(
        "from http.server import BaseHTTPRequestHandler, HTTPServer\n"
        "class Handler(BaseHTTPRequestHandler):\n"
        "    def log_message(self, format, *args):\n"
        "        pass\n"
        "    def redirect(self, location):\n"
        "        self.send_response(302)\n"
        "        self.send_header('Location', location)\n"
        "        self.send_header('Content-Length', '0')\n"
        "        self.end_headers()\n"
        "    def do_GET(self):\n"
        "        if self.path == '/redirect':\n"
        "            self.redirect('/final')\n"
        "            return\n"
        "        if self.path == '/loop':\n"
        "            self.redirect('/loop')\n"
        "            return\n"
        "        encoded = ('final:' + self.path).encode('utf-8')\n"
        "        self.send_response(200)\n"
        "        self.send_header('Content-Type', 'text/plain')\n"
        "        self.send_header('Content-Length', str(len(encoded)))\n"
        "        self.end_headers()\n"
        "        self.wfile.write(encoded)\n"
        "HTTPServer(('127.0.0.1', int(__import__('sys').argv[1])), Handler).serve_forever()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"redirect server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                    break
            except OSError:
                if time.time() >= deadline:
                    raise AssertionError("redirect server did not start")
                time.sleep(0.05)

        sample = tmp_path / "aiohttp_redirects.py"
        sample.write_text(
            "import asyncio\n"
            "import aiohttp\n"
            "\n"
            "async def main():\n"
            f"    async with aiohttp.ClientSession(base_url='http://127.0.0.1:{port}') as session:\n"
            "        async with session.get('/redirect') as resp:\n"
            "            print(resp.status, resp.real_url, await resp.text())\n"
            "            print(len(resp.history), resp.history[0].status, resp.history[0].headers['location'])\n"
            "        async with session.get('/redirect', allow_redirects=False) as resp:\n"
            "            print(resp.status, len(resp.history), resp.headers['location'])\n"
            "        try:\n"
            "            async with session.get('/loop', max_redirects=1) as resp:\n"
            "                print('not reached', resp.status)\n"
            "        except aiohttp.TooManyRedirects as err:\n"
            "            print('too-many', err.status, err.message)\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == (
            f"200 http://127.0.0.1:{port}/final final:/final\n"
            "1 302 /final\n"
            "302 0 /final\n"
            "too-many 302 Too many redirects\n"
        )
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_asyncio_open_connection_streams(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    server_script = tmp_path / "stream_server.py"
    ready_file = tmp_path / "stream-ready"
    server_script.write_text(
        "import socket\n"
        "import sys\n"
        "server = socket.socket()\n"
        "server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)\n"
        "server.bind(('127.0.0.1', int(sys.argv[1])))\n"
        "server.listen(1)\n"
        "open(sys.argv[2], 'w').close()\n"
        "conn, addr = server.accept()\n"
        "with conn:\n"
        "    data = b''\n"
        "    while not data.endswith(b'\\n'):\n"
        "        chunk = conn.recv(1024)\n"
        "        if not chunk:\n"
        "            break\n"
        "        data += chunk\n"
        "    conn.sendall(b'chunk|tail')\n"
        "server.close()\n"
    )
    server = subprocess.Popen(
        [sys.executable, str(server_script), str(port), str(ready_file)],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        deadline = time.time() + 5
        while True:
            if server.poll() is not None:
                stdout, stderr = server.communicate()
                raise AssertionError(f"stream server exited early\nstdout:\n{stdout}\nstderr:\n{stderr}")
            if ready_file.exists():
                break
            if time.time() >= deadline:
                raise AssertionError("stream server did not start")
            time.sleep(0.05)

        sample = tmp_path / "asyncio_streams.py"
        sample.write_text(
            "import asyncio\n"
            "from asyncio.streams import open_connection\n"
            "\n"
            "async def main():\n"
            f"    reader, writer = await open_connection('127.0.0.1', {port})\n"
            f"    print(writer.get_extra_info('peername')[1] == {port})\n"
            "    print(writer.is_closing())\n"
            "    print(writer.can_write_eof())\n"
            "    writer.writelines([b'he', b'llo', b'\\n'])\n"
            "    writer.write_eof()\n"
            "    await writer.drain()\n"
            "    print(await reader.readuntil(b'|'))\n"
            "    try:\n"
            "        print(await reader.readexactly(10))\n"
            "    except asyncio.IncompleteReadError as err:\n"
            "        print(err.partial, err.expected)\n"
            "    print(reader.at_eof())\n"
            "    writer.close()\n"
            "    print(writer.is_closing())\n"
            "    await writer.wait_closed()\n"
            "    print(writer.is_closing())\n"
            "\n"
            "asyncio.run(main())\n"
        )
        result = run_clamp(sample)
        assert result.stdout == "True\nFalse\nTrue\nb'chunk|'\nb'tail' 10\nTrue\nTrue\nTrue\n"
    finally:
        server.terminate()
        try:
            server.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            server.kill()
            server.communicate()


def test_asyncio_start_server_streams(tmp_path):
    with socket.socket() as probe:
        probe.bind(("127.0.0.1", 0))
        port = probe.getsockname()[1]

    sample = tmp_path / "asyncio_start_server.py"
    sample.write_text(
        "import asyncio\n"
        "\n"
        "async def handle(reader, writer):\n"
        "    data = await reader.readline()\n"
        "    writer.writelines([b'srv:', data])\n"
        "    await writer.drain()\n"
        "    writer.close()\n"
        "    await writer.wait_closed()\n"
        "\n"
        "async def main():\n"
        f"    server = await asyncio.start_server(handle, '127.0.0.1', {port})\n"
        "    print(server.is_serving())\n"
        f"    reader, writer = await asyncio.open_connection('127.0.0.1', {port})\n"
        "    writer.write(b'ping\\n')\n"
        "    await writer.drain()\n"
        "    print(await reader.readline())\n"
        "    writer.close()\n"
        "    await writer.wait_closed()\n"
        "    server.close()\n"
        "    print(server.is_serving())\n"
        "    await server.wait_closed()\n"
        "\n"
        "asyncio.run(main())\n"
    )
    result = run_clamp(sample)
    assert result.stdout == "True\nb'srv:ping\\n'\nFalse\n"




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


def test_dict_literal_item_access_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_203.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_dict_get_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_207.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_dict_pop_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_221.py"
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


def test_import_file_reader_open_resource_seek_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_204.py"
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


def test_import_file_reader_open_resource_name_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_206.py"
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


def test_import_file_reader_open_resource_read1_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_208.py"
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


def test_import_file_reader_open_resource_readline_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_209.py"
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


def test_import_module_dict_namespace_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_210.py"
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


def test_import_module_spec_cached_suffixes_match_local_cpython_when_available():
    sample = TEST_DIR / "example_211.py"
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


def test_import_file_reader_open_resource_context_methods_match_local_cpython_when_available():
    sample = TEST_DIR / "example_212.py"
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


def test_import_source_file_loader_init_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_213.py"
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


def test_import_source_file_loader_dict_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_214.py"
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


def test_import_file_reader_init_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_217.py"
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


def test_import_file_reader_open_resource_peek_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_218.py"
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


def test_import_file_reader_contents_iterator_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_219.py"
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


def test_import_file_reader_open_resource_readlines_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_220.py"
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


def test_import_file_reader_open_resource_isatty_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_222.py"
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


def test_import_file_reader_open_resource_flush_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_223.py"
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


def test_import_file_reader_open_resource_iteration_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_225.py"
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


def test_import_file_reader_dict_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_226.py"
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



def test_import_file_reader_files_path_object_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_228.py"
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

def test_import_path_read_bytes_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_229.py"
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


def test_import_path_read_text_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_230.py"
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


def test_import_path_joinpath_segments_match_local_cpython_when_available():
    sample = TEST_DIR / "example_231.py"
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


def test_import_path_truediv_operator_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_234.py"
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


def test_import_path_parent_property_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_236.py"
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


def test_import_path_suffix_property_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_237.py"
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


def test_import_path_stem_property_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_240.py"
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


def test_import_path_suffixes_property_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_241.py"
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


def test_import_path_as_posix_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_242.py"
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


def test_import_path_direct_repr_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_244.py"
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


def test_ascii_builtin_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_227.py"
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


def test_dict_copy_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_216.py"
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


def test_dict_delitem_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_215.py"
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


def test_dict_clear_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_224.py"
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


def test_dict_setdefault_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_238.py"
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


def test_dict_update_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_239.py"
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


def test_dict_iteration_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_243.py"
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

def test_id_builtin_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_205.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_empty_tuple_identity_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_232.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_bytes_iterator_example_matches_local_cpython_when_available():
    if not CPYTHON_314.exists():
        pytest.skip("local CPython 3.14.5 interpreter is not built")
    sample = TEST_DIR / "example_233.py"
    cpython_result = subprocess.run(
        [str(CPYTHON_314), str(sample)],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    clamp_result = run_clamp(sample)
    assert clamp_result.stdout == cpython_result.stdout


def test_import_module_spec_private_set_fileattr_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_235.py"
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


def test_import_module_spec_dict_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_245.py"
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


def test_dict_equality_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_246.py"
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


def test_import_path_direct_str_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_247.py"
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


def test_import_source_file_loader_pathlike_input_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_248.py"
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


def test_import_path_fspath_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_249.py"
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


def test_import_path_with_name_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_250.py"
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


def test_import_path_with_suffix_method_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_252.py"
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


def test_operator_length_hint_example_matches_local_cpython_when_available():
    sample = TEST_DIR / "example_251.py"
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
