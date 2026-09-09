#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Марк-веб: мини-чат в браузере. Чистая стандартная библиотека, без пип-зависимостей.
Запуск:  cd ~/selflearner/web && python3 server.py
Затем открой http://localhost:8000

Как работает:
  - держит Марка (sbcl --script api-brain.lisp) живым процессом, чтобы он помнил диалог
  - GET  /      -> отдаёт index.html
  - POST /chat  -> {"msg": "..."}  -> ответ {"reply": "..."}
"""
import json
import os
import subprocess
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)                    # ~/selflearner
MARK_SCRIPT = os.path.join(ROOT, "api-brain.lisp")
PORT = 8000


def start_mark():
    """Поднимаем Марка как живой процесс. Первая строка из stdout = READY."""
    p = subprocess.Popen(
        ["sbcl", "--script", MARK_SCRIPT],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        stderr=subprocess.PIPE, text=True, bufsize=1,
    )
    ready = p.stdout.readline().strip()
    print("[mark] ready:", ready)
    return p


MARK = start_mark()


def ask_mark(msg: str) -> str:
    MARK.stdin.write(msg + "\n")
    MARK.stdin.flush()
    line = MARK.stdout.readline()
    if not line:
        raise RuntimeError("Марк умер (перезапусти сервер)")
    # обратно разворачиваем \n и \t (мы их закодировали одной строкой)
    return line.strip().replace("\\n", "\n").replace("\\t", "\t")


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *a):   # чтобы не спамить консоль
        pass

    def do_GET(self):
        if self.path == "/":
            with open(os.path.join(HERE, "index.html"), encoding="utf-8") as f:
                body = f.read()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write(body.encode("utf-8"))
            return
        self.send_response(404)
        self.end_headers()

    def do_POST(self):
        if self.path != "/chat":
            self.send_response(404)
            self.end_headers()
            return
        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length).decode("utf-8")
        try:
            data = json.loads(raw or "{}")
        except json.JSONDecodeError:
            data = {}
        msg = data.get("msg", "")
        try:
            reply = ask_mark(msg)
        except Exception as e:
            reply = f"(ошибка: {e})"
        out = json.dumps({"reply": reply}, ensure_ascii=False).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(out)


if __name__ == "__main__":
    print("Марк-веб на http://localhost:%d  (Ctrl+C чтобы выйти)" % PORT)
    try:
        ThreadingHTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
    except KeyboardInterrupt:
        print("\nпока, Марк!")
        MARK.terminate()
