#!/usr/bin/env python3
"""convert.py — конвертер логов диалогов в датасеты и память Марка.

Понимает два формата входа:
  1) Сырой экспорт Telegram (result.json) — {"messages": [{"type","from","text"}, ...]}
  2) Старый формат — список сессий [{"messages": [{"role","content"}, ...]}]

Пары «вопрос → ответ» строятся по смене автора: сообщение и следующее за ним
сообщение ДРУГОГО автора (неперекрывающиеся пары). Для одиночных диалогов
берутся пары user -> assistant.

Выход (с префиксом --out, по умолчанию "dataset"):
  <out>_sharegpt.json  — ShareGPT
  <out>_llama.txt      — ChatML (LLaMA)
  <out>_lisp.lisp      — память Марка: (("вопрос" "ответ" nil 1.0) ...)

Запуск:
  python3 convert.py                          # читает tg_logs.json рядом
  python3 convert.py result.json              # любой вход
  python3 convert.py result.json --out dump   # префикс выходных файлов
  python3 convert.py result.json --names      # "Автор: текст" (полезно для групп)
"""
import argparse
import json
import os
import re
import sys


def _clean(s):
    return re.sub(r"\s+", " ", (s or "")).strip()


def extract_text(t):
    """Telegram: text бывает строкой или списком сущностей [{'type','text'}]"""
    if isinstance(t, str):
        return t
    if isinstance(t, list):
        return "".join(e.get("text", "") for e in t if isinstance(e, dict))
    return ""


def _pure_bot_command(m):
    t = m.get("text")
    return bool(isinstance(t, list) and t
                and all(isinstance(e, dict) and e.get("type") == "bot_command" for e in t))


def from_telegram(d):
    """экспорт Telegram -> [(автор_вопроса, вопрос, автор_ответа, ответ), ...]"""
    msgs = [m for m in d.get("messages", []) if m.get("type") == "message"]
    msgs.sort(key=lambda m: m.get("id", 0))
    rows = []
    for m in msgs:
        if _pure_bot_command(m):
            continue
        txt = _clean(extract_text(m.get("text")))
        if txt:
            rows.append((m.get("from") or "?", txt))
    pairs, i = [], 0
    while i < len(rows) - 1:
        a_name, a_txt = rows[i]
        b_name, b_txt = rows[i + 1]
        if a_name != b_name:
            pairs.append((a_name, a_txt, b_name, b_txt))
            i += 2
        else:
            i += 1
    return pairs


def from_sessions(d):
    """список сессий -> [(None, вопрос, None, ответ), ...]"""
    pairs = []
    for s in d:
        msgs = s.get("messages", []) if isinstance(s, dict) else []
        for i in range(len(msgs) - 1):
            if msgs[i].get("role") == "user" and msgs[i + 1].get("role") != "user":
                pairs.append((None, _clean(msgs[i].get("content", "")),
                              None, _clean(msgs[i + 1].get("content", ""))))
    return pairs


def _texts(pair, names):
    an, at, bn, bt = pair
    q = f"{an}: {at}" if (names and an) else at
    a = f"{bn}: {bt}" if (names and bn) else bt
    return q, a


def convert_to_sharegpt(pairs, names=False):
    out = []
    for p in pairs:
        q, a = _texts(p, names)
        out.append({"conversations": [{"from": "human", "value": q},
                                      {"from": "gpt", "value": a}]})
    return out


def convert_to_llama(pairs, names=False):
    blocks = []
    for p in pairs:
        q, a = _texts(p, names)
        blocks.append(f"<|im_start|>user\n{q}\n<|im_end|>\n"
                      f"<|im_start|>assistant\n{a}\n<|im_end|>")
    return "\n\n".join(blocks)


def _esc(s):
    return s.replace("\\", "\\\\").replace('"', '\\"')


def convert_to_lisp(pairs, names=False):
    entries = []
    for p in pairs:
        q, a = _texts(p, names)
        entries.append(f'("{_esc(q)}" "{_esc(a)}" nil 1.0)')
    return "(" + " ".join(entries) + ")"


def main():
    ap = argparse.ArgumentParser(description="Конвертер логов в датасеты и память Марка")
    ap.add_argument("input", nargs="?", default="tg_logs.json",
                    help="входной JSON: Telegram result.json или список сессий")
    ap.add_argument("--out", default="dataset", help="префикс выходных файлов")
    ap.add_argument("--names", action="store_true",
                    help="добавлять 'Автор: ' в текст (для групповых чатов)")
    args = ap.parse_args()

    if not os.path.exists(args.input):
        print(f"Error: {args.input} not found.")
        return 1
    try:
        with open(args.input, encoding="utf-8") as f:
            data = json.load(f)
    except json.JSONDecodeError:
        print("Error: не удалось прочитать JSON.")
        return 2

    if isinstance(data, dict) and "messages" in data:
        pairs = from_telegram(data)
        print(f"формат: экспорт Telegram — {len(pairs)} пар")
    elif isinstance(data, list):
        pairs = from_sessions(data)
        print(f"формат: список сессий — {len(pairs)} пар")
    else:
        print("Error: не понимаю формат (нужен Telegram result.json или список сессий).")
        return 2

    sg = f"{args.out}_sharegpt.json"
    lm = f"{args.out}_llama.txt"
    lp = f"{args.out}_lisp.lisp"
    with open(sg, "w", encoding="utf-8") as f:
        json.dump(convert_to_sharegpt(pairs, args.names), f, ensure_ascii=False, indent=2)
    with open(lm, "w", encoding="utf-8") as f:
        f.write(convert_to_llama(pairs, args.names))
    with open(lp, "w", encoding="utf-8") as f:
        f.write(convert_to_lisp(pairs, args.names))

    print("✅ Conversion complete!")
    for f in (sg, lm, lp):
        print(" -", f)
    return 0


if __name__ == "__main__":
    sys.exit(main())
