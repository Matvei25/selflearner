#!/usr/bin/env python3
"""МАРК: генератор текста — марковская цепь 2-го порядка (без нейросети)
Персоны: отдельный корпус на каждую (corpus_<имя>.txt) — генерация "по духу".
Subcommands:
  generate [seed [temp]]              -- общий корпус corpus.txt
  learn текст                          -- дописать в общий корпус
  learn-file путь                      -- влить файл в общий корпус
  stats                                -- строк в общем корпусе
  persona-generate <имя> [seed [temp]] -- генерация в духе персоны
  persona-learn <имя> текст            -- дописать фразу в корпус персоны
  persona-stats <имя>                  -- строк в корпусе персоны
"""
import json, random, re, sys, os

HERE = os.path.dirname(os.path.abspath(__file__))
CORPUS = os.path.join(HERE, "corpus.txt")
MODEL = os.path.join(HERE, "model.json")


def persona_corpus(name):
    """файл корпуса персоны — имя очищается от спецсимволов."""
    safe = re.sub(r"[^а-яёa-z0-9_-]", "-", name.strip().lower()) or "никто"
    return os.path.join(HERE, f"corpus_{safe}.txt")


def tokens(text):
    return re.findall(r"[а-яёa-z0-9]+", text.lower())


def build(corpus=CORPUS):
    chain = {}  # {(w1,w2): {w3: count}}
    try:
        with open(corpus, encoding="utf-8") as f:
            for line in f:
                t = tokens(line)
                for i in range(len(t) - 2):
                    key = (t[i], t[i + 1])
                    d = chain.setdefault(key, {})
                    d[t[i + 2]] = d.get(t[i + 2], 0) + 1
    except FileNotFoundError:
        pass
    return chain


def pick(counter, temp):
    """выбор слова с температурой: weight = count^(1/temp)
    temp=1 — как раньше; temp>1 — разнообразнее; temp<1 — предсказуемее"""
    items = list(counter.items())
    ws = [max(c, 1e-9) ** (1.0 / temp) for _, c in items]
    total = sum(ws)
    r = random.random() * total
    acc = 0.0
    for (w, _), weight in zip(items, ws):
        acc += weight
        if r <= acc:
            return w
    return items[-1][0]


def generate(seed="ага", limit=15, temp=1.0, corpus=CORPUS):
    chain = build(corpus)
    if not chain:
        return "..."
    keys = [k for k in chain if k[0] == seed] or list(chain)
    key = random.choice(keys)
    out = list(key)
    for _ in range(limit):
        nxt = chain.get(tuple(out[-2:]))
        if not nxt:
            break
        out.append(pick(nxt, temp))
    return " ".join(out)


def learn(text, corpus=CORPUS):
    with open(corpus, "a", encoding="utf-8") as f:
        f.write(text.strip() + "\n")


def learn_file(path, corpus=CORPUS):
    with open(path, encoding="utf-8") as f:
        content = f.read()
    if content:
        with open(corpus, "a", encoding="utf-8") as out:
            out.write(content if content.endswith("\n") else content + "\n")


def stats(corpus=CORPUS):
    print(f"корпус строк: {sum(1 for _ in open(corpus, encoding='utf-8'))}")


if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "generate"
    if cmd == "generate":
        seed = sys.argv[2] if len(sys.argv) > 2 else "ага"
        temp = float(sys.argv[3]) if len(sys.argv) > 3 else 1.0
        print(generate(seed, 15, temp))
    elif cmd == "learn":
        learn(" ".join(sys.argv[2:]))
    elif cmd == "learn-file":
        learn_file(sys.argv[2])
    elif cmd == "stats":
        stats()
    elif cmd == "persona-generate":
        name = sys.argv[2]
        seed = sys.argv[3] if len(sys.argv) > 3 else "ага"
        temp = float(sys.argv[4]) if len(sys.argv) > 4 else 1.0
        print(generate(seed, 15, temp, persona_corpus(name)))
    elif cmd == "persona-learn":
        name = sys.argv[2]
        learn(" ".join(sys.argv[3:]), persona_corpus(name))
    elif cmd == "persona-stats":
        stats(persona_corpus(sys.argv[2]))
