#!/bin/bash
# марк говорит сам с собой
# запуск: ./selfchat.sh [число шагов]
cd "$(dirname "$0")"
N="${1:-15}"
sbcl --script selfchat.lisp "$N"
