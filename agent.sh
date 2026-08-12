#!/bin/bash
# запуск агента марка с целью: ./agent.sh "покажи отчёт"
cd "$(dirname "$0")"
GOAL="${1:-покажи отчёт}"
sbcl --script /dev/stdin <<EOF
(load "core.lisp")
(load-memory)
(load-macros)
(load-codes)
(format t "~%=== марк-агент стартует ===~%")
(agent-run "$GOAL")
(format t "~%=== журнал действий: ===~%")
(agent-log-show)
EOF
