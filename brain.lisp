;;; МАРК v0.5 — чат-интерфейс (мозг в core.lisp)
;;; Запуск: ./run.sh
;;; Вид диалога: юзер: ... / марк: ...

(require :asdf)
(load (merge-pathnames "core.lisp" *load-pathname*))

(load-memory)
(load-macros)
(load-codes)
(load-personas)

(format t "марк v0.9 — учусь сам. !help — инструменты. (персона имя) — включить персонажа.~%")

(loop for line = (read-line *standard-input* nil nil) while line do
  (let ((l (string-trim '(#\Newline #\Space) line)))
    (when (string-equal l "exit") (return))
    (unless (string= l "")
      (format t "юзер: ~a~%" l)
      (format t "марк: ")
      (process-message l))))
