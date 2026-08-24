;;; МАРК v0.3 — чат-интерфейс (мозг в core.lisp)
;;; Запуск: ./run.sh
;;; Вид диалога: юзер: ... / марк: ...

(require :asdf)
(load (merge-pathnames "core.lisp" *load-pathname*))

(load-memory)
(load-macros)
(load-codes)

(format t "марк v0.3 — учусь сам. !help — инструменты.~%")

(loop for line = (read-line *standard-input* nil nil) while line do
  (let ((l (string-trim '(#\Newline #\Space) line)))
    (when (string-equal l "exit") (return))
    (unless (string= l "")
      (format t "юзер: ~a~%" l)
      (format t "марк: ")
      (process-message l))))
