;;; Марк v0.6-веб — обвязка для веб-панели (не трогаю brain.lisp!)
;;; Читает строку из stdin -> отдаёт РОВНО одну строку с ответом в stdout.
;;; Протокол: при старте печатает READY. Каждый ответ — одна строка (переносы -> \n).
;;; Запуск:  sbcl --script api-brain.lisp
(require :asdf)
(load (merge-pathnames "core.lisp" *load-pathname*))

(load-memory)
(load-macros)
(load-codes)
(load-personas)
(load-agent-state)

;; экранируем переносы, чтобы ответ уместился в одну строку
(defun esc (s)
  (when s
    (with-output-to-string (o)
      (loop for c across s do
        (case c
          (#\Newline (write-string "\\n" o))
          (#\Tab     (write-string "\\t" o))
          (otherwise (write-char c o)))))))

(format t "READY~%")
(finish-output)

(loop for line = (read-line *standard-input* nil nil) while line do
  (let ((l (string-trim '(#\Newline #\Space) line)))
    (when (string-equal l "exit") (return))
    (unless (string= l "")
      ;; весь печатный шум инструментов гоним в stderr,
      ;; чтобы в stdout попал только чистый ответ Марка
      (let ((ans (let ((*standard-output* *error-output*))
                   (process-message l nil t))))
        (format t "~a~%" (or (esc ans) ""))
        (finish-output)))))
