;;; МАРК: импорт датасета в память и корпус
;;; Запуск: sbcl --script import.lisp <файлы...>
;;;   *.lisp — пары (формат sft_lisp.lisp) -> память (уверенность 1.0)
;;;   *.txt  — строки (формат corpus.txt) -> корпус маркова
;;; Пары с тем же вопросом перезаписывают старые (датасет вытесняет догадки)

(require :asdf)
(load (merge-pathnames "core.lisp" *load-pathname*))

(load-memory)

(defun import-pairs (path)
  "добавить пары из s-expr файла ((вопрос ответ) ...) в память"
  (let ((added 0) (replaced 0))
    (handler-case
        (with-open-file (in path)
          (let ((data (read in)))
            (dolist (pair data)
              (when (and (listp pair) (>= (length pair) 2))
                (let ((norm (normalize (first pair))))
                  (if (find-if (lambda (e) (string= (normalize (first e)) norm)) *memory*)
                      (incf replaced)
                      (incf added))
                  (setf *memory*
                        (cons (if (= (length pair) 4)
                                  pair
                                  (list (first pair) (second pair) nil 1.0))
                              (remove-if (lambda (e) (string= (normalize (first e)) norm)) *memory*))))))))
      (error (e) (format t "⚠ не удалось прочитать ~a: ~a~%" path e)))
    (format t "пары: +~a новых, ~a заменено  <- ~a~%" added replaced path)))

(defun import-corpus (path)
  "добавить строки файла в корпус маркова"
  (uiop:run-program (list "python3" (namestring *markov-script*) "learn-file" path)
                    :output :string :ignore-error-status t)
  (format t "корпус пополнен  <- ~a~%" path))

(dolist (f (uiop:command-line-arguments))
  (if (uiop:string-suffix-p (string-downcase f) ".txt")
      (import-corpus f)
      (import-pairs f)))

(save-memory)
(format t "итого в памяти: ~a пар~%" (length *memory*))
