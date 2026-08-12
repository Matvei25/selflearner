;;; МАРК1 и МАРК2 говорят друг с другом — чат двух агентов
;;; Запуск: ./selfchat.sh [число шагов]
;;; марк1 (температура 1.0) и марк2 (температура 1.6) — две персоны,
;;; оба используют один мозг (core.lisp) и общую память, учатся друг на друге.

(require :asdf)
(load (merge-pathnames "core.lisp" *load-pathname*))

(load-memory)
(load-macros)
(load-codes)

(defun rand-word (text)
  "случайное первое слово из текста (для seed генератора), без стоп-слов"
  (or (first (words text)) (first (words-all text)) "марк"))

(defun fresh-answer (q prev &optional (history '()))
  "сгенерировать ответ, который не повторяет вопрос q, prev и недавнюю историю"
  (loop repeat 8
        for seed = (rand-word q) then (or (rand-word prev) (rand-word q))
        for c = (improvise (or seed ""))
        when (and (not (string= c ""))
                  (not (string-equal c q))
                  (not (string-equal c prev))
                  (not (member c history :test #'string-equal)))
          do (return c)
        finally (return (improvise ""))))  ; совсем застрял — случайная фраза

(defun self-chat (&optional (n 10) (start "привет марк2, о чём поговорим"))
  (format t "=== марк1 и марк2 говорят друг с другом — ~a шагов ===~%" n)
  (let ((q start)
        (prev nil)
        (history '()))  ; последние ответы, чтобы ловить циклы A->B->A->B
    (dotimes (i n)
      ;; марк1 задаёт вопрос, марк2 отвечает — и наоборот
      (let* ((mk1 (evenp i))
             (speaker (if mk1 "марк1" "марк2"))
             (answerer (if mk1 "марк2" "марк1")))
        ;; разные персоны: марк1 спокойный, марк2 дикий
        (setf *temperature* (if mk1 1.0 1.6))
        (format t "~%[~a] ~a: ~a~%" (1+ i) speaker q)
        (let ((a (process-message q nil nil)))  ; learn=nil — не засоряем корпус своим трёпом
          (cond
            ;; зациклился, пусто, повторился или уже было недавно — свежий ответ
            ((or (null a) (string= a "") (string-equal a q) (string-equal a prev)
                 (member a history :test #'string-equal))
             (setf a (fresh-answer q prev history)))
            ;; 25% — отвечающий сам задаёт встречный вопрос
            ((< (random 100) 25)
             (setf a (format nil "а что такое ~a?" (rand-word prev)))))
          (format t "   ~a: ~a~%" answerer a)
          (setf prev a)
          (setf history (append (list a) history))
          (when (> (length history) 12)
            (setf history (subseq history 0 12)))
          (setf q a)))))
  (terpri)
  (format t "=== итог ===~%")
  (stats))

(let ((n (or (parse-integer (first (uiop:command-line-arguments)) :junk-allowed t) 10)))
  (self-chat n))
