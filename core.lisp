;;; МАРК core.lisp — мозг (память + инструменты + process-message)
;;; Загружается из brain.lisp (чат) и selfchat.lisp (само-диалог)

(require :asdf)

(defvar *memory-file* (merge-pathnames "memory.lisp" *load-pathname*))
(defvar *self-dir* (uiop:pathname-directory-pathname *load-pathname*))
(defvar *markov-script* (merge-pathnames "markov.py" *load-pathname*))
(defvar *memory* '())        ; ((вопрос ответ уверенность) ...)
(defvar *guess-count* 0)
(defvar *last-q* nil)        ; последний вопрос, чтобы поправка знала куда писать
(defvar *temperature* 1.2)   ; температура генерации маркова (>1 — разнообразнее)
(defvar *macros* '())        ; ((имя (параметры...) шаблон [auto]) ...) — объявлено заранее
(defvar *codes* '())         ; ((имя (аргументы...) "тело") ...) — объявлено заранее

;; ---------- персона (v0.9: марк становится персонажем) ----------
(defvar *personas* '())          ; реестр: ((имя (характер...)) ...)
(defvar *persona* nil)           ; имя активной персоны или nil
(defvar *persona-memory* '())    ; память активной персоны (вопрос ответ вызов уверенность)
(defvar *personas-file* (merge-pathnames "persons.lisp" *load-pathname*))

(defparameter *stop-words*
  '("что" "как" "это" "а" "ну" "и" "в" "на" "по" "не" "я" "ты" "он" "она" "они" "мы" "вы" "то" "да" "нет" "у" "о" "же"
    "такой" "такая" "такое" "такие" "сам" "сама" "само" "этот" "эта" "эти" "просто" "вообще" "только" "ещё"
    "если" "бы" "ли" "или" "но" "за" "из" "от" "до" "под" "над" "при" "с" "со" "к" "ко" "об" "про" "без" "для"
    "уже" "вот" "раз" "где" "когда" "почему" "зачем" "какой" "какая" "какие" "чем" "чём" "типа" "короче"))

;; ---------- память ----------

(defun load-memory ()
  (setf *memory* '())
  (when (probe-file *memory-file*)
    (handler-case
        (with-open-file (in *memory-file* :direction :input)
          ;; читаем ВСЕ формы (не только первую) и сливаем
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (when (listp form)
                     (setf *memory* (append *memory* form)))))
      (error (e)
        ;; битый файл — не молчим: бэкап + предупреждение
        (format t "~&⚠ память не читается (~a)~%  делаю бэкап в memory.lisp.broken~%" e)
        (handler-case
            (uiop:run-program (list "cp" (namestring *memory-file*)
                                    (namestring (merge-pathnames "memory.lisp.broken" *load-pathname*)))
                              :output :string :ignore-error-status t)
          (error () nil))
        (setf *memory* '()))))
  ;; миграция: пары -> (вопрос ответ вызов уверенность), тройки -> 4-ки
  (setf *memory*
        (mapcar (lambda (e)
                  (cond
                    ((= (length e) 2) (list (first e) (second e) nil 1.0))
                    ((= (length e) 3) (list (first e) (second e) nil (third e)))
                    (t e)))
                *memory*)))

(defun save-memory ()
  "сохранить память. защита: не затираем непустой файл пустой памятью"
  (let ((has-data (and (probe-file *memory-file*)
                       (handler-case
                           (with-open-file (in *memory-file*)
                             (not (eq (read in nil :eof) :eof)))
                         (error () nil)))))
    (unless (and (null *memory*) has-data)
      (with-open-file (out *memory-file* :direction :output :if-exists :supersede)
        (with-standard-io-syntax
          (let ((*print-case* :downcase) (*print-pretty* t))
            (prin1 *memory* out)))))))

(defun persist-memory ()
  "сохранить память туда, где она сейчас живёт (персона или общая)"
  (if *persona*
      (persona-save-memory *persona*)
      (save-memory)))

;; ---------- утилиты ----------

(defun normalize (text)
  (remove-if-not (lambda (c) (or (alphanumericp c) (char= c #\Space)))
                 (string-downcase text)))

(defun words (text)
  (remove-if (lambda (w) (member w *stop-words* :test #'string=))
             (remove "" (uiop:split-string (normalize text) :separator '(#\Space)) :test #'string=)))

(defun words-all (text)
  "все слова без фильтра стоп-слов"
  (remove "" (uiop:split-string (normalize text) :separator '(#\Space)) :test #'string=))

;; ---------- ИНСТРУМЕНТЫ ----------

(defun recall-in (q mem)
  "найти лучший ответ по пересечению слов в конкретном списке (запись или nil)"
  (let ((qw (or (words q) (words-all q))) (best nil) (best-score 0))
    (dolist (entry mem)
      ;; пропускаем самоповторы (вопрос => тот же вопрос) — мусор от догадок
      (unless (string-equal (first entry) (second entry))
        (let* ((mw (words (first entry)))
               (score (length (intersection qw (or mw (words-all (first entry))) :test #'string=))))
          ;; v0.5: при равном совпадении побеждает БОЛЕЕ уверенная запись
          (when (or (> score best-score)
                    (and (= score best-score) (>= best-score 1)
                         (> (fourth entry) (fourth best))))
            (setf best entry best-score score)))))
    (when (and best (>= best-score 1)) best)))

(defun recall (q)
  "найти лучший ответ: сначала в памяти персоны (если есть), потом в общей"
  (or (recall-in q *persona-memory*)
      (recall-in q *memory*)))

(defun remember (q a &optional (conf 1.0) (verbose t))
  "запомнить пару (перезаписывает тот же вопрос). если активна персона — в её память"
  (let ((norm (normalize q)))
    (if *persona*
        (progn
          (setf *persona-memory*
                (cons (list q a nil conf)
                      (remove-if (lambda (e) (string= (normalize (first e)) norm)) *persona-memory*)))
          (persona-save-memory *persona*))
        (progn
          (setf *memory*
                (cons (list q a nil conf)
                      (remove-if (lambda (e) (string= (normalize (first e)) norm)) *memory*)))
          (save-memory))))
  (when verbose
    (format t "запомнил: ~a => ~a~%" q a)))

(defun forget (q)
  (let ((norm (normalize q)))
    (if *persona*
        (let ((before (length *persona-memory*)))
          (setf *persona-memory*
                (remove-if (lambda (e) (string= (normalize (first e)) norm)) *persona-memory*))
          (persona-save-memory *persona*)
          (format t "забыл ~a записей~%" (- before (length *persona-memory*))))
        (let ((before (length *memory*)))
          (setf *memory*
                (remove-if (lambda (e) (string= (normalize (first e)) norm)) *memory*))
          (save-memory)
          (format t "забыл ~a записей~%" (- before (length *memory*)))))))

;; ---------- v0.5: обратная связь + / - ----------

(defvar *praise-count* 0)
(defvar *critic-count* 0)

(defun current-entry ()
  "найти запись, которую марк использовал последней (по *last-q*) — в персоне и в общей"
  (when *last-q*
    (let ((norm (normalize *last-q*)))
      (or (find-if (lambda (e) (string= (normalize (first e)) norm)) *persona-memory*)
          (find-if (lambda (e) (string= (normalize (first e)) norm)) *memory*)))))

(defun praise ()
  "плюс: ответ сработал — закрепить запись (уверенность -> 1.0)"
  (let ((entry (current-entry)))
    (cond
      ((null entry)
       (format t "нечего хвалить — сначала дождись ответа~%"))
      (t
       (setf (fourth entry) 1.0)
       (incf *praise-count*)
       (persist-memory)
       (format t "👍 запомнил, что сработало: ~a => ~a [точно]~%"
               (first entry) (second entry))))))

(defun criticize ()
  "минус: ответ не сработал — понизить уверенность; слабые записи удаляются"
  (let ((entry (current-entry)))
    (cond
      ((null entry)
       (format t "нечего ругать — сначала дождись ответа~%"))
      ((<= (or (fourth entry) 0.0) 0.4)
       ;; догадка не сработала — выбрасываем совсем
       (let ((q (first entry)))
         (forget q)
         (incf *critic-count*)
         (format t "👎 выбросил догадку: ~a~%" q)))
      (t
       (setf (fourth entry) 0.2)
       (incf *critic-count*)
       (persist-memory)
       (format t "👎 понизил уверенность: ~a => ~a [догадка]~%"
               (first entry) (second entry))))))

(defun improvise (&optional (seed "ага"))
  "сгенерировать текст марковской цепью (с температурой).
если активна персона — генерирует в её духе из её корпуса"
  (let ((args (if *persona*
                  (list "python3" (namestring *markov-script*) "persona-generate"
                        *persona* (or seed "ага") (format nil "~a" *temperature*))
                  (list "python3" (namestring *markov-script*) "generate"
                        (or seed "ага") (format nil "~a" *temperature*)))))
    (let ((out (uiop:run-program args :output :string :ignore-error-status t)))
      (if (uiop:emptyp out) "..." (string-trim '(#\Newline #\Space) out)))))

(defun absorb (text)
  "впитать текст в корпус — учится на всём, что слышит"
  (uiop:run-program (list "python3" (namestring *markov-script*) "learn" text)
                    :output :string :ignore-error-status t))

;; ---------- v0.8: сны — ночная консолидация памяти ----------

(defvar *since-dream* 0)              ; реплик с прошлого сна
(defparameter *dream-every* 25)       ; раз в столько реплик Марк засыпает
(defparameter *dream-junk-conf* 0.5)  ; догадки-пустышки ниже этой уверенности — в мусор
(defparameter *dream-sim* 0.75)       ; схожесть вопросов для слияния (доля общих слов)

(defun dream-junk-p (e)
  "мусор: пустой/бессмысленный ответ, вопрос-эхо, или пустой вопрос"
  (let ((q (first e)) (a (second e)) (c (or (fourth e) 0.0)))
    (or (null a)
        (string= a "")
        (string-equal a "...")
        (string-equal (normalize q) (normalize a))
        (and (< c *dream-junk-conf*) (null (words q))))))

(defun word-set (s)
  (remove-duplicates (words s) :test #'string=))

(defun word-jaccard (a b)
  "доля общих слов из объединения (0..1)"
  (let ((inter (intersection a b :test #'string=))
        (un (union a b :test #'string=)))
    (if (null un) 0.0 (/ (float (length inter)) (length un)))))

(defun dream-merge-similar (mem)
  "слить почти-дубли (схожесть >= *dream-sim*), оставив более уверенную запись"
  (let ((out '()) (merged 0))
    (dolist (e mem)
      (let ((twin (find-if (lambda (o)
                             (>= (word-jaccard (word-set (first o)) (word-set (first e)))
                                 *dream-sim*))
                           out)))
        (cond
          ((null twin) (push e out))
          ((> (or (fourth e) 0.0) (or (fourth twin) 0.0))
           (setf out (cons e (remove twin out)))   ; более уверенная вытесняет
           (incf merged))
          (t (incf merged)))))                      ; уже принятый twin сильнее — оставляем его
    (values (nreverse out) merged)))

(defun dream-dedup (mem)
  "убрать точные дубли по нормализованному вопросу, оставив самый уверенный"
  (let ((seen '()) (out '()))
    (dolist (e mem)
      (let* ((k (normalize (first e)))
             (prev (assoc k seen :test #'string=)))
        (cond
          ((null prev) (push (cons k e) seen) (push e out))
          ((> (or (fourth e) 0.0) (or (fourth (cdr prev)) 0.0))
           (setf out (cons e (remove (cdr prev) out)))   ; вытесняем слабый дубль
           (setf (cdr prev) e)))))
    out))

(defun dream (&optional (verbose t))
  "v0.8 — ночная пересборка памяти: мусор вон, похожие вопросы сливаются,
дубли схлопываются. чистит память активной персоны, если она есть, иначе общую"
  (let* ((before (length (if *persona* *persona-memory* *memory*)))
         (mem (remove-if #'dream-junk-p (if *persona* *persona-memory* *memory*)))
         (junk (- before (length mem))))
    (multiple-value-bind (mem2 merged) (dream-merge-similar mem)
      (let ((clean (dream-dedup mem2)))
        (if *persona*
            (progn (setf *persona-memory* clean) (persona-save-memory *persona*))
            (progn (setf *memory* clean) (save-memory)))
        (setf *since-dream* 0)
        (when verbose
          (format t "🌙 снилось: мусора ~a · слито ~a · было ~a, стало ~a~%"
                  junk merged before (length clean)))
        (length clean)))))

(defun stats ()
  (let ((guesses (count-if (lambda (e) (< (or (fourth e) 0.0) 0.9)) *memory*)))
    (format t "знаю пар: ~a (из них слабых: ~a)~%" (length *memory*) guesses)
    (format t "догадок за сессию: ~a · оценок: ~a 👍 / ~a 👎~%"
            *guess-count* *praise-count* *critic-count*)))

(defun show-memory ()
  (if (null *memory*)
      (format t "память пуста~%")
      (dolist (e *memory*)
        (format t "~a => ~a~@[ (вызов ~a)~] [~a]~%" (first e) (second e) (third e)
                (if (>= (fourth e) 0.9) "точно" (if (>= (fourth e) 0.5) "почти" "догадка"))))))

(defun parse-number (s)
  (handler-case (let ((v (read-from-string s))) (when (numberp v) v))
    (error () nil)))

(defun set-temp (x)
  (let ((v (parse-number x)))
    (if v
        (progn (setf *temperature* v) (format t "температура: ~a~%" v))
        (format t "формат: !temp 1.5~%"))))

(defparameter *tools*
  '(("remember"  "запомнить: (remember вопрос => ответ)"     remember)
    ("recall"    "поиск: (recall вопрос)"                    recall)
    ("improvise" "сгенерить: (improvise [слово])"            improvise)
    ("absorb"    "впитать текст: (absorb текст)"             absorb)
    ("dream"     "ночная пересборка памяти (мусор вон, дубли вместе)"  dream)
    ("forget"    "забыть: (forget вопрос)"                   forget)
    ("stats"     "статистика"                                stats)
    ("memory"    "показать всю память"                       show-memory)
    ("temp"      "температура генерации: (temp 1.5)"         set-temp)
    ("macro"     "создать макрос: (macro имя (x) \"текст {x}\")" add-macro)
    ("macros"    "показать макросы"                           show-macros)
    ("run"       "выполнить макрос: (run имя аргументы)"     run-macro)
    ("macro-forget" "удалить макрос: (macro-forget имя)"     macro-forget)
    ("macro-learn"  "обучиться макросам из памяти"            macro-learn)
    ("code"      "определить функцию: (code имя (x) \"(format nil ... x)\")" handle-code-form)
    ("codes"     "показать функции"                           show-codes)
    ("code-forget" "удалить функцию: (code-forget имя)"      code-forget)
    ("goal"      "поставить цель агенту: (goal \"покажи отчёт\")" agent-run)
    ("status"    "состояние агента"                           agent-status)
    ("log"       "журнал действий агента"                    agent-log-show)
    ("stop"      "остановить агента"                          agent-stop)
    ("персона"         "включить/показать/снять персону: (персона имя|[ничего]|-)" persona-cmd)
    ("персона-define"  "определить персону: (персона-define имя характер...)" persona-define)
    ("персоны"         "список персон"                         persona-list)
    ("персона-учить"   "впитать фразу в персону: (персона-учить текст)" persona-teach)
    ("персона-реплика" "сгенерить в духе персоны: (персона-реплика [слово])" persona-speak)
    ("правила"   "кто жив, кто умер: (правила)"               rules-show)
    ("эволюция"  "суд поколения прямо сейчас: (эволюция)"      evolve-rules)
    ("воскресить" "вернуть мёртвое правило: (воскресить имя)"  rule-revive)
    ("help"      "справка по инструментам"                   help)))

(defun help ()
  (format t "марк v0.9 — инструменты (вызывай как в лиспе: (имя ...)):~%")
  (dolist (t* *tools*)
    (format t "  (~a ...) — ~a~%" (first t*) (second t*)))
  (format t "просто болтай — марк сам учится. поправка после догадки: правильно: ответ~%")
  (format t "оценка ответа: + (сработало, закрепить) или - (не сработало, выбросить)~%")
  (format t "персона: (персона имя) включить, (персона) показать, (персона -) снять — марк говорит в её духе~%")
  (format t "правила живут и умирают: (правила) — кто жив, (эволюция) — суд, (воскресить имя) — вернуть~%"))

(defun run-tool (name args)
  (let ((tool (find name *tools* :key #'first :test #'string-equal)))
    (if tool
        (let ((fn (third tool)))
          (cond
            ((eq fn 'remember)
             (let ((pos (search "=>" args)))
               (if pos
                   (remember (string-trim " " (subseq args 0 pos))
                             (string-trim " " (subseq args (+ pos 2))))
                   (format t "формат: !remember вопрос => ответ~%"))))
            ((eq fn 'recall)
             (let ((r (recall args)))
               (if r (format t "~a~%" (second r)) (format t "не помню~%"))))
            ((eq fn 'improvise)
             (format t "~a~%" (improvise (if (uiop:emptyp args) "ага" args))))
            ((eq fn 'absorb) (absorb args) (format t "впитал~%"))
            ((eq fn 'forget) (forget args))
            ((eq fn 'set-temp) (set-temp args))
            ((eq fn 'add-macro)
             (let* ((arrow (search "=>" args))
                    (sp (position #\Space args))
                    (name (if sp (subseq args 0 sp) args))
                    (rest (if sp (string-trim " " (subseq args (1+ sp))) ""))
                    (lp (position #\( rest))
                    (rp (position #\) rest))
                    (params (if (and lp rp (> rp lp))
                                (remove "" (uiop:split-string (subseq rest (1+ lp) rp)
                                                              :separator '(#\Space)) :test #'string=)
                                '()))
                    (template (cond
                                (arrow (string-trim " " (subseq args (+ arrow 2))))
                                ((and lp rp (> rp lp)) (string-trim " " (subseq rest (1+ rp))))
                                (t rest))))
               (add-macro name params template)))
            ((eq fn 'run-macro)
             (let* ((sp (position #\Space args))
                    (name (if sp (subseq args 0 sp) args))
                    (rest (if sp (string-trim " " (subseq args (1+ sp))) "")))
               (run-macro name (if (uiop:emptyp rest)
                                   '()
                                   (uiop:split-string rest :separator '(#\Space))))))
            ((eq fn 'macro-forget) (macro-forget args))
            ((eq fn 'agent-run) (agent-run args))
            ((eq fn 'rule-revive) (rule-revive args))
            (t (funcall fn))))
        (format t "нет такого инструмента. !help~%"))))

;; ---------- самостоятельный выбор инструментов ----------

(defparameter *tool-rules*
  '(("сколько знаешь" "stats")
    ("статистик" "stats")
    ("покажи память" "memory")
    ("что помнишь" "memory")
    ("вся память" "memory")
    ("забудь" "forget")
    ("сгенерируй" "improvise")
    ("придумай" "improvise")
    ("сочини" "improvise")
    ("почисти память" "dream")
    ("приберись" "dream")
    ("поспи" "dream")
    ("умеешь" "help")
    ("что умеешь" "help")
    ("помощь" "help")
    ("инструменты" "help")
    ("стань" "персона")
    ("кем ты" "персона")
    ("включи персону" "персона")))

(defun detect-tool (text)
  "самостоятельный выбор инструмента по ключевым словам -> (name args) или nil"
  (let ((norm (normalize text)))
    (dolist (rule *tool-rules* nil)
      (let ((pos (and (rule-alive-p "инструмент" (first rule)) (search (first rule) norm))))
        (when pos
          (rule-touch "инструмент" (first rule))
          (return-from detect-tool
            (list (second rule)
                  (string-trim " " (subseq norm (+ pos (length (first rule))))))))))))

(defun call-by-name (name args)
  "вызвать инструмент, макрос или определённую функцию по имени"
  (cond
    ((find name *tools* :key #'first :test #'string-equal)
     (run-tool name (format nil "~{~a~^ ~}" args)))
    ((assoc name *macros* :test #'string-equal)
     (format t "~a~%" (macro-expand name args)))
    ((assoc name *codes* :key #'first :test #'string-equal)
     (handler-case
         (format t "~a~%" (apply (symbol-function (intern (string-upcase name))) args))
       (error (e) (format t "ошибка вызова: ~a~%" e))))
    (t (format t "нет такого инструмента, макроса или функции: ~a~%" name))))

(defun handle-code-form (args)
  "обработать (code имя (арг...) тело) — сырые объекты, без конвертации в строку"
  (when (>= (length args) 2)
    (let* ((nm (first args))
           (params (second args))
           (body (if (> (length args) 2) (third args) ""))
           (name (string-downcase (if (stringp nm) nm (format nil "~a" nm))))
           (plist (if (listp params)
                      (mapcar (lambda (p) (string-downcase (format nil "~a" p))) params)
                      '()))
           (bstr (if (stringp body) body (format nil "~a" body))))
      (add-code name plist bstr))))

(defun str-args (items)
  "список аргументов -> строка через пробел (символы — строчными, строки как есть)"
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (a)
                    (if (stringp a)
                        a
                        (string-downcase (format nil "~a" a))))
                  items)))

(defun execute-call (call)
  "выполнить вызов из записи памяти: (имя аргументы...) — инструмент или макрос"
  (when (and (listp call) call)
    (let ((name (string-downcase (symbol-name (first call))))
          (args (rest call)))
      (call-by-name name (if (every #'stringp args)
                             args
                             (uiop:split-string (str-args args) :separator '(#\Space)))))))

;; ---------- ELIZA (классика 1966) ----------

(defparameter *eliza-rules*
  '(("я хочу" "почему ты хочешь ~a?")
    ("я не могу" "что тебе мешает ~a?")
    ("я боюсь" "чего ты боишься — ~a?")
    ("я ненавижу" "почему ты ненавидишь ~a?")
    ("я люблю" "что тебе нравится в ~a?")
    ("меня" "расскажи больше про ~a")
    ("моя" "расскажи про свою ~a")
    ("мой" "расскажи про свой ~a")
    ("всегда" "можешь привести пример, когда это всегда?")
    ("никогда" "точно ли никогда?")
    ("почему" "почему ты так думаешь?")
    ("все" "все? кто именно?")
    ("никто" "совсем никто?")
    ("ты" "почему ты говоришь обо мне?")))

(defun mirror (text)
  "отзеркалить фразу: я->ты, моя->твоя, меня->тебя..."
  (let ((repl '(("я" . "ты") ("меня" . "тебя") ("мне" . "тебе")
                ("мой" . "твой") ("моя" . "твоя") ("моё" . "твоё")
                ("мои" . "твои") ("мы" . "вы") ("нас" . "вас"))))
    (format nil "~{~a~^ ~}"
            (mapcar (lambda (w)
                      (let ((hit (assoc w repl :test #'string=)))
                        (if hit (cdr hit) w)))
                    (words-all text)))))

(defun eliza-reply (text)
  "элиза-рефлексы: найти правило, отзеркалить хвост фразы. nil если не сработало"
  (let ((norm (normalize text)))
    (dolist (rule *eliza-rules* nil)
      (let ((pos (and (rule-alive-p "элиза" (first rule)) (search (first rule) norm))))
        (when pos
          (rule-touch "элиза" (first rule))
          (let ((tail (string-trim " " (subseq norm (+ pos (length (first rule)))))))
            (return-from eliza-reply
              (if (string= tail "")
                  (second rule)  ; правило без хвоста — без подстановки
                  (format nil (second rule) (mirror tail))))))))))


;; ---------- ПРАВИЛА: ЖИЗНЬ И СМЕРТЬ (v0.7) ----------
;; у каждого правила есть здоровье: сработало — растёт, не срабатывало за поколение — падает.
;; на нуле правило умирает (перестаёт применяться), но его можно воскресить.

(defvar *rules-file* (merge-pathnames "rules.lisp" *self-dir*))
(defvar *rule-stat* '())        ; (((вид имя) за-поколение здоровье всего) ...)
(defvar *generation* 1)
(defvar *epoch-ticks* 0)
(defparameter *epoch-size* 20)      ; сколько услышанных реплик = одно поколение
(defparameter *rule-max-hp* 3.0)
(defparameter *rule-reward* 0.5)
(defparameter *rule-decay* 1.0)

(defun rule-key (kind name)
  (list (string-downcase (format nil "~a" kind))
        (string-downcase (format nil "~a" name))))

(defun rule-entry (kind name)
  (assoc (rule-key kind name) *rule-stat* :test #'equal))

(defun rule-register (kind name)
  "рождение: правило попадает в реестр, если его там ещё нет"
  (or (rule-entry kind name)
      (car (push (list (rule-key kind name) 0 (coerce *rule-max-hp* 'float) 0)
                 *rule-stat*))))

(defun rule-alive-p (kind name)
  (let ((e (rule-entry kind name)))
    (or (null e) (> (third e) 0))))

(defun rule-older-p (a b)
  (string< (format nil "~a/~a" (first (first a)) (second (first a)))
           (format nil "~a/~a" (first (first b)) (second (first b)))))

(defun rule-touch (kind name)
  "правило сработало — счётчик и здоровье вверх"
  (let ((e (rule-register kind name)))
    (incf (second e))
    (incf (fourth e))
    (setf (third e) (min (coerce *rule-max-hp* 'float)
                         (+ (third e) *rule-reward*)))))

(defun save-rules ()
  (handler-case
      (with-open-file (out *rules-file* :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
        (format out ";; правила марка — здоровье и статистика (его жизнь)~%(~%")
        (dolist (e *rule-stat*)
          (format out " (~s ~a ~,2f ~a)~%" (first e) (second e) (third e) (fourth e)))
        (format out " (generation ~a))~%" *generation*))
    (error (e) (format t "~~ не смог сохранить правила: ~a~%" e))))

(defun load-rules ()
  (when (probe-file *rules-file*)
    (handler-case
        (with-open-file (in *rules-file*)
          (let ((data (read in nil nil)))
            (when (listp data)
              (setf *rule-stat*
                    (loop for item in data
                          when (and (listp item) (listp (first item)))
                            collect (list (first item) (second item)
                                          (coerce (third item) 'float) (fourth item))))
              (dolist (item data)
                (when (and (listp item) (eq (first item) 'generation))
                  (setf *generation* (second item)))))))
      (error (e) (format t "~~ не смог прочитать правила: ~a~%" e)))))

(defun evolve-rules (&optional (verbose t))
  "суд поколения: чем не пользовались — теряет здоровье, на нуле умирает"
  (let ((died '()) (survived 0))
    (dolist (e *rule-stat*)
      (if (> (second e) 0)
          (progn
            (incf survived)
            (setf (third e) (min (coerce *rule-max-hp* 'float)
                                 (+ (third e) *rule-reward*))))
          (progn
            (decf (third e) *rule-decay*)
            (when (<= (third e) 0)
              (setf (third e) 0.0)
              (push (second (first e)) died))))
      (setf (second e) 0))
    (setf *epoch-ticks* 0)
    (incf *generation*)
    (save-rules)
    (when verbose
      (format t "поколение ~a: выжило ~a, умерло ~a~@[ — ~{~a~^, ~}~]~%"
              *generation* survived (length died) (reverse died)))
    (values)))

(defun rules-show ()
  (if (null *rule-stat*)
      (format t "правил в реестре нет~%")
      (progn
        (format t "правила — поколение ~a, до суда ещё ~a реплик~%"
                *generation* (max 0 (- *epoch-size* *epoch-ticks*)))
        (let ((live 0) (dead 0))
          (dolist (e (sort (copy-list *rule-stat*) #'rule-older-p))
            (let ((kind (first (first e)))
                  (name (second (first e)))
                  (hp (third e))
                  (total (fourth e)))
              (if (<= hp 0)
                  (progn
                    (incf dead)
                    (format t "  ☠ ~a/~a — мертво (срабатывало ~a раз)~%" kind name total))
                  (progn
                    (incf live)
                    (format t "  ~a ~a/~a — здоровье ~,1f, срабатывало ~a~%"
                            (if (>= hp *rule-max-hp*) "живо" "слабо") kind name hp total)))))
          (format t "живых: ~a, мёртвых: ~a. воскресить: (воскресить имя)~%" live dead)))))

(defun rule-revive (args)
  "вернуть мёртвое правило к жизни: (воскресить имя)"
  (let ((name (string-downcase (string-trim " " (format nil "~a" (or args ""))))))
    (if (uiop:emptyp name)
        (format t "формат: (воскресить имя-правила)~%")
        (let ((found '()))
          (dolist (e *rule-stat*)
            (when (string= (second (first e)) name)
              (setf (third e) (coerce *rule-max-hp* 'float))
              (setf (second e) 1)
              (push (first (first e)) found)))
          (if found
              (progn
                (save-rules)
                (format t "воскресил ~{~a~^, ~} — здоровье ~a~%" found *rule-max-hp*))
              (format t "нет правила с именем ~a~%" name))))))

(defun rule-tick ()
  "каждая услышанная реплика — тик; на границе поколения суд"
  (incf *epoch-ticks*)
  (when (>= *epoch-ticks* *epoch-size*)
    (evolve-rules t))
  (when (zerop (mod *epoch-ticks* 5))
    (save-rules)))

(defun rule-seed ()
  "внести встроенные правила в реестр"
  (dolist (r *eliza-rules*) (rule-register "элиза" (first r)))
  (dolist (r *tool-rules*) (rule-register "инструмент" (first r))))

(load-rules)
(rule-seed)

;; ---------- МАКРОСЫ ----------

(defvar *macros-file* (merge-pathnames "macros.lisp" *load-pathname*))
(defvar *macros* '())  ; ((имя (параметры...) шаблон) ...)

(defun load-macros ()
  (setf *macros* '())
  (when (probe-file *macros-file*)
    (handler-case
        (with-open-file (in *macros-file* :direction :input)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (when (listp form) (setf *macros* (append *macros* form)))))
      (error () nil))))

(defun save-macros ()
  (with-open-file (out *macros-file* :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-case* :downcase) (*print-pretty* t))
        (prin1 *macros* out)))))

(defun str-replace-all (old new s)
  "заменить все вхождения old на new в строке s"
  (let ((pos (search old s)))
    (if pos
        (str-replace-all old new
                         (concatenate 'string (subseq s 0 pos) new
                                      (subseq s (+ pos (length old)))))
        s)))

(defun macro-expand (name args)
  "развернуть макрос: подставить аргументы в шаблон {параметр} (регистр не важен)"
  (let ((m (assoc name *macros* :test #'string-equal)))
    (when m
      (let ((result (third m)))
        (loop for p in (second m)
              for a in args
              do (setf result (str-replace-all (format nil "{~a}" (string-downcase p))
                                               (format nil "~a" a) result))
                 (setf result (str-replace-all (format nil "{~a}" (string-upcase p))
                                               (format nil "~a" a) result)))
        result))))

(defun add-macro (name params template)
  (setf *macros* (cons (list (string-downcase name) params template)
                       (remove-if (lambda (m) (string-equal (first m) name)) *macros*)))
  (save-macros)
  (format t "макрос ~a (~{~a~^ ~}) создан~%" (string-downcase name) params))

(defun show-macros ()
  (if (null *macros*)
      (format t "макросов нет. !macro имя (x) => текст с {x}~%")
      (dolist (m *macros*)
        (format t "~a (~{~a~^ ~}) => ~a~%" (first m) (second m) (third m)))))

(defun run-macro (name args)
  (let ((exp (macro-expand name args)))
    (if exp
        (format t "~a~%" exp)
        (format t "нет такого макроса: ~a~%" name))))

(defun macro-forget (name)
  (setf *macros* (remove-if (lambda (m) (string-equal (first m) name)) *macros*))
  (save-macros)
  (format t "макрос ~a забыт~%" name))

(defun macro-learn ()
  "обучение макросам: пары 'W X => ответ' становятся макросами W(x) => ответ"
  (let ((made 0))
    (dolist (entry *memory*)
      (let* ((qw (words-all (first entry)))
             (name (first qw)))
        (when (and name (= (length qw) 2)
                   (not (assoc name *macros* :test #'string-equal)))
          (push (list name (list "x") (second entry) t) *macros*)  ; t = авто, не перехватывает диалог
          (incf made))))
    (save-macros)
    (format t "обучено макросов: ~a~%" made)))

(defun try-macro-call (text)
  "если текст начинается с имени РУЧНОГО макроса — развернуть его с остатком как аргументами"
  (let ((words (words-all text)))
    (when words
      (let ((m (assoc (first words) *macros* :test #'string-equal)))
        (when (and m (not (fourth m)))  ; авто-макросы не перехватывают диалог
          (macro-expand (first words) (rest words)))))))

;; ---------- САМОПРОГРАММИРОВАНИЕ (функции на лету) ----------

(defvar *codes-file* (merge-pathnames "codes.lisp" *load-pathname*))
(defvar *codes* '())  ; ((имя (аргументы...) "тело-выражение") ...)

(defun load-codes ()
  (setf *codes* '())
  (when (probe-file *codes-file*)
    (handler-case
        (with-open-file (in *codes-file* :direction :input)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (when (listp form) (setf *codes* (append *codes* form)))))
      (error () nil)))
  ;; компилируем сохранённые функции
  (dolist (c *codes*)
    (compile-code c)))

(defun save-codes ()
  (with-open-file (out *codes-file* :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-case* :downcase) (*print-pretty* t))
        (prin1 *codes* out)))))

(defun compile-code (c)
  "собрать defun из (имя (арги) тело) и выполнить"
  (handler-case
      (let ((form (read-from-string
                   (format nil "(defun ~a (~{~a~^ ~}) ~a)"
                           (first c) (second c) (third c)))))
        (eval form)
        t)
    (error (e) (format t "⚠ код не скомпилировался: ~a~%" e) nil)))

(defun add-code (name params body)
  (setf *codes* (cons (list (string-downcase name) params body)
                      (remove-if (lambda (c) (string-equal (first c) name)) *codes*)))
  (if (compile-code (list (string-downcase name) params body))
      (progn (save-codes) (format t "функция ~a (~{~a~^ ~}) определена~%" (string-downcase name) params))
      (format t "функция не сохранена~%")))

(defun show-codes ()
  (if (null *codes*)
      (format t "функций нет. (code имя (x) \"(format nil ... x)\")~%")
      (dolist (c *codes*)
        (format t "~a (~{~a~^ ~}) => ~a~%" (first c) (second c) (third c)))))

(defun code-forget (name)
  (setf *codes* (remove-if (lambda (c) (string-equal (first c) name)) *codes*))
  (save-codes)
  (format t "функция ~a забыта~%" name))

;; ---------- НАСТОЯЩИЙ АГЕНТ: цели, планировщик, цикл ----------

(defvar *agent-goals* '())  ; очередь целей (строки)
(defvar *agent-plan* '())   ; текущий план: (("инструмент" "аргументы") ...)
(defvar *agent-log* '())    ; журнал действий
(defvar *agent-busy* nil)

(defun log-agent (fmt &rest args)
  "записать действие в журнал и показать"
  (push (apply #'format nil fmt args) *agent-log*)
  (format t "  [агент] ~a~%" (apply #'format nil fmt args)))

(defun agent-think (goal)
  "ПЛАНИРОВЩИК: цель -> последовательность инструментов (эвристики по словам)"
  (let ((g (normalize goal)))
    (cond
      ((or (search "отчёт" g) (search "отчет" g)
           (search "покажи всё" g) (search "сколько" g))
       '(("stats" "") ("macros" "") ("codes" "") ("memory" "")))
      ((or (search "учись" g) (search "обучись" g)
           (search "выучи" g) (search "тренир" g))
       '(("macro-learn" "") ("dream" "") ("stats" "")))
      ((or (search "чисти" g) (search "прибери" g)
           (search "убери" g) (search "порядок" g))
       '(("dream" "") ("stats" "")))
      ((or (search "поговори" g) (search "сам с собой" g))
       '(("improvise" "привет") ("stats" "")))
      (t '(("help" "") ("stats" ""))))))

(defun agent-step ()
  "один шаг цикла: выполнить первый пункт плана"
  (if (null *agent-plan*)
      (progn
        (setf *agent-busy* nil)
        (format t "~%  [агент] ✔ цель выполнена~%")
        nil)
      (let* ((step (pop *agent-plan*))
             (tool (first step))
             (args (second step)))
        (log-agent "шаг: (~a ~a)" tool args)
        (call-by-name tool (if (uiop:emptyp args)
                               '()
                               (uiop:split-string args :separator '(#\Space))))
        t)))

(defun agent-run (goal)
  "ПОЛНЫЙ ЦИКЛ: план -> выполнение шагов (макс 10, чтобы не зациклился)"
  (setf *agent-goals* (append *agent-goals* (list goal)))
  (setf *agent-plan* (agent-think goal))
  (setf *agent-busy* t)
  (log-agent "цель: ~a" goal)
  (format t "  [агент] план: ~{~a~^, ~}~%" (mapcar #'first *agent-plan*))
  (loop repeat 10 while *agent-busy* do (agent-step))
  (when *agent-busy*
    (setf *agent-busy* nil)
    (format t "  [агент] лимит шагов — план обрезан~%")))

(defun agent-status ()
  (format t "целей в очереди: ~a~%план: ~a~%занят: ~a~%"
          (length *agent-goals*) *agent-plan* *agent-busy*))

(defun agent-log-show ()
  (if (null *agent-log*)
      (format t "журнал пуст — агент ещё ничего не делал~%")
      (dolist (e (reverse *agent-log*)) (format t "~a~%" e))))

(defun agent-stop ()
  (setf *agent-plan* '() *agent-busy* nil)
  (format t "агент остановлен~%"))

;; ---------- ПЕРСОНЫ (марк становится персонажем) ----------

(defun load-personas ()
  (setf *personas* '())
  (when (probe-file *personas-file*)
    (handler-case
        (with-open-file (in *personas-file* :direction :input)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (when (listp form) (setf *personas* (append *personas* form)))))
      (error () nil))))

(defun save-personas ()
  (with-open-file (out *personas-file* :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-case* :downcase) (*print-pretty* t))
        (prin1 *personas* out)))))

(defun person-memory-file (name)
  (merge-pathnames (format nil "memory_~a.lisp"
                           (string-downcase (string-trim " " name)))
                   *self-dir*))

(defun persona-load-memory (name)
  (let ((f (person-memory-file name)))
    (setf *persona-memory* '())
    (when (probe-file f)
      (handler-case
          (with-open-file (in f :direction :input)
            (loop for form = (read in nil :eof)
                  until (eq form :eof)
                  do (when (listp form) (setf *persona-memory* (append *persona-memory* form)))))
        (error () nil)))
    ;; миграция 2/3 -> 4 (вопрос ответ вызов уверенность)
    (setf *persona-memory*
          (mapcar (lambda (e)
                    (cond ((= (length e) 2) (list (first e) (second e) nil 1.0))
                          ((= (length e) 3) (list (first e) (second e) nil (third e)))
                          (t e)))
                  *persona-memory*))))

(defun persona-save-memory (name)
  (with-open-file (out (person-memory-file name) :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-case* :downcase) (*print-pretty* t))
        (prin1 *persona-memory* out)))))

(defun find-persona (name)
  (assoc name *personas* :test #'string-equal))

(defun persona-define (name &rest traits)
  "определить/перезаписать персону: (персона-define имя [характер ...])"
  (let ((n (string-downcase (string-trim " " name))))
    (setf *personas*
          (cons (list n (mapcar #'string-downcase traits))
                (remove-if (lambda (p) (string-equal (first p) n)) *personas*)))
    (save-personas)
    (format t "персона ~a определена: ~{~a~^, ~}~%" n traits)))

(defun persona-set (name)
  "активировать персону (загрузить её память). нет такой — создаст пустую"
  (let ((n (string-downcase (string-trim " " name))))
    (when *persona* (persona-save-memory *persona*))
    (unless (find-persona n) (persona-define n))
    (setf *persona* n)
    (persona-load-memory n)
    (format t "персона: ~a (помнит ~a пар)~%" n (length *persona-memory*))))

(defun persona-clear ()
  (when *persona* (persona-save-memory *persona*))
  (setf *persona* nil *persona-memory* '())
  (format t "персона снята~%"))

(defun persona-current ()
  (if *persona*
      (let ((p (find-persona *persona*)))
        (format t "активна персона: ~a" *persona*)
        (when (and p (second p)) (format t " (~{~a~^, ~})" (second p)))
        (format t "~%помнит ~a пар.~%память:~%" (length *persona-memory*))
        (dolist (e *persona-memory*)
          (format t "  ~a => ~a [~a]~%" (first e) (second e)
                  (if (>= (fourth e) 0.9) "точно"
                      (if (>= (fourth e) 0.5) "почти" "догадка")))))
      (format t "персона не задана~%")))

(defun persona-list ()
  (if (null *personas*)
      (format t "персон нет. (персона-define имя характер)~%")
      (dolist (p *personas*)
        (format t "~a~@[ (~{~a~^, ~})~]~%" (first p) (second p)))))

(defun persona-teach (text)
  "научить активную персону фразе (в её корпус). пример: (персона-учить привет дружище)"
  (uiop:run-program (list "python3" (namestring *markov-script*) "persona-learn"
                          (or *persona* "никто") text)
                    :output :string :ignore-error-status t)
  (format t "персона ~a впитала фразу~%" (or *persona* "никто")))

(defun persona-speak (&optional (seed "ага"))
  "сгенерировать реплику в духе активной персоны (из её корпуса)"
  (let ((out (uiop:run-program (list "python3" (namestring *markov-script*)
                                     "persona-generate" (or *persona* "никто")
                                     (or seed "ага") (format nil "~a" *temperature*))
                               :output :string :ignore-error-status t)))
    (if (uiop:emptyp out) "..." (string-trim '(#\Newline #\Space) out))))

(defun persona-cmd (args)
  "обработать (персона ...): пусто — показать текущую; -/none — снять; иначе — включить"
  (let ((a (string-trim " " (or args ""))))
    (cond
      ((string= a "") (persona-current))
      ((or (string-equal a "-") (string-equal a "none") (string-equal a "снять"))
       (persona-clear))
      (t (persona-set a)))))

;; ---------- обработка сообщения ----------

(defun process-message (l &optional (verbose t) (learn t))
  "обработать одно сообщение. печатает ответ если verbose, возвращает строку ответа (или nil)
learn=nil — не впитывать в корпус (для selfchat, чтобы не засорять корпус своим трёпом)"
  (let ((l (string-trim '(#\Newline #\Space) l)))
    (cond
      ((string-equal l "exit") :exit)
      ((string-equal l "help") (help) nil)
      ((or (string-equal l "+") (string-equal l "плюс"))
       (praise) nil)
      ((or (string-equal l "-") (string-equal l "минус"))
       (criticize) nil)
      ((uiop:string-prefix-p "(" l)
       ;; лисповская форма: (имя аргументы...)
       (handler-case
           (let ((form (read-from-string l)))
             (when (and (listp form) form)
               (let ((name (string-downcase (symbol-name (first form))))
                     (args (rest form)))
                 (if (string-equal name "code")
                     (handle-code-form args)
                     (call-by-name name (if (every #'stringp args)
                                            args
                                            (uiop:split-string (str-args args) :separator '(#\Space)))))))
             nil)
         (error () nil)))
      ((string-equal l "") nil)
      ((uiop:string-prefix-p "правильно:" l)
       (if *last-q*
           (remember *last-q* (string-trim " " (subseq l 10)))
           (format t "нечего исправлять~%"))
       nil)
      (t
       ;; каждая реплика — тик жизни правил (v0.7)
       (rule-tick)
       ;; v0.8: раз в *dream-every* реплик марк засыпает и пересобирает память
       (incf *since-dream*)
       (when (>= *since-dream* *dream-every*) (dream))
       ;; самостоятельный выбор инструмента по ключевым словам
       (let ((tool (detect-tool l)))
         (if tool
             (progn
               (run-tool (first tool) (second tool))
               nil)
             (progn
               ;; автообучение: всё, что слышит — впитывает (если learn)
               (when learn (absorb l))
               (let* ((hit (recall l))
                      (el (eliza-reply l))
                      (mx (try-macro-call l)))
                 (cond
                   ;; явный вызов макроса — приоритетнее всего
                   (mx
                    (setf *last-q* l)
                    (when verbose (format t "~a~%" mx))
                    mx)
                   ;; элиза важнее слабой догадки (conf < 0.5)
                   ((and el (or (null hit) (< (fourth hit) 0.5)))
                    (incf *guess-count*)
                    (setf *last-q* l)
                    (remember l el 0.4 nil)
                    (when verbose (format t "~a~%" el))
                    el)
                   ;; сильная память (conf >= 0.5)
                   (hit
                    (setf *last-q* (first hit))
                    (when verbose (format t "~a~%" (second hit)))
                    ;; вызов инструмента из записи — после ответа
                    (when (third hit) (execute-call (third hit)))
                    (second hit))
                   ;; элиза без памяти
                   (el
                    (incf *guess-count*)
                    (setf *last-q* l)
                    (remember l el 0.4 nil)
                    (when verbose (format t "~a~%" el))
                    el)
                   ;; марковская догадка
                   (t
                    (let ((g (improvise (or (first (words l)) "ага"))))
                      (incf *guess-count*)
                      (setf *last-q* l)
                      (unless (or (string= g "") (string-equal g l))
                        (remember l g 0.4 nil))
                      (when verbose (format t "~a~%" g))
                      g)))))))))))
