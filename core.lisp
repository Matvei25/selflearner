;;; МАРК core.lisp — мозг (память + инструменты + process-message)
;;; Загружается из brain.lisp (чат) и selfchat.lisp (само-диалог)

(require :asdf)

(defvar *memory-file* (merge-pathnames "memory.lisp" *load-pathname*))
(defvar *markov-script* (merge-pathnames "markov.py" *load-pathname*))
(defvar *memory* '())        ; ((вопрос ответ уверенность) ...)
(defvar *guess-count* 0)
(defvar *last-q* nil)        ; последний вопрос, чтобы поправка знала куда писать
(defvar *temperature* 1.2)   ; температура генерации маркова (>1 — разнообразнее)
(defvar *macros* '())        ; ((имя (параметры...) шаблон [auto]) ...) — объявлено заранее
(defvar *codes* '())         ; ((имя (аргументы...) "тело") ...) — объявлено заранее

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

(defun recall (q)
  "найти лучший ответ по пересечению слов (возвращает запись или nil)"
  (let ((qw (or (words q) (words-all q))) (best nil) (best-score 0))
    (dolist (entry *memory*)
      ;; пропускаем самоповторы (вопрос => тот же вопрос) — мусор от догадок
      (unless (string-equal (first entry) (second entry))
        (let* ((mw (words (first entry)))
               (score (length (intersection qw (or mw (words-all (first entry))) :test #'string=))))
          (when (> score best-score)
            (setf best entry best-score score)))))
    (when (and best (>= best-score 1)) best)))

(defun remember (q a &optional (conf 1.0) (verbose t))
  "запомнить пару (перезаписывает тот же вопрос)"
  (let ((norm (normalize q)))
    (setf *memory*
          (cons (list q a nil conf)
                (remove-if (lambda (e) (string= (normalize (first e)) norm)) *memory*))))
  (save-memory)
  (when verbose
    (format t "запомнил: ~a => ~a~%" q a)))

(defun forget (q)
  (let ((norm (normalize q))
        (before (length *memory*)))
    (setf *memory* (remove-if (lambda (e) (string= (normalize (first e)) norm)) *memory*))
    (save-memory)
    (format t "забыл ~a записей~%" (- before (length *memory*)))))

(defun improvise (&optional (seed "ага"))
  "сгенерировать текст марковской цепью (с температурой)"
  (let ((out (uiop:run-program (list "python3" (namestring *markov-script*) "generate"
                                    seed (format nil "~a" *temperature*))
                               :output :string :ignore-error-status t)))
    (if (uiop:emptyp out) "..." (string-trim '(#\Newline #\Space) out))))

(defun absorb (text)
  "впитать текст в корпус — учится на всём, что слышит"
  (uiop:run-program (list "python3" (namestring *markov-script*) "learn" text)
                    :output :string :ignore-error-status t))

(defun dream ()
  "пересборка памяти: убрать дубликаты"
  (let ((before (length *memory*)))
    (setf *memory* (remove-duplicates *memory*
                                      :test (lambda (a b) (string= (normalize (first a)) (normalize (first b))))
                                      :from-end t))
    (save-memory)
    (format t "приснилось: почистил ~a, осталось ~a~%" (- before (length *memory*)) (length *memory*))))

(defun stats ()
  (format t "знаю пар: ~a (из них догадок: ~a)~%" (length *memory*) *guess-count*))

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
    ("dream"     "пересобрать память"                        dream)
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
    ("help"      "справка по инструментам"                   help)))

(defun help ()
  (format t "марк v0.3 — инструменты (вызывай как в лиспе: (имя ...)):~%")
  (dolist (t* *tools*)
    (format t "  (~a ...) — ~a~%" (first t*) (second t*)))
  (format t "просто болтай — марк сам учится. поправка после догадки: правильно: ответ~%"))

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
    ("инструменты" "help")))

(defun detect-tool (text)
  "самостоятельный выбор инструмента по ключевым словам -> (name args) или nil"
  (let ((norm (normalize text)))
    (dolist (rule *tool-rules* nil)
      (let ((pos (search (first rule) norm)))
        (when pos
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
      (let ((pos (search (first rule) norm)))
        (when pos
          (let ((tail (string-trim " " (subseq norm (+ pos (length (first rule)))))))
            (return-from eliza-reply
              (if (string= tail "")
                  (second rule)  ; правило без хвоста — без подстановки
                  (format nil (second rule) (mirror tail))))))))))

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

;; ---------- обработка сообщения ----------

(defun process-message (l &optional (verbose t) (learn t))
  "обработать одно сообщение. печатает ответ если verbose, возвращает строку ответа (или nil)
learn=nil — не впитывать в корпус (для selfchat, чтобы не засорять корпус своим трёпом)"
  (let ((l (string-trim '(#\Newline #\Space) l)))
    (cond
      ((string-equal l "exit") :exit)
      ((string-equal l "help") (help) nil)
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
