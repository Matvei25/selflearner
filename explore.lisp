;;;; МАРК explore.lisp — отбор всего: память, правила, макросы, функции, персоны
;;;;
;;;; Идея (ISLM в применении к МАРКу):
;;;;   состояние агента = снимок (память, память персоны, правила, коды, макросы, активная персона)
;;;;   переход           = мутация одной части этого снимка
;;;;   экзамен           = набор пар «вопрос => ожидаемый ответ», по нему считается приспособленность
;;;;   журнал            = все состояния с оценками, любое можно вернуть командой (откат N)
;;;;
;;;; Грузится ПОСЛЕ core.lisp. Ядро не меняется.
;;;;   sbcl --script brain.lisp
;;;;   затем в REPL: (load "explore.lisp")
;;;;
;;;; Команды:
;;;;   (экзамен)                       — показать экзамен
;;;;   (экзамен-из-памяти [сколько])    — собрать экзамен из памяти: одно слово вопроса -> ответ
;;;;   (экзамен-добавить "вопрос" "ответ")
;;;;   (фитнес)                         — оценка текущего состояния
;;;;   (мутация)                        — сделать одну случайную мутацию и оценить
;;;;   (поколение [бюджет])             — beam search: мутации -> отбор -> следующее поколение
;;;;   (журнал)                         — все состояния: id, мутация, оценка, родитель
;;;;   (дерево)                         — журнал деревом
;;;;   (откат N)                        — вернуть состояние N
;;;;   (слои)                           — что вообще лежит в состоянии (размеры)

(in-package :cl-user)

;;; ---------- экзамен ----------

(defvar *exam* '() "((вопрос ожидаемый-ответ) ...)")
(defvar *conf-step* 0.1)
(defvar *junk-conf* 0.5)
(defvar *merge-sim* 0.6)

(defun экзамен-добавить (q a)
  (setf *exam* (cons (list q a) (remove-if (lambda (p) (string= (normalize (first p)) (normalize q)))
                                           *exam*)))
  (format t "экзамен: ~a => ~a~%" q a))

(defun экзамен-из-памяти (&optional (сколько 20))
  "Взять из памяти пары и спросить по ОДНОМУ слову вопроса — проверка не заучивания, а обобщения."
  (setf *exam* '())
  (let ((pairs (subseq *memory* 0 (min сколько (length *memory*)))))
    (dolist (e pairs)
      (let* ((q (first e)) (a (second e))
             (ws (words q))
             (hint (or (first ws) (first (words-all q)))))
        (when (and hint a (not (string-equal hint a)))
          (push (list hint a) *exam*))))
    (setf *exam* (nreverse *exam*))
    (format t "экзамен собран из памяти: ~a вопросов~%" (length *exam*))
    (length *exam*)))

(defun экзамен ()
  (if (null *exam*)
      (format t "экзамен пуст. (экзамен-из-памяти) или (экзамен-добавить \"вопрос\" \"ответ\")~%")
      (progn
        (format t "экзамен (~a):~%" (length *exam*))
        (dolist (p *exam*)
          (format t "  ~a => ~a~%" (first p) (second p)))
        (length *exam*))))

;;; ---------- состояние ----------

(defun слой-размеры (&optional (s (снимок)))
  (format nil "память ~a, персона-память ~a, правила ~a, коды ~a, макросы ~a"
          (length (nth 0 s)) (length (nth 1 s)) (length (nth 2 s))
          (length (nth 3 s)) (length (nth 4 s))))

(defun снимок ()
  "Полное состояние агента. Всё — списки, копируем, чтобы прошлое не мутировало."
  (list (copy-tree *memory*)
        (copy-tree *persona-memory*)
        (copy-tree *rule-stat*)
        (copy-tree *codes*)
        (copy-tree *macros*)
        *persona*))

(defun восстановить (s)
  (setf *memory* (copy-tree (nth 0 s)))
  (setf *persona-memory* (copy-tree (nth 1 s)))
  (setf *rule-stat* (copy-tree (nth 2 s)))
  (setf *codes* (copy-tree (nth 3 s)))
  (setf *macros* (copy-tree (nth 4 s)))
  (setf *persona* (nth 5 s))
  ;; заново собрать функции из кодов — тело могло измениться
  (dolist (c *codes*) (ignore-errors (compile-code c)))
  s)

;;; ---------- приспособленность ----------

(defun фитнес-точность (mem)
  "Доля экзамена, на которую агент ответил ожидаемым ответом."
  (if (null *exam*) 0.0
      (let ((ok 0))
        (dolist (p *exam*)
          (let ((hit (recall-in (first p) mem)))
            (when (and hit (string-equal (normalize (second hit)) (normalize (second p))))
              (incf ok))))
        (/ (float ok) (length *exam*)))))

(defun фитнес (&optional (s (снимок)) verbose)
  "Счёт = точность на экзамене − штраф за раздутость памяти − штраф за мёртвый груз."
  (let* ((mem (append (nth 0 s) (nth 1 s)))
         (acc (фитнес-точность mem))
         (size (length mem))
         (dead (count-if (lambda (r) (<= (third r) 0)) (nth 2 s)))
         (score (- acc (* 0.002 size) (* 0.01 dead))))
    (when verbose
      (format t "  точность ~,2f  пар ~a  мёртвых правил ~a  => счёт ~,4f~%"
              acc size dead score))
    score))

;;; ---------- журнал ----------

(defstruct (кадр (:constructor make-кадр (id мутация слой снимок оценка родитель)))
  id мутация слой снимок оценка родитель)

(defvar *journal* nil)
(defvar *next-id* 0)
(defvar *current* nil)

(defun записать (мутация слой s оценка родитель)
  (let ((k (make-кадр (incf *next-id*) мутация слой (copy-tree s) оценка родитель)))
    (push k *journal*)
    (setf *current* k)
    k))

(defun кадр (id) (find id *journal* :key #'кадр-id))

(defun журнал ()
  (format t "~%  id  слой     мутация                       счёт     родитель~%")
  (dolist (k (reverse *journal*))
    (format t "  ~3d  ~7a  ~28a  ~,4f  ~@[~a~]~a~%"
            (кадр-id k)
            (format nil "~a" (кадр-слой k))
            (format nil "~a" (кадр-мутация k))
            (кадр-оценка k)
            (кадр-родитель k)
            (if (eq k *current*) "   <= здесь" "")))
  (length *journal*))

(defun дерево (&optional (root nil))
  (let ((kids (make-hash-table)))
    (dolist (k *journal*)
      (push k (gethash (кадр-родитель k) kids)))
    (labels ((walk (k d)
               (format t "~%~a~a ~a  [~,4f]"
                       (make-string (* 2 d) :initial-element #\space)
                       (кадр-id k) (кадр-мутация k) (кадр-оценка k))
               (dolist (c (gethash (кадр-id k) kids)) (walk c (1+ d)))))
      (let ((starts (if root (list (кадр root))
                        (remove-if (lambda (k) (кадр-родитель k)) (reverse *journal*)))))
        (dolist (k starts) (walk k 0))))
    (terpri)))

(defun откат (id)
  (let ((k (кадр id)))
    (unless k (error "нет кадра ~a" id))
    (восстановить (кадр-снимок k))
    (setf *current* k)
    (format t "откат в ~a (~a), ~a~%" id (кадр-мутация k) (слой-размеры))
    (кадр-оценка k)))

;;; ---------- мутации ----------

(defvar *learn-from-exam* t  "разрешать мутации «выучить проваленный вопрос»")
(defvar *explore-verbose* t)
(defvar *beam* 3)
(defvar *mutations-per-node* 14)

(defun случайный (lst) (and lst (nth (random (length lst)) lst)))

(defun пара-похожесть (a b)
  (word-jaccard (or (words (first a)) (words-all (first a)))
                (or (words (first b)) (words-all (first b)))))

(defun коротко (s &optional (n 24))
  (subseq s 0 (min n (length s))))

(defun провалы (mem)
  "Пары экзамена, на которые эта память отвечает неправильно."
  (remove-if (lambda (p)
               (let ((hit (recall-in (first p) mem)))
                 (and hit (string-equal (normalize (second hit)) (normalize (second p))))))
             *exam*))

(defun мутировать-память (mem)
  "Случайная правка памяти. Возвращает (описание . новая-память) или nil."
  (let ((op (random 5)))
    (cond
      ;; 1. уверенность вниз/вверх
      ((and (= op 0) mem)
       (let* ((i (random (length mem)))
              (d (if (zerop (random 2)) *conf-step* (- *conf-step*)))
              (copy (copy-tree mem))
              (old (fourth (nth i copy)))
              (new (max 0.0 (min 1.0 (+ old d)))))
         (when (/= old new)
           (setf (fourth (nth i copy)) new)
           (cons (format nil "уверенность ~a: ~,2f->~,2f" (коротко (first (nth i copy)) 16) old new)
                 copy))))
      ;; 2. выкинуть джанк
      ((and (= op 1) mem)
       (let ((junk (remove-if-not (lambda (e) (< (fourth e) *junk-conf*)) mem)))
         (when junk
           (let ((v (car junk)))
             (cons (format nil "дроп джанка: ~a" (коротко (first v) 20))
                   (remove v mem :test #'equal))))))
      ;; 3. слить две похожие
      ((and (= op 2) (>= (length mem) 2))
       (let ((bi nil) (bj nil) (best +1.0))
         (dotimes (i (length mem))
           (loop for j from (1+ i) below (length mem) do
             (let ((sim (пара-похожесть (nth i mem) (nth j mem))))
               (when (and (> sim best) (>= sim *merge-sim*))
                 (setf best sim bi i bj j)))))
         (when bi
           (let* ((a (nth bi mem))
                  (b (nth bj mem))
                  (keep (if (>= (fourth a) (fourth b)) a b))
                  (other (if (eq keep a) b a))
                  (merged (list (first keep) (second keep) (third keep)
                                (min 1.0 (+ (fourth keep) (fourth other)))))
                  (copy nil))
             (dotimes (i (length mem))
               (unless (or (= i bi) (= i bj))
                 (push (copy-tree (nth i mem)) copy)))
             (push merged copy)
             (cons (format nil "merge ~a + ~a" (коротко (first a) 14) (коротко (first b) 14))
                   (nreverse copy))))))
      ;; 4. выучить проваленный вопрос (экзамен как учитель)
      ((and (= op 3) *learn-from-exam*)
       (let* ((bad (провалы mem))
              (p (случайный bad)))
         (when p
           (let ((known (find (normalize (first p)) mem
                              :test #'string= :key (lambda (e) (normalize (first e))))))
             (unless known
               (cons (format nil "выучить: ~a => ~a" (коротко (first p) 18) (коротко (second p) 14))
                     (cons (list (first p) (second p) nil 1.0) (copy-tree mem))))))))
      ;; 5. выкинуть пару, которая на экзамене ни разу не выиграла и не нужна
      ((and (= op 4) (> (length mem) 1))
       (let* ((needed (mapcar (lambda (p)
                                (let ((hit (recall-in (first p) mem)))
                                  (and hit (car hit))))
                              *exam*))
              (spare (remove-if (lambda (e) (member (first e) needed :test #'string=)) mem))
              (v (случайный spare)))
         (when v
           (cons (format nil "убрать лишнее: ~a" (коротко (first v) 20))
                 (remove v mem :test #'equal))))))))

(defun мутировать-правила (rp)
  (let ((op (random 2)) (copy nil))
    (cond
      ((and (= op 0) rp)
       (let ((dead (remove-if-not (lambda (r) (<= (third r) 0)) rp)))
         (when dead
           (setf copy (copy-tree rp))
           (setf (third (assoc (first (car dead)) copy :test #'equal)) *rule-max-hp*)
           (cons (format nil "воскресить ~a" (second (first (car dead)))) copy))))
      (rp
       (let* ((weak (first (sort (copy-tree rp) #'< :key #'third))))
         (setf copy (copy-tree rp))
         (setf (third (assoc (first weak) copy :test #'equal)) 0.0)
         (cons (format nil "убить ~a" (second (first weak))) copy))))))

(defun мутировать-коды (codes)
  (when (> (length codes) 1)
    (let* ((i (random (length codes)))
           (copy (copy-tree codes)))
      (cons (format nil "убрать функцию ~a" (first (nth i copy)))
            (remove (nth i copy) copy :test #'equal)))))

(defun применить-мутацию (s m)
  "Собрать новое состояние из старого с одной заменой слоя."
  (let ((layer (car m)) (payload (cdr m)))
    (case layer
      (:memory (list payload (nth 1 s) (nth 2 s) (nth 3 s) (nth 4 s) (nth 5 s)))
      (:rules  (list (nth 0 s) (nth 1 s) payload (nth 3 s) (nth 4 s) (nth 5 s)))
      (:codes  (list (nth 0 s) (nth 1 s) (nth 2 s) payload (nth 4 s) (nth 5 s)))
      (t s))))

(defun случайная-мутация (s)
  "Один случайный переход из состояния s: (описание слой новое-состояние) или nil."
  (let ((r (random 100)))
    (cond
      ((< r 60)
       (when (nth 0 s)
         (let ((m (мутировать-память (nth 0 s))))
           (when m (list (car m) :memory (применить-мутацию s (cons :memory (cdr m))))))))
      ((< r 90)
       (let ((m (мутировать-правила (nth 2 s))))
         (when m (list (car m) :rules (применить-мутацию s (cons :rules (cdr m)))))))
      (t
       (let ((m (мутировать-коды (nth 3 s))))
         (when m (list (car m) :codes (применить-мутацию s (cons :codes (cdr m))))))))))

(defun все-мутации (&optional (s (снимок)))
  "N случайных разных кандидатов-переходов из состояния s."
  (let ((out nil) (tries 0))
    (loop while (and (< (length out) *mutations-per-node*) (< tries (* 6 *mutations-per-node*)))
          do (incf tries)
             (let ((m (случайная-мутация s)))
               (when (and m (not (assoc (first m) out :test #'string=)))
                 (push m out))))
    out))

;;; ---------- переход и отбор ----------

(defun шаг (мутация слой s родитель)
  (let* ((score (фитнес s))
         (k (записать мутация слой s score родитель)))
    (восстановить s)
    (when *explore-verbose*
      (format t "~%   ~a [~a] => ~,4f" мутация слой score))
    k))

(defun поколение (&optional (бюджет 200))
  "Beam search по состояниям агента: мутации -> оценка -> лучшие раскрываются дальше."
  (unless *exam* (экзамен-из-памяти))
  (unless *current*
    (записать 'старт :state (снимок) (фитнес (снимок) t) nil)
    (format t "старт: ~a~%" (слой-размеры)))
  (let* ((base (кадр-снимок *current*))
         (base-score (кадр-оценка *current*))
         (frontier (list (cons *current* 0)))
         (made 0))
    (loop while (and frontier (< made бюджет))
          do (let* ((item (pop frontier))
                    (k (car item))
                    (d (cdr item)))
               (let ((cands (все-мутации (кадр-снимок k))))
                 (let ((scored nil))
                   (dolist (c cands)
                     (when (>= made бюджет) (return))
                     (let* ((layer (second c))
                            (s (third c))
                            (sc (фитнес s)))
                       (incf made)
                       (push (list sc c s) scored)))
                   ;; отбираем лучших в этом поколении
                   (setf scored (sort scored #'> :key #'first))
                   (dotimes (i (min *beam* (length scored)))
                     (destructuring-bind (sc c s) (nth i scored)
                       (when (> sc (кадр-оценка k))
                         (let ((nk (записать (first c) (second c) s sc
                                             (кадр-id k))))
                           (push (cons nk (1+ d)) frontier)))))))))
    (let ((best (car (sort (copy-list *journal*) #'> :key #'кадр-оценка))))
      (format t "~%сгенерировано состояний: ~a, лучший кадр ~a со счётом ~,4f~%"
              made (кадр-id best) (кадр-оценка best))
      (if (> (кадр-оценка best) base-score)
          (progn
            (восстановить (кадр-снимок best))
            (setf *current* best)
            (format t "принято: ~a (~a), теперь ~a~%"
                    (кадр-мутация best) (кадр-слой best) (слой-размеры))
            (кадр-оценка best))
          (progn
            (восстановить base)
            (setf *current* (кадр (кадр-родитель best)))
            (format t "ничего лучше не нашлось — состояние не меняю~%")
            base-score)))))

(defun мутация ()
  (let* ((s (if *current* (кадр-снимок *current*) (снимок)))
         (c (случайный (все-мутации s))))
    (unless c (format t "мутировать нечего~%") (return-from мутация nil))
    (let ((before (фитнес s)) (k (шаг (first c) (second c) (third c) (and *current* (кадр-id *current*)))))
      (format t "~%~a => ~,4f (было ~,4f)~%" (first c) (кадр-оценка k) before)
      (кадр-оценка k))))

(defun слои ()
  (format t "~a~%" (слой-размеры))
  (format t "  экзамен: ~a вопросов, beam: ~a~%" (length *exam*) *beam*)
  (длина-истории))

(defun длина-истории () (length *journal*))

;;; ---------- подключение к чату МАРКА ----------
;;; core.lisp разбирает «(команда аргументы)» через call-by-name, который смотрит в *tools*.
;;; Регистрируемся там при загрузке — ядро не правим.

(defun w-экзамен (&optional (arg ""))
  (declare (ignore arg))
  (when (null *exam*) (экзамен-из-памяти))
  (экзамен)
  nil)

(defun w-фитнес (&optional (arg ""))
  (declare (ignore arg))
  (фитнес (снимок) t)
  nil)

(defun w-поколение (&optional (arg "150"))
  (поколение (or (parse-integer (string-trim " " (string arg)) :junk-allowed t) 150))
  nil)

(defun w-журнал (&optional (arg "")) (declare (ignore arg)) (журнал) nil)
(defun w-дерево-состояний (&optional (arg "")) (declare (ignore arg)) (дерево) nil)
(defun w-сохранить (&optional (arg ""))
  (declare (ignore arg))
  (handler-case (progn (persist-memory) (format t "память сохранена на диск~%"))
    (error (e) (format t "не сохранилось: ~a~%" e)))
  nil)

(defun w-слои (&optional (arg "")) (declare (ignore arg)) (слои) nil)

(defun w-откат (&optional (arg ""))
  (let ((id (parse-integer (string-trim " " (string arg)) :junk-allowed t)))
    (if id (откат id) (format t "нужен номер кадра: (откат 12)~%")))
  nil)

(defun w-экзамен-добавить (&optional (arg ""))
  (let* ((s (string arg))
         (pos (search "=>" s)))
    (if pos
        (экзамен-добавить (string-trim " " (subseq s 0 pos))
                          (string-trim " " (subseq s (+ 2 pos))))
        (format t "формат: (экзамен-добавить вопрос => ответ)~%")))
  nil)

(defun w-экзамен-из-памяти (&optional (arg ""))
  (экзамен-из-памяти (or (parse-integer (string-trim " " (string arg)) :junk-allowed t) 20))
  nil)

(defvar *explore-tools* nil "наши команды, которым нужны аргументы")

(defun подключить-отбор ()
  (dolist (entry '(("экзамен" "экзамен по памяти: (экзамен)" w-экзамен)
                   ("экзамен-из-памяти" "собрать экзамен: (экзамен-из-памяти 20)" w-экзамен-из-памяти)
                   ("экзамен-добавить" "(экзамен-добавить вопрос => ответ)" w-экзамен-добавить)
                   ("фитнес" "оценка состояния агента: (фитнес)" w-фитнес)
                   ("поколение" "отбор состояний: (поколение 150)" w-поколение)
                   ("журнал" "все состояния отбора: (журнал)" w-журнал)
                   ("дерево-состояний" "дерево отбора: (дерево-состояний)" w-дерево-состояний)
                   ("откат" "вернуть состояние: (откат 12)" w-откат)
                   ("слои" "размеры слоёв состояния: (слои)" w-слои)
                   ("сохранить" "записать память на диск: (сохранить)" w-сохранить)))
    (push entry *explore-tools*)
    (unless (assoc (first entry) *tools* :test #'string-equal)
      (push entry *tools*)))
  ;; run-tool в ядре по умолчанию зовёт функцию БЕЗ аргументов — оборачиваем,
  ;; чтобы наши команды получали строку аргументов. core.lisp не меняется.
  (let ((orig (and (fboundp 'run-tool) (symbol-function 'run-tool))))
    (setf (symbol-function 'run-tool)
          (lambda (name args)
            (let ((hit (find name *explore-tools* :key #'first :test #'string-equal)))
              (if hit
                  (handler-case (funcall (third hit) args)
                    (error (e) (format t "ошибка отбора: ~a~%" e)))
                  (funcall orig name args))))))
  (format t "отбор подключён: (экзамен) (фитнес) (поколение 150) (журнал) (откат N) (слои)~%"))

(подключить-отбор)
