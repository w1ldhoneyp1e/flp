#lang racket
;; Домашнее задание 1. Темы 1--2.
;; Функциональное и логическое программирование, осень 2026, Институт iSpring.
;;
;; Яшметов Кирилл, группа ПС-41
;;
;; Как пользоваться этим файлом.
;; 1. Переименуйте файл в homework-1-<Фамилия>.rkt и впишите себя выше.
;; 2. Код пишите как код, а рассуждения, выкладки по подстановочной модели
;;    и доказательства --- в комментариях под соответствующей задачей.
;; 3. У каждой задачи оставьте одну строку декларации об использовании ИИ
;;    и удалите вторую. Задача без декларации считается несделанной.
;; 4. Запускайте файл (racket homework-1-<Фамилия>.rkt или кнопка Run
;;    в DrRacket): примеры из условия оформлены как проверки, и каждая
;;    несовпадающая печатает ожидаемое и фактическое значение.
;;    Если проверки молчат, значит все примеры сходятся.
;; 5. Заглушки 'todo замените своим кодом. Все функции должны быть
;;    определены, даже если задача не решена: файл должен запускаться.
;; 6. К задаче 1.8 приложите transcript.md отдельным файлом.

(require rackunit)

;; ============================================================================
;; Общие определения (даны в условии, не меняйте)
;; ============================================================================

;; Слово --- список символов.
(define (word->list w) (string->list w))

;; Длина списка явной рекурсией (для задачи 1.4).
(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))]))

;; Небольшой словарь пятибуквенных слов.
(define words
  (map word->list
       '("топор" "ротор" "мотор" "робот" "табор"
         "баран" "сарай" "сахар" "халат" "канат"
         "карат" "парад" "народ" "салат" "касса"
         "масса" "крыса" "берег" "бегун" "бетон"
         "белок" "билет" "буква" "волна" "ворон"
         "ворот" "город" "горох" "гость" "дверь")))

;; Коммит --- список (автор добавлено удалено); сборка --- список (ветка исход секунды).
(define (commit-author c) (first c))
(define (commit-added c) (second c))
(define (commit-deleted c) (third c))
(define (build-branch b) (first b))
(define (build-status b) (second b))
(define (build-seconds b) (third b))

;; ============================================================================
;; Задача 1.1. Списки явной рекурсией
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.

(define (index-of x lst)
  (define (go acc tail)
    (cond
      [(empty? tail) #f]
      [(equal? x (first tail)) acc]
      [else (go (add1 acc) (rest tail))]))
  (go 0 lst))

(define (dedupe-adjacent lst)
  (cond
    [(empty? lst) empty]
    [else 
      (define (go prev deduped tail)
        (cond
          [(empty? tail) deduped]
          [(equal? prev (first tail)) (go prev deduped (rest tail))]
          [else (go (first tail) (append deduped (list (first tail))) (rest tail))]))
      (go (first lst) (list (first lst)) (rest lst))]))

(define (merge-sorted a b)
  (define (go result curr lst-a lst-b)
    (cond
      [(empty? lst-a) (append result (cons curr lst-b))]
      [(empty? lst-b) (append result (cons curr lst-a))]
      [(<= curr (first lst-b)) (go (append result (list curr)) (first lst-a) (rest lst-a) lst-b)]
      [else (go (append result (list (first lst-b))) curr lst-a (rest lst-b))]))
  (cond
    [(and (empty? a) (empty? b)) empty]
    [(empty? a) (go empty (first b) a (rest b))]
    [else (go empty (first a) (rest a) b)]))

(define (rotate-left lst k) ; Дополнил условие: при отрицательных k возвращаю поданный список
  (define (go count result tail)
    (cond
      [(< count 0) tail]
      [(empty? tail) result]
      [(> count 0) (go (sub1 count) (append result (list (first tail))) (rest tail))]
      [else (append tail result)]))
  (go k empty lst))

(define (split-on sep lst)
  (define (go result buffer tail)
   (cond 
    [(empty? tail) (append result (list buffer))]
    [(equal? sep (first tail)) (go (append result (list buffer)) empty (rest tail))]
    [else (go result (append buffer (list (first tail))) (rest tail))]))
  (go empty empty lst))

(check-equal? (index-of 3 '(1 3 5 3)) 1)
(check-equal? (index-of 4 '(1 3 5)) #f)
(check-equal? (index-of 4 '()) #f)
(check-equal? (index-of #\о (word->list "топор")) 1)
(check-equal? (dedupe-adjacent '(1 1 2 2 2 1)) '(1 2 1))
(check-equal? (dedupe-adjacent '()) '())
(check-equal? (dedupe-adjacent (word->list "касса")) (word->list "каса"))
(check-equal? (merge-sorted '(1 4 6) '(2 3 7 9)) '(1 2 3 4 6 7 9))
(check-equal? (merge-sorted '() '(5)) '(5))
(check-equal? (merge-sorted '(2 2) '(2)) '(2 2 2))
(check-equal? (rotate-left '(1 2 3 4 5) 2) '(3 4 5 1 2))
(check-equal? (rotate-left '(1 2 3) 0) '(1 2 3))
(check-equal? (rotate-left '(1 2 3) 3) '(1 2 3))
(check-equal? (rotate-left '(1 2 3) -1) '(1 2 3))
(check-equal? (split-on 0 '(1 2 0 3 0 0 4)) '((1 2) (3) () (4)))
(check-equal? (split-on 0 '(0)) '(() ()))
(check-equal? (split-on 0 '()) '(()))
(check-equal? (split-on 0 '(1 2)) '((1 2)))
(check-equal? (split-on #\newline (string->list "ok\nfail\n")) '((#\o #\k) (#\f #\a #\i #\l) ()))

;; ============================================================================
;; Задача 1.2. Параметр-аккумулятор
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.

(define (digits n)
  (define (go result tail)
    (define head (quotient tail 10))
    (define last (remainder tail 10))
    (cond
      [(and (equal? head 0) (equal? last 0)) result]
      [else (go (cons last result) head)]))
  (cond
    [(equal? n 0) '(0)]
    [else (go empty n)]))

(define (from-digits lst)
  (define (go result idx tail)
    (cond
      [(empty? tail) result]
      [else (go (+ (* result 10) (first tail)) (add1 idx) (rest tail))]))
  (go 0 0 lst))

(define (longest-run lst)
  (define (go greatest last-count last-ch tail)
    (cond
      [(empty? tail) greatest]
      [(equal? last-ch (first tail)) 
        (define new-count (add1 last-count))
        (cond
          [(> new-count greatest) (go new-count new-count last-ch (rest tail))]
          [else (go greatest new-count (first tail) (rest tail))])]
      [else (go greatest 1 (first tail) (rest tail))]))
    (cond
      [(empty? lst) 0]
      [else (go 1 1 (first lst) (rest lst))]))

;; (г) Вычисление (digits 205) по подстановочной модели:
;;   (digits 205)
;;   = (go empty 205)

;;   = (go (cons (remainder 205 10) '()) (quotient 205 10)) 
;;   = (go (cons 5 '()) 20) 
;;   = (go '(5) 20)

;;   = (go (cons (remainder 20 10) '()) (quotient 20 10)) 
;;   = (go (cons 0 '(5)) 2) 
;;   = (go '(0 5) 2)

;;   = (go (cons (remainder 2 10) '()) (quotient 2 10)) 
;;   = (go (cons 2 '()) 0) 
;;   = (go '(2 0 5) 0)

;;   По первой ветке в go
;;   = '(2 0 5)

(check-equal? (digits 2026) '(2 0 2 6))
(check-equal? (digits 0) '(0))
(check-equal? (digits 7) '(7))
(check-equal? (from-digits '(2 0 2 6)) 2026)
(check-equal? (from-digits '(0)) 0)
(check-equal? (from-digits (digits 90210)) 90210)
(check-equal? (longest-run '(1 1 2 2 2 1)) 3)
(check-equal? (longest-run '()) 0)
(check-equal? (longest-run '(4 4 4)) 3)
(check-equal? (longest-run (word->list "масса")) 2)

;; ============================================================================
;; Задача 1.3. Игра в слова
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.

(define (feedback guess answer)
  (define (get-greened-and-bank bank new-guess guess-rest answer-rest)
    (cond
      [(and 
        (empty? guess-rest) 
        (empty? answer-rest)) (values new-guess bank)]
      [(equal? (first guess-rest) (first answer-rest)) 
        (get-greened-and-bank 
          bank 
          (append new-guess '(green)) 
          (rest guess-rest) 
          (rest answer-rest))]
      [else 
        (get-greened-and-bank 
          (cons (first answer-rest) bank) 
          (append new-guess (list (first guess-rest)))
          (rest guess-rest)
          (rest answer-rest))]))
  (define-values (greened-guess yellow-bank)
    (get-greened-and-bank empty empty guess answer))
  (define (mark-yellow-gray result tail bank)
      (cond
        [(empty? tail) result]
        [(equal? (first tail) 'green) 
          (mark-yellow-gray 
            (append result '(green)) 
            (rest tail) 
            bank)]
        [(member (first tail) bank) 
          (mark-yellow-gray 
            (append result '(yellow)) 
            (rest tail) 
            (remove (first tail) bank))]
        [else (mark-yellow-gray 
          (append result '(gray)) 
          (rest tail) 
          bank)]))
  (mark-yellow-gray '() greened-guess yellow-bank))

(define (consistent? word guess fb)
  (define suggested-fb (feedback guess word))
  (equal? suggested-fb fb))

(define (candidates dict guess fb)
  (filter
    (lambda (word)
      (equal? (feedback guess word) fb))
    dict))

(define (best-guess dict)
  ; Найти такой word-answer из dict, для которого предполагая, что оно загадано и
  ;   для каждого word-guess из dict получаем подсказку
  ;   из полученного fb-lst-lst, для каждого fb-lst
  ;     находим fb-lst с максимальным количеством различных списков цветов

  
  (argmax
    ;; из fb-lst-lst сделать массив длин, где длина - количество уникальных подсказок
    (lambda (dict-word)
      (define fb-lst ; Формируем список фидбеков для определенного answer для каждого guess
        (map (lambda (word-guess)
          (feedback word-guess dict-word))
        dict))

      (length 
        (filter-not
          (lambda (fb)
            (member fb (remove fb fb-lst)))
          fb-lst)))
    dict))

(check-equal? (feedback (word->list "топор") (word->list "ротор"))
              '(yellow green gray green green))
(check-equal? (feedback (word->list "робот") (word->list "робот"))
              '(green green green green green))
(check-equal? (feedback (word->list "касса") (word->list "сарай"))
              '(gray green yellow gray yellow))
(check-equal? (feedback (word->list "масса") (word->list "касса"))
              '(gray green green green green))
(check-equal? (feedback (word->list "сахар") (word->list "салат"))
              '(green green gray green gray))
(check-equal? (feedback (word->list "aaaaa") (word->list "aaaaa"))
              '(green green green green green))
(check-equal? (feedback (word->list "baaaa") (word->list "aaaab"))
              '(yellow green green green yellow))
(check-equal? (feedback (word->list "baaaa") (word->list "caaab"))
              '(yellow green green green gray))
(check-equal? (consistent? (word->list "ротор") (word->list "топор")
                           '(yellow green gray green green))
              #t)
(check-equal? (consistent? (word->list "табор") (word->list "топор")
                           '(yellow green gray green green))
              #f)
(check-equal? (candidates words (word->list "топор") '(yellow green gray green green))
              (map word->list '("ротор" "мотор")))
(check-equal? (candidates words (word->list "робот") '(gray gray gray gray gray))
              (map word->list '("касса" "масса")))
(check-equal? (best-guess (map word->list '("топор" "ротор" "мотор" "робот" "табор")))
              (word->list "ротор"))

;; ============================================================================
;; Задача 1.4. Подстановочная модель
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;;
;; (а) (my-length (merge-sorted a b)) = (+ (my-length a) (my-length b))
;;
;; Длина списка явной рекурсией (для задачи 1.4).
; (define (my-length lst)
;   (cond
;     [(empty? lst) 0]
;     [else (+ 1 (my-length (rest lst)))]))
;;
; (define (merge-sorted a b)
;  (define (go result curr lst-a lst-b)
;    (cond
;      [(empty? lst-a) (append result (cons curr lst-b))]
;      [(empty? lst-b) (append result (cons curr lst-a))]
;      [(<= curr (first lst-b)) (go (append result (list curr)) (first lst-a) (rest lst-a) lst-b)]
;      [else (go (append result (list (first lst-b))) curr lst-a (rest lst-b))]))
;  (cond
;    [(and (empty? a) (empty? b)) empty]
;    [(empty? a) (go empty (first b) a (rest b))]
;    [else (go empty (first a) (rest a) b)]))
;;
;; Доказательство:
;;   (my-length (merge-sorted a b)) = (+ (my-length a) (my-length b))
;;     
;;   1. Случай a = empty b = empty:
;;      (my-length '()) = (+ 0 0)
;;      '0 = '0
;;     
;;   2. Случай a = empty b != empty:
;;      (my-length b) = (+ 0 (my-length b))
;;      (length b) = (length b)
;;     
;;   3. Случай a != empty b = empty:
;;      Аналогично п.2
;;      (length a) = (length a)
;;     
;;   4. Случай a != empty b != empty:
;;      (my-length (go empty (first a) (rest a) b)) = (+ (my-length a) (my-length b))
;;      (my-length (go empty (first a) (rest a) b)) = (+ (my-length a) (my-length b))
;;      Для go:
;;      (my-length (go result current a b)) = (+ (my-length result) 1 (my-length lst-a) (my-length lst-b))
;;     
;;   (my-length (merge-sorted a b)) = (+ (my-length a) (my-length b))
;;
;; (б) (dedupe-adjacent (dedupe-adjacent lst)) = (dedupe-adjacent lst)
;;
;; Доказательство:
;;   ...

;; ============================================================================
;; Задача 1.5. Функции как значения
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (negate p)
  'todo)

(define (all-of ps)
  'todo)

(define (compose2 f g)
  'todo)

(define (on cmp key)
  'todo)

(define (argmax-by f lst)
  'todo)

;; (г) Почему (4) осталось перед (7), и что было бы при (on <= length):
;;   ...

; (check-equal? ((negate even?) 3) #t)
; (check-equal? ((negate even?) 4) #f)
; (check-equal? (filter (all-of (list even? positive?)) '(-2 1 4 6)) '(4 6))
; (check-equal? (filter (all-of '()) '(1 2)) '(1 2))
; (check-equal? ((compose2 add1 (lambda (x) (* 2 x))) 5) 11)
; (check-equal? (sort '((1 2 3) (4) (5 6) (7)) (on < length)) '((4) (7) (5 6) (1 2 3)))
; (check-equal? ((on string<? symbol->string) 'b 'a) #f)
; (check-equal? (argmax-by string-length '("да" "нет" "ага")) "нет")
; (check-equal? (argmax-by - '(3 1 2)) 1)

;; ============================================================================
;; Задача 1.6. Свёртки и история коммитов
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (count-if p lst)
  'todo)

(define (my-map f lst)
  'todo)

(define (repo-size commits)
  'todo)

(define (peak-size commits)
  'todo)

(define (added-by-author commits)
  'todo)

(define commits
  '(("Аня" 120 10) ("Борис" 40 60) ("Вера" 0 80) ("Аня" 5 5)))

; (check-equal? (count-if even? '(1 2 3 4 6)) 3)
; (check-equal? (count-if (lambda (w) (member #\о w)) words) 14)
; (check-equal? (my-map add1 '(1 2 3)) '(2 3 4))
; (check-equal? (my-map string-length '("ab" "" "abc")) '(2 0 3))
; (check-equal? (repo-size commits) 10)
; (check-equal? (repo-size '()) 0)
; (check-equal? (peak-size commits) 110)
; (check-equal? (peak-size '()) 0)
; (check-equal? (added-by-author commits) '(("Аня" . 125) ("Борис" . 40) ("Вера" . 0)))

;; ============================================================================
;; Задача 1.7. Конвейеры и сборки CI
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в <части> этой задачи в соответствии с правилами курса и условием.

(define (passed builds)
  'todo)

(define (branches-of builds)
  'todo)

(define (success-table builds)
  'todo)

(define (stable-branches builds)
  'todo)

(define (slowest builds)
  'todo)

(define builds
  '(("main" ok 210) ("main" fail 190) ("feature-login" ok 320)
    ("main" ok 205) ("feature-login" fail 300) ("hotfix" ok 95)))

; (check-equal? (passed builds) 4)
; (check-equal? (branches-of builds) '("main" "feature-login" "hotfix"))
; (check-equal? (success-table builds) '(("hotfix" . 1) ("main" . 2/3) ("feature-login" . 1/2)))
; (check-equal? (stable-branches builds) '("hotfix"))
; (check-equal? (slowest builds) '("feature-login" ok 320))

;; ============================================================================
;; Задача 1.8. Проверка кода от ИИ
;; ============================================================================
;; Я не использовал(а) ИИ при решении этой задачи.
;; Я использовал(а) ИИ (<модель>) в пункте (а) этой задачи в соответствии с правилами курса и условием.
;;
;; (а) Функция, написанная ИИ (скопируйте её сюда без изменений; полный диалог
;;     --- в transcript.md). Назовите её feedback-ai, чтобы не перекрывать
;;     вашу feedback из задачи 1.3.

(define (feedback-ai guess answer)
  'todo)

;; (б) Четыре проверки, из них хотя бы две с повторяющимися буквами.
;;     Для каждой напишите в комментарии ожидаемый ответ, а проверку
;;     оформите как check-equal? с ожидаемым значением (не с вашей feedback:
;;     сверяться нужно с правилом из условия, а не с другой программой).
;;
;; Проверка 1: попытка ..., ответ ..., ожидаю ...
;; Проверка 2: ...
;; Проверка 3: ...
;; Проверка 4: ...

;; (в) Наименьший контрпример и исправление, либо объяснение, почему
;;     правило повторов соблюдено (с указанием строк).
;;
;; ...
