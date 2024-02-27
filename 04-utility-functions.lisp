;;; 4.1 ユーティリティの誕生
;; 前章の関数的インターフェースが守られていない例でもある
(defun all-nicknames (names)
  (if (null names)
      nil
      ;; nicknamesが新規にリストを作り出す前提でnconcを使っている？
      (nconc (nicknames (car names))
             (all-nicknames (cdr names)))))

;; dangerous
(defun nicknames (sym)
  (get sym 'nicknames))

;; safe
(defun nicknames (sym)
  (copy-list (get sym 'nicknames)))

(setf (get 'elizabeth 'nicknames)  '(bess betsy betty liz)
      (get 'alessandro 'nicknames) '(alex al andy))

(all-nicknames '(elizabeth alessandro))
(all-nicknames '(elizabeth alessandro)) ; => 2回目が無限ループになる。何故か？

;; 1回目終了時点でalessandroのニックネームが破壊的に連結されているので、
;; 2回目でalessandroのニックネームをさらに破壊的に連結しようとすると(ALEX AL ANDY)の実体が同一オブジェクトであるため無限ループになる
(get 'elizabeth 'nicknames) ; => (BESS BETSY BETTY LIZ ALEX AL ANDY)
(get 'alessandro 'nicknames) ; => (ALEX AL ANDY)

(eq (fifth (get 'elizabeth 'nicknames))
    (first (get 'alessandro 'nicknames)))
;; => T

;;; mapcan

(documentation 'mapcan 'function)

"Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
Return NCONC of FUNCTION return values."

(mapcan #'nicknames '(elizabeth alessandro))
(mapcan #'nicknames '(elizabeth alessandro)) ; => これも2回目が無限ループになる

;; mappendなら安全

(ql:quickload :alexandria)
(alexandria:mappend #'nicknames '(elizabeth alessandro))

;; 自家版mappend
(defun mappend (fn list-of-list)
  (apply #'append (mapcar fn list-of-list)))

;; find-books

(setf (get 'adachi 'bookshops)    '()
      (get 'shinjuku 'bookshops)  '(kinokuniya junku-do)
      (get 'minato 'bookshops)    '(kumazawa))

(defun bookshops (town)
  (get town 'bookshops))

(setf towns '(adachi shinjuku minato))

(let ((town (find-if #'bookshops towns)))
  (values town (bookshops town))) ; => bookshopsを1回余計に適用している

;; 特定のユーケースの具体化版
(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
            (values (car towns) shops)
            (find-books (cdr towns))))))

(find-books towns)

;; 汎用化されたユーティリティ関数
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(find2 #'bookshops towns)

;;; 4.3 リストに対する操作

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;; インライン展開の効果

(defun last1-user (lst)
  (last1 lst))

(defun last1-not-inline (lst)
  (car (last lst)))

(defun last1-not-inline-user (lst)
  (last1-not-inline lst))

;; disassembleすると関数呼び出しが無くなっている
(disassemble 'last1-user)
(disassemble 'last1-not-inline-user)

(time
 (let ((lst '(1)))
   (loop repeat 1000000000 do (last1-user lst))))

(time
 (let ((lst '(1)))
   (loop repeat 1000000000 do (last1-not-inline-user lst))))

;; mklistのユースケース

(defun lookup (x)
  (identity x))

(setf data '((a b)
             c
             (d e f)))

(mapcan #'(lambda (d) (mklist (lookup d)))
        data)
;; => (A B C D E F)

;;; longer

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(longer '(a b c) '(1 2))
(longer "abc" '(1 2))
(longer "abc" "12")

(defun slow-longer (x y)
  (> (length x) (length y)))

(length lst)
(time (longer lst '(1)))
(time (slow-longer lst '(1)))

;;; filter
;; remove-if-not + 変換を噛ませる

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc))) ; safe

(filter #'(lambda (x) (if (numberp x) (1+ x)))
        '(a 1 2 b 3 c d 4))

;; 以下でも意味的には一緒だが、一周半しているため処理が余計にある
(mapcar #'1+
        (remove-if-not #'numberp
                       '(a 1 2 b 3 c d 4)))

;; 末尾再帰版
(defun filter-tco (fn lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (let  ((val (funcall fn (car lst))))
                   (if val
                       (rec (cdr lst) (cons val acc))
                       (rec (cdr lst) acc))))))
    (nreverse (rec lst '()))))

(filter-tco #'(lambda (x) (if (numberp x) (1+ x)))
            '(a 1 2 b 3 c d 4))

;;; group

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(group '(a b c d e f g) 2)
;; (group '(a b c d e f g) 0) ; => error

;; split
;; nthcdrとsubseqを一度にできるので効率も増す?
;; => チューニングしてやっと同等
(defun split (lst n)
  (declare (type list lst)
           (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (labels ((rec (i first-part rest-part)
             (declare (type fixnum i)
                      (type list first-part rest-part))
             (if (or (null rest-part) (= i n))
                 (values (nreverse first-part) rest-part)
                 (rec (1+ i)
                      (cons (car rest-part) first-part)
                      (cdr rest-part)))))
    (rec 0 '() lst)))

(defun split (lst n)
  (values (subseq lst 0 n)
          (nthcdr n lst)))

(split '(a b c d e f g) 2)

(time (car (split lst 1000000)))
(time (car (split lst 1000000)))

(time (car (nthcdr 1000000 lst)))
(time (car (subseq lst 0 1000000)))

(defun group (source n)
  (labels ((rec (source acc)
             (multiple-value-bind (first-part rest-part)
                 (split source n)
               (if (consp rest-part)
                   (rec rest-part (cons first-part acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(time (progn (group lst 100000) 'done))

;;; 入れ子になったリスト内へ下っていく関数の例

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

(flatten '(a (b c) ((d e) f)))

(defun flatten (x)
  (cond ((null x) x)
        ((atom (car x)) (cons (car x) (flatten (cdr x))))
        (t (append (flatten (car x))
                   (flatten (cdr x))))))

(flatten '(a (b c) ((d e) f)))

(defun flatten (lst)
  (mapcan (lambda (elem)
            (if (atom elem)
                (cons elem nil)
                (flatten elem)))
          lst))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ;; 木を辿る
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   ;; 葉ノード
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;; remove-ifのtree版
(prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))

;;; 4.4 検索

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(before 'a 'b '(a b c d)) ; => (A B C D)
(before 'b 'a '(a b c d)) ; => nil
(before 'a 'b '(a e c d)) ; => (A E C D)

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(after 'b 'a '(a e c d)) ; => nil

;; memberの性質をうまく使った例
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

(duplicate 'a '(a b c a d)) ; => (A D)

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src))) ; 局所変数, 初期値, 更新式
        ((or (null src) (funcall fn (car src))) ; 終了判定
         (values (nreverse acc) src)) ; 返値
      (push (car src) acc))))

(split-if #'(lambda (x) (> x 4))
          '(1 2 3 4 5 6 7 8 9 10))

(split-if #'(lambda (x) (string> x "first"))
          '("after" "before" "current" "rest"))

(defun split-if (fn lst)
  (labels ((rec (src acc)
             #>src #>acc
             (if (or (null src) (funcall fn (car src)))
                 (values (nreverse acc) src)
                 (rec (cdr src)
                      (cons (car src) acc)))))
    (rec lst nil)))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(most #'length '((a b) (a b c) (a) (e f g)))

(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(best #'> '(1 2 3 4 5))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(mostn #'length '((a b) (a b c) (a) (e f g)))

;; ((A B C) (E F G)) ; スコアが同点のものも返す
;;3

;;; 4.5 マッピング



;;; 4.6 入出力

;;; 4.8 密度
;; ユーティリティ関数が沢山あると読むのが大変になるのでは？
;; => 本文: ユーティリティ関数を読む手間は増えるが、ユーティリティ関数を使わないよりも総合的な労力は減る
;; => 感想: 実際には、ユーティリティ関数が何をやっているかを知ればよく、実装の詳細を知る必要はない
;; 例外: 小さなパッケージを配布するときは依存を少なくする方が優先される
