(ql:quickload :cl-debug-print)
(cl-debug-print:use-debug-print)
(ql:quickload :alexandria)
(ql:quickload :assoc-utils)

(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car #>lst))
          (our-remove-if fn (cdr lst))
          #>(cons (car lst) (our-remove-if fn (cdr lst))))))

(our-remove-if #'evenp '(1 2 3 4 5 6 7))

;;; シンボルの属性リストに関数を突っ込む

#+(or)
(defun behave (animal)
  (case animal
    (dog (wag-tail)
     (bark))
    (rat (scurry)
     (squeak))
    (cat (rub-legs)
     (scratch-carpet))))

(defun behave (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
          (print "bark")))

(behave 'dog)
;; "bark"

(setf (get 'dog 'height) 30)

(inspect 'dog)

#|
The object is a SYMBOL.
0. Name: "DOG"
1. Package: #<PACKAGE "COMMON-LISP-USER">
2. Value: #<unbound slot>
3. Function: #<unbound slot>
4. Plist: (HEIGHT 30 BEHAVIOR #<FUNCTION (LAMBDA ()) {54CEF72B}>)
> 4

The object is a proper list of length 4.
0. 0: HEIGHT
1. 1: 30
2. 2: BEHAVIOR
3. 3: #<FUNCTION (LAMBDA ()) {54CEF72B}>
|#

;; 連想リストでやるとしたら？微妙にめんどくさいかも

(defparameter *animals* '())

(push `(dog . ((behavior . ,#'(lambda ()
                                (print "bark")))
               (height . 10)))
      *animals*)

(defun behave-assoc (animal)
  (funcall (assoc-utils:aget
            (assoc-utils:aget *animals* animal)
            'behavior)))

(behave-assoc 'dog)

;;; レキシカルスコープ

(let ((y 7))
  (defun scope-test (x)
    (list x y)))

(scope-test 10)
;; => (10 7)

(let ((y 5))
  (scope-test 3))
;; => (3 7)  レキシカルスコープ
;; => (3 5)  ダイナミックスコープ

;; ダイナミックスコープの例
(defvar *y*)

(defun scope-test2 (x)
  (list x *y*))

(let ((*y* 5))
  (scope-test 3))
;; => (3 5)

;; *y*の値は外から変えられる

;;; クロージャ

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

(list+ '(1 2 3) 10)

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(new-id)
(reset-id)

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
          (setq n x)
          (+ x n))))

(let ((add (make-adderb 10)))
  #>(funcall add 20) ; => 30
  (funcall add 20 t)
  #>(funcall add 20)) ; => 40

(defun make-dbms (db)
  (list
   ;; referrer
   #'(lambda (key)
       (cdr (assoc key db)))
   ;; setter
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   ;; cleaner
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(defparameter cities (make-dbms '((boston . us) (paris . france))))
;; referrer
(funcall (first cities) 'boston) ; => US
(funcall (first cities) 'paris) ; => FRANCE

;; setter
(funcall (second cities) 'london 'england)

(funcall (first cities) 'london) ; => ENGLAND

;; cleaner
(funcall (third cities) 'london)
(funcall (first cities) 'london) ; => NIL

;;; ローカル関数

(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))

;; リストの各要素リスト内のaの数を数える
(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))

;; Schemeのように内部関数でもいいのでは？ => defunはトップレベルに定義されてしまう
(defun count-instances (obj lsts)
  (defun instances-in (lst)
    (if (consp lst)
        (+ (if (eq (car lst) obj) 1 0)
           (instances-in (cdr lst)))
        0))
  (mapcar #'instances-in lsts))

(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
(instances-in '(a b c)) ; => 1

;; count-instancesを呼ぶ度にinstances-inが上書かれる
(count-instances 'b '((a b c) (d a r p a) (d a r) (a a)))
(instances-in '(d a r p a)) ; => 0

;;; 末尾再帰

(defun our-length (lst)
  (if (null #>lst)
      0
      #>(1+ (our-length (cdr lst)))))

(our-length '(1 2 3))

(defun our-length-tco (lst)
  (labels ((rec (lst acc)
             #>lst #>acc
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(our-length-tco '(1 2 3))

;; 末尾再帰でないバージョンで100万件のリストの長さをカウントしようとするとstackoverflowになる
#+(or)
(let ((huge-list (loop for i from 1 to 1000000 collect i)))
  (our-length huge-list))

;; Control stack exhausted (no more space for function call frames).
;; This is probably due to heavily nested or infinitely recursive function
;; calls, or a tail call that SBCL cannot or has not optimized away.

;; PROCEED WITH CAUTION.
;;    [Condition of type SB-KERNEL::CONTROL-STACK-EXHAUSTED]

;;; disassemble

;; CL-USER> (disassemble 'our-length)
; disassembly for OUR-LENGTH
; Size: 83 bytes. Origin: #x54CE23E3                          ; OUR-LENGTH
; 3E3:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 3E7:       488945F8         MOV [RBP-8], RAX
; 3EB:       4881FE17010050   CMP RSI, #x50000117             ; NIL
; 3F2:       7505             JNE L1
; 3F4:       31D2             XOR EDX, EDX
; 3F6: L0:   C9               LEAVE
; 3F7:       F8               CLC
; 3F8:       C3               RET
; 3F9: L1:   8D46F9           LEA EAX, [RSI-7]
; 3FC:       A80F             TEST AL, 15
; 3FE:       7531             JNE L2
; 400:       488B5601         MOV RDX, [RSI+1]
; 404:       4883EC10         SUB RSP, 16
; 408:       B902000000       MOV ECX, 2
; 40D:       48892C24         MOV [RSP], RBP
; 411:       488BEC           MOV RBP, RSP
; 414:       E8C9DA65FB       CALL #x5033FEE2                 ; #<FDEFN OUR-LENGTH> 再帰呼び出し
; 419:       480F42E3         CMOVB RSP, RBX
; 41D:       488B75F0         MOV RSI, [RBP-16]
; 421:       BF02000000       MOV EDI, 2
; 426:       E8E5EA11FF       CALL #x53E00F10                 ; SB-VM::GENERIC-+
; 42B:       488B75F0         MOV RSI, [RBP-16]
; 42F:       EBC5             JMP L0
; 431: L2:   CC53             INT3 83                         ; OBJECT-NOT-LIST-ERROR
; 433:       18               BYTE #X18                       ; RSI(d)
; 434:       CC10             INT3 16                         ; Invalid argument count trap

;; CL-USER> (disassemble 'our-length-tco)
; disassembly for OUR-LENGTH-TCO
; Size: 84 bytes. Origin: #x54CE24B3                          ; OUR-LENGTH-TCO
; 4B3:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 4B7:       488945F8         MOV [RBP-8], RAX
; 4BB:       488B75F0         MOV RSI, [RBP-16]
; 4BF:       4531C0           XOR R8D, R8D
; 4C2:       EB2F             JMP L1
; 4C4:       660F1F440000     NOP
; 4CA:       660F1F440000     NOP
; 4D0: L0:   8D46F9           LEA EAX, [RSI-7]
; 4D3:       A80F             TEST AL, 15
; 4D5:       752B             JNE L2
; 4D7:       488B7601         MOV RSI, [RSI+1]
; 4DB:       488975E8         MOV [RBP-24], RSI
; 4DF:       BF02000000       MOV EDI, 2
; 4E4:       498BD0           MOV RDX, R8
; 4E7:       E824EA11FF       CALL #x53E00F10                 ; SB-VM::GENERIC-+
; 4EC:       4C8BC2           MOV R8, RDX
; 4EF:       488B75E8         MOV RSI, [RBP-24]
; 4F3: L1:   4881FE17010050   CMP RSI, #x50000117             ; NIL
; 4FA:       75D4             JNE L0                          ; 再帰呼び出しがなく、L0へのジャンプ(ループになっている)
; 4FC:       498BD0           MOV RDX, R8
; 4FF:       C9               LEAVE
; 500:       F8               CLC
; 501:       C3               RET
; 502: L2:   CC53             INT3 83                         ; OBJECT-NOT-LIST-ERROR
; 504:       18               BYTE #X18                       ; RSI(d)
; 505:       CC10             INT3 16                         ; Invalid argument count trap


;;; 木構造再帰の例: fib
;;; https://sicp.iijlab.net/fulltext/x122.html

(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(time (fib 40))

;; Evaluation took:
;;   1.555 seconds of real time
;;   1.556050 seconds of total run time (1.552375 user, 0.003675 system)
;;   100.06% CPU
;;   5,913,056,804 processor cycles
;;   0 bytes consed

;; メモ化: 関数の返値をキャッシュする
;; 副作用のない純粋関数だからできる所業
;; => 5章で自前実装する
(ql:quickload :fare-memoization)
(fare-memoization:memoize 'fib)

(time (fib 40))

(fare-memoization:unmemoize 'fib)

(time (fib 40))

;; 末尾再帰版fib

(defun fib (n)
  (labels ((fib-inner (n cnt pre1 pre2)
             (declare (optimize speed)
                      (type fixnum n cnt pre1 pre2))
             ;; (format t "n: ~A, cnt: ~A, pre1: ~A, pre2: ~A~%" n cnt pre1 pre2)
             (if (> n cnt)
                 (fib-inner n (1+ cnt) (+ pre1 pre2) pre1)
                 pre2)))
    (fib-inner n 0 1 0)))

(time (fib 40))

;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000001 seconds of total run time (0.000001 user, 0.000000 system)
;;   100.00% CPU
;;   1,482 processor cycles
;;   0 bytes consed

;; 400万倍の高速化

(defun triangle (n)
  (labels ((tri (c n)
             (declare (optimize speed)
                      (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

(defun triangle (n)
  (labels ((tri (c n)
             (declare (optimize speed)
                      (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (+ n c)
                      (- n 1)))))
    (tri 0 n)))

(time (triangle 1000000))

(defun triangle-raw (n)
  (labels ((tri (c n)
             (if (zerop n)
                 c
                 (tri (+ n c)
                      (- n 1)))))
    (tri 0 n)))

(time (triangle-raw 1000000))

;; 再帰はループ+スタック
(defun quicksort (list)
  (if (null list)
      nil
      (let ((pivot (first list))
            (rest (rest list)))
        (append (quicksort (remove-if-not (lambda (x) (<= x pivot)) rest))
                (list pivot)
                (quicksort (remove-if-not (lambda (x) (> x pivot)) rest))))))

;; 使用例
(quicksort '(6 2 7 3 1 5))

;;; コンパイル
(compiled-function-p #'triangle) ; => T

;; clispなどだと明示的にコンパイルする必要がある
(compile 'triangle)

;; コンパイルできない例として挙がってるけど実際にはコンパイルできていそう
(let ((y 2))
  (defun foo (x) (+ x y)))

(compiled-function-p #'foo) ; => T

(foo 10)

;; クロージャを生成する関数がコンパイルされていると、生成された関数もコンパイルされている
(compiled-function-p #'make-adder) ; => T

(let ((add10 (make-adder 10)))
  #>(compiled-function-p add10) ; => T
  (funcall add10 3))
