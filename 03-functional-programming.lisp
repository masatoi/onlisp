(ql:quickload :cl-debug-print)
(cl-debug-print:use-debug-print)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)
(ql:quickload :parse-float)
(ql:quickload :clgplot)
(ql:quickload :local-time)

;; 悪いポイント: 値を返さない。副作用のみを目的にしている
(defun bad-reverse (lst)
  (declare (optimize speed))
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(setf lst '(a b c))
(bad-reverse lst)

lst ; => (C B A)

(rotatef (car lst) (caddr lst)) ; => nil

lst ; => (A B C)

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (declare (optimize speed))
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;; そして短いだけでなくgood-reverseは効率的でもある： O(n^2)ではなくO(n)なのだ． 
;; => nthが連結リストを辿る時間(sbclの場合、リストは単純な連結リストではなくO(1)でアクセスできていそう)

;; 確実にランダムアクセスがO(1)の固定長配列でやったらどうなる？

(defun reverse-array! (a)
  (declare (optimize speed)
           (type (simple-array fixnum) a))
  (let* ((len (length a))
         (ilimit (truncate (/ len 2)))
         (tmp 0))
    (declare (type fixnum len ilimit tmp))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      ;; swap
      (setf tmp (aref a i)
            (aref a i) (aref a j)
            (aref a j) tmp))
    a))

(setf arr (make-array 3 :element-type 'fixnum :initial-contents '(1 2 3)))
(reverse-array! arr)

(setf arr (make-array 1000000 :element-type 'fixnum))
(time (progn (reverse-array! arr) 'done))

(let ((result (loop for m from 0 to 1000000 by 100000
                    collect (let ((huge-array (make-array m :element-type 'fixnum)))
                              (sb-ext:gc :full t)
                              (timeit (reverse-array! huge-array))))))
  (clgp:plot result
             :x-label "length of list"
             :y-label "duration"
             :main "reverse-array!"))
;; => O(n)

;;; 時間計測

(loop for m from 1 to 30 do
  (let ((huge-list (loop for i from 1 to (* 1000 m) collect i)))
    (time (progn
            (bad-reverse huge-list)
            'done))))

;; timeマクロの結果をパース
(defmacro timeit (&rest body)
  `(parse-float:parse-float
    (elt (nth-value 1 (cl-ppcre:scan-to-strings "\\b(\\d+\\.\\d+) seconds of total run time"
                                                (with-output-to-string (*trace-output*)
                                                  (time ,@body))))
         0)))

;; Common Lispの標準機能のみで実現
(defmacro timeit (&rest body)
  (let ((start-time (gensym))
        (end-time (gensym)))
    `(let ((,start-time (get-internal-real-time)))
       ,@body
       (let ((,end-time (get-internal-real-time)))
         (coerce (/ (- ,end-time ,start-time)
                    internal-time-units-per-second)
                 'single-float)))))

;; マクロを使わずcall-withスタイルで
(defun call-with-time (fn)
  (let ((start-time (get-internal-real-time)))
    (funcall fn)
    (let ((end-time (get-internal-real-time)))
      (coerce (/ (- end-time start-time)
                 internal-time-units-per-second)
              'single-float))))

(call-with-time
 (lambda () (sleep 1)))

;; nanosecまで測れるバージョン
(defmacro timeit (&rest body)
  (let ((start (gensym)))
    `(let ((,start (local-time:now)))
       ,@body
       (local-time:timestamp-difference (local-time:now) ,start))))

;; プロット
(let* ((max-len 100000)
       (step 10000)
       (len-list (loop for l from 0 to max-len by step collect l))
       (huge-list (loop for i from 0 to max-len collect i))
       (result (loop for l in len-list
                     collect (let ((huge-sublist (subseq huge-list 0 l)))
                               (cons #>(timeit (bad-reverse huge-sublist))
                                     (timeit (good-reverse huge-sublist))))))
       (bad-reverse-duration (mapcar #'car result))
       (good-reverse-duration (mapcar #'cdr result)))
  (clgp:plots (list bad-reverse-duration
                    good-reverse-duration)
              :x-seqs (list len-list len-list)
              :x-label "length of list"
              :y-label "duration"
              :y-logscale t
              :main "bad-reverse vs good-reverse"
              :title-list '("bad-reverse" "good-reverse"))
  (clgp:plot bad-reverse-duration
             :x-seq len-list
             :x-label "length of list"
             :y-label "duration"
             :main "bad-reverse")
  (clgp:plot good-reverse-duration
             :x-seq len-list
             :x-label "length of list"
             :y-label "duration"
             :main "good-reverse"))

;; GC時間で直線にならないのでgood-reverseの前で明示的にfull GC
(let ((result (loop for m from 0 to 1000000 by 10000
                    collect (let ((huge-list (loop for i from 0 to m collect i)))
                              (sb-ext:gc :full t)
                              (timeit (good-reverse huge-list))))))
  (clgp:plot result
             :x-label "length of list"
             :y-label "duration"
             :main "good-reverse"))

(progn
  (setq lst (loop for i from 1 to 1000000 collect i))
  'done)

(time
 (progn
   (good-reverse lst)
   'done))

;; 1万件に対して
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000051 seconds of total run time (0.000051 user, 0.000000 system)
;;   100.00% CPU
;;   193,306 processor cycles
;;   163,760 bytes consed

(time
 (progn
   (bad-reverse lst)
   'quit))

;; 1万件に対して
;; Evaluation took:
;;   0.084 seconds of real time
;;   0.082493 seconds of total run time (0.082493 user, 0.000000 system)
;;   97.62% CPU
;;   313,466,788 processor cycles
;;   0 bytes consed

(defun sum-tail (n &key (acc 0))
  (if (zerop n)
      acc
      (sum-tail (- n 1) :acc (+ n acc))))

(sum-tail 1000000)

; disassembly for SUM-TAIL
; Size: 94 bytes. Origin: #x54AEB38E                          ; SUM-TAIL
; 8E:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 92:       488945F8         MOV [RBP-8], RAX
; 96:       488B55F0         MOV RDX, [RBP-16]
; 9A:       31FF             XOR EDI, EDI
; 9C:       E89F5D31FF       CALL #x53E01140                  ; SB-VM::GENERIC-=
; A1:       7507             JNE L0
; A3:       488B55E8         MOV RDX, [RBP-24]
; A7:       C9               LEAVE
; A8:       F8               CLC
; A9:       C3               RET
; AA: L0:   488B55F0         MOV RDX, [RBP-16]
; AE:       BF02000000       MOV EDI, 2
; B3:       E8C85B31FF       CALL #x53E00F80                  ; SB-VM::GENERIC--
; B8:       488BC2           MOV RAX, RDX
; BB:       488945E0         MOV [RBP-32], RAX
; BF:       488B55F0         MOV RDX, [RBP-16]
; C3:       488B7DE8         MOV RDI, [RBP-24]
; C7:       E8445B31FF       CALL #x53E00F10                  ; SB-VM::GENERIC-+
; CC:       488BF2           MOV RSI, RDX
; CF:       488B45E0         MOV RAX, [RBP-32]
; D3:       488BD0           MOV RDX, RAX
; D6:       488B3DBBFEFFFF   MOV RDI, [RIP-325]               ; :ACC
; DD:       B906000000       MOV ECX, 6
; E2:       FF7508           PUSH QWORD PTR [RBP+8]
; E5:       E9986B84FB       JMP #x50331F82                   ; #<FDEFN SUM-TAIL>
; EA:       CC10             INT3 16                          ; Invalid argument count trap

;; JMPがあるので末尾再帰最適化されていそう？

;;; 破壊的バージョンがある関数でも結果のみを目的にすること

;;; 命令型プログラミングの裏返し

(defun fun (x)
  (list 'a (expt (car x) 2)))

(defun imp (x)
  (let (y sqr) ; 初期化していない(値をバインドしていない) => 副作用を目的にしている
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

;; 結局、途中経過を局所変数を介在させて代入しているかどうかの違いでしかない気がする
;; このimp関数自体は外部から見たときには副作用を持たない

;;; 命令型で書かれた関数を関数型にする手順
;;;  命令型で最後に起きること(list関数の適用)が関数型でいうと最初に来る
