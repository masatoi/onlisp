#|
以下のようにして実行するとスタックオーバーフローエラーになる
ros run -l 03-1-stackoverflow.lisp 

lstをdefparameterでスペシャル変数にしてしまっているのでgood-reverse内で捕捉してしまっているのが原因
サボらずに *lst* のようにしよう。
style-warningを避けたいだけの場合、setを使う?
|#

(defparameter lst '(a b c))

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (declare (optimize speed))
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

(progn
  (setf lst (loop for i from 1 to 1000000 collect i))
  'done)

(time
 (progn
   (good-reverse lst)
   'done))

(quit)
