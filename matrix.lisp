
(ql:quickload :macro.util.my)

(defpackage matrix
  (:use :cl :macro.util.my)
  (:export :#\{ :#\}))
(in-package :matrix)


(declaim 
  (optimize 
    (speed 0) 
    (debug 3) 
    (safety 3)))

(set-macro-character #\} 
 (get-macro-character #\)))

(set-macro-character #\{ 
 (lambda (stream char)
   (declare (ignore char))
   `(new matrix :matrix (quote ,(read-delimited-list #\} stream t)))))
 


(defconstant +COLUMN-SPACE+ 5)

$(dfclass matrix nil
 (matrix     (error "List expressing the matrix is required"))
 (matrix-arr nil)
 (row        nil)
 (column     nil)
 (issquire   nil)
 (check      nil))


;;; ここで配列を作ってしまうと
;;; でかい行列が来た時遅くなる
;;; ランダムアクセスの無い操作の方が多くなるなら
;;; それが行われるまで配列のオブジェクトは作らないで
;;; 高速化すべきではあるけど、どっちが多くなるかはわからないので
;;; 作ることにしたけどやめるべきかも. 間をとって遅延評価にした
;;; 実際に配列をつかうメソッドが呼ばれたらそこでforceする
(defconstructer matrix 
   (with-accessors ((matrix-lst matrix-of)) self
     (let ((row    (length matrix-lst))
           (column (length (car matrix-lst))))
       (unless (check-of self)
         (unless 
            (every 
             (lambda (x) 
              (= (length x) column)) matrix-lst) 
            (error "invalid matrix"))) 
       (setf 
          (row-of self)        row
          (column-of self)     column
          (issquire-of self)   (= row column)
          (matrix-arr-of self) (lazy (make-array (list row column) :initial-contents matrix-lst))))))

;;; matrix オブジェクトを作る
(defun mat (lst) (new matrix :matrix lst))

;;; row column を配列のindexに変換するだけ
(defun transform (index) (1- index))


$(defgeneric show (matrix-obj)
  (:documentation "print matrix"))

(defmethod show ((matrix matrix))
  (let* ((matrix-lst (matrix-of matrix))
         (pr (format nil "~~~A:@<~~a~~>" +COLUMN-SPACE+)) 
         (template 
           (concatenate 'string "~{|"
            (with-output-to-string (*standard-output*) 
              (loop repeat (column-of matrix) do (princ pr))) "|~%~}")))
        (loop for each in matrix-lst do (format t template each))) matrix)


$(defgeneric formt (matrix-obj)
    (:documentation "show format"))

(defmethod formt ((x matrix))
  (format t "~A by ~A ~A matrix~%" 
          (row-of x) (column-of x) (if (issquire-of x) "square" "")))


$(defgeneric elmt (matrix-obj row column)
  (:documentation "return element"))

(defmethod elmt ((matrix matrix) row col)
  (aref (force (matrix-arr-of matrix)) 
        (transform row) 
        (transform col)))


$(defgeneric trans (matrix-obj)
  (:documentation "return transpose matrix"))

(defmethod trans ((matrix matrix))
  (new matrix :matrix 
     (reverse 
        (labels 
          ((main (lsts acc)
             (if (null (car lsts)) acc 
                (main 
                    (mapcar #'cdr lsts) 
                    (cons (mapcar #'car lsts) acc)))))
           (main (matrix-of matrix) nil)))))


$(defgeneric row (matrix-obj row)
    (:documentation "return row vector"))


;;; 列が相当でかいなら、配列は遅くなるし
;;; 行がでかいならcdrダウンには時間がかかるし
;;; row <= column ならリストを下った方が速いな
;;; ただ、row > column　で配列使う場合でも
;;; force する時間も考慮するとどちらか一方に統一すべき?
;;; ただどこで初めてforceされるかという問題もある
(defmethod row ((matrix matrix) num)
  (let1 index (transform num)
   (if (<= (row-of matrix) (column-of matrix))
     (nth index (matrix-of matrix))
     (let1 arr (force (matrix-arr-of matrix)) 
       (loop for col from 0 upto 
             (transform (column-of matrix))
             collect (aref arr index col))))))


(defgeneric col (matrix-obj col)
  (:documentation "return column vector"))


(defmethod col ((matrix matrix) num)
  (let  ((arr  (force (matrix-arr-of matrix))) 
         (c (transform num)))
        (loop for r from 0 
              upto (transform (row-of matrix)) 
              collect (aref arr r c))))


$(defgeneric addablep (matrix-obj matrix-obj)
  (:documentation "addable x y"))

(defmethod addablep ((x matrix) (y matrix))
  (and 
    (= (row-of x) (row-of y)) 
    (= (column-of y) (column-of y))))


$(defgeneric add (matrix-obj matrix-obj) 
  (:documentation "add matrix"))

(defmethod add ((x matrix) (y matrix))
  (if (addablep x y)
    (let ((lst1 (matrix-of x)) (lst2 (matrix-of y)))
      (new matrix :matrix 
        (maplist 
          (lambda (x y)
            (mapcar 
              (lambda (x y) (+ x y)) 
               (car x) (car y))) lst1 lst2)))
    (error "same type matrix required")))



$(defgeneric multipliablep (matrix-obj matrix-obj)
   (:documentation "multipliablep matrix"))

(defmethod multipliablep ((x matrix) (y matrix))
  (= (column-of x) (row-of y)))


$(defgeneric mult (matrix-obj matrix-obj)
    (:documentation "multiplication matrix"))

(defmethod mult ((x matrix) (y matrix))
  (if (multipliablep x y)
    (new matrix :matrix 
         (let ((left (matrix-of x))
          (col-cache 
            (memoize 
              (lambda (ins colnum) (col ins colnum)))))
          (mapcar 
            (lambda (each-row)
              (loop for c from 1 upto (column-of y) collect
                  (apply #'+ (mapcar 
                    (lambda (x y)
                      (* x y)) each-row (funcall col-cache y c))))) left)))
    (error "invalid matrix type")))




(defvar *pr* (loop for x from 0 to 999 collect x))
(defvar *lst* (loop repeat 1000 collect *pr*))

(time (add (new matrix :matrix *lst*) (new matrix :matrix *lst*)))
(show (add { (1 2 3) (4 5 6) (7 8 9) } { (0 -1 3) (6 7 -3) (2 -7 6) }))
(time (row (new matrix :matrix *lst*) 1))

(defvar *mat* (mat *lst*))

(time (mult *mat* *mat*)) ;; 18sec
