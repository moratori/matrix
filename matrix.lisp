
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
 (flag       nil))


(defconstructer matrix 
   (with-accessors ((matrix-lst matrix-of)) self
     (let ((row    (length matrix-lst))
           (column (length (car matrix-lst))))
       (unless (flag-of self)
         (unless 
            (every 
             (lambda (x) 
              (= (length x) column)) matrix-lst) 
            (error "invalid matrix"))) 
       (setf 
          (row-of self)    row
          (column-of self) column))))


;;; matrix クラスのslot matrix から配列を作ってセット
(defun set-array (matrix-obj)
  (setf 
    (matrix-arr-of matrix-obj)
    (make-array 
      (list (row-of matrix-obj) (column-of matrix-obj))
      :initial-contents (matrix-of matrix-obj))))


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
  (format t "~A by ~A matrix~%" 
          (row-of x) (column-of x)))


$(defgeneric elmt (matrix-obj row column)
  (:documentation "return element"))

;;; 前半で配列がなかったら配列を作る
(defmethod elmt ((matrix matrix) row col)
  (let ((matrix-arr (matrix-arr-of matrix)))
     (if (null matrix-arr)
        (aref (set-array matrix) row col) 
        (aref matrix-arr row col))))


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



$(defgeneric addablep (matrix-obj matrix-obj)
  (:documentation "addable x y"))

(defmethod addablep ((x matrix) (y matrix))
  (and 
    (= (row-of x) (row-of y)) 
    (= (column-of y) (column-of y))))


$(defgeneric add (x y) 
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



(defvar *pr* (loop for x from 0 to 10000 collect x))
(defvar *lst* (loop repeat 1000 collect *pr*))


(add (new matrix :matrix *lst*) (new matrix :matrix *lst*))
