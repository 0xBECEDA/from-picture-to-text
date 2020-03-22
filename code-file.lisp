(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun make-pix (x y)
  (cons x y))

(defun put-pix (pix)
  (enqueue pix *queue-of-pixels*))

(defun get-x (pix)
  (car pix))

(defun get-y (pix)
  (cdr pix))

(defun get-pix (queue)
  (dequeue queue))

(defun empty-queue? (queue)
  (and (null (car queue))
       (null (cdr queue))))

;; (defparameter test (make-pix 1 2))
;; (defparameter test2 (make-pix 3 4))

;; (put-pix test)
;; (get-pix *queue-of-pixels*)

;; (dequeue *queue-of-pixels*)


(defparameter *queue-of-pixels* (make-queue))
(defparameter *max-x* (- (array-dimension *test-image* 0) 1))
(defparameter *max-y* (- (array-dimension *test-image* 1) 1))

(defparameter *r* 224)
(defparameter *g* 134)
(defparameter *b* 26)

(defparameter *color-tolerance* 100)

(defparameter *new-r* 255)
(defparameter *new-g* 0)
(defparameter *new-b* 0)

(defun needed-pix? (x y r g b image)
  (if (or (< *max-x* x)
          (< *max-y* y)
          (< x 0)
          (< y 0))
      nil
      (progn
        ;; (format t "x ~A y ~A ~%" x y)
        ;; (format t "r ~A g ~A b ~A ~%" (aref image y x 0)
        ;;        (aref image y x 1) (aref image y x 2))
        (if (and (or (= r (aref image y x 0))
                     ( <= (abs (- r (aref image y x 0))) *color-tolerance*))
                 (or (= g (aref image y x 1))
                     ( <= (abs (- g (aref image y x 1))) *color-tolerance*))
                 (or (= b (aref image y x 2))
                     ( <= (abs (- b (aref image y x 2))) *color-tolerance*)))
            (progn
              (put-pix (make-pix x y))
              t
              ;;(format t "car ~A ~%" *queue-of-pixels*)
              )
            nil))))

(defstruct letter
  up down left right)

(defconstant *array-of-letters* (make-array *array-of-letters-amount*))

(defun check-sides (pix struct)
  (progn
    (if (or (null (letter-up struct))
            (< (get-y pix) (get-y (letter-up struct))))
        (setf (letter-up struct) pix))
    (if (or (null (letter-down struct))
            (> (get-y pix) (get-y (letter-down struct))))
        (setf (letter-down struct) pix))
    (if (or (null (letter-left struct))
            (< (get-x pix) (get-x (letter-left struct))))
        (setf (letter-left struct) pix))
    (if (or (null (letter-right struct))
            (> (get-x pix) (get-x (letter-right struct))))
        (progn
          (setf (letter-right struct) pix)
          struct)
        struct)))

(defun automat (pixels-queue cur-struct image)
(if (null (car pixels-queue))
    (progn
      (format t " automat: done struct ~A ~%" cur-struct)
      cur-struct
      )
    (let* ((cur-pix (get-pix pixels-queue))
           (cur-x (get-x cur-pix))
           (cur-y (get-y cur-pix)))
      ;;(format t " automat: struct ~A ~%" cur-struct)
      ;;(format t " automat: cur-pix ~A ~%" cur-pix)
      (setf cur-struct (check-sides cur-pix cur-struct))
      ;; меняем цвет пикселя
      ;;(format t "~A ~%" (length pixels-queue))
      (setf (aref image cur-y cur-x 0) *new-r*
            (aref image cur-y cur-x 1) *new-g*
            (aref image cur-y cur-x 2) *new-b*)
      ;; проверяем соседей с 4х сторон
      (needed-pix? (+ cur-x 1) cur-y *r* *g* *b* image)
      (needed-pix? (- cur-x 1) cur-y *r* *g* *b* image)
      (needed-pix? cur-x  (+ cur-y 1) *r* *g* *b* image)
      (needed-pix? cur-x  (- cur-y 1) *r* *g* *b* image)
      (automat pixels-queue cur-struct image))))

;; ТЕСТ на заполнение пустой структуры с последующим переписыванием слотов
;; (let ((cur-struct (aref *array-of-letters* 1)))
;;   (check-sides (make-pix 90 90) cur-struct)
;;   (format t "~A ~% "cur-struct)
;;   (check-sides (make-pix 30 40) cur-struct)
;;   (format t "~A ~% "cur-struct)
;;   (check-sides (make-pix 12 0) cur-struct)
;;   (format t "~A ~% "cur-struct)
;;   (check-sides (make-pix 14 32) cur-struct)
;;   (format t "~A ~% "cur-struct)
;;   (check-sides (make-pix 14 100) cur-struct)
;;   (format t "~A ~% "cur-struct)
;;   )

(defun find-letter (image)
  (do ((x 0 (incf x)))
      ((= x *max-x*))
    (do ((y 0 (incf y)))
        ((= y *max-y*))
      (if (needed-pix? x y *r* *g* *b* image)
          (let ((cur-struct (aref *array-of-letters* *array-of-letters-indx*)))
            (setf (aref *array-of-letters* *array-of-letters-indx*)
                  (automat *queue-of-pixels* cur-struct *test-image*))
            (incf *array-of-letters-indx*))))))

(defun crop-pattern (pix-struct image)
  (let* ((up (letter-up pix-struct))
         (down (letter-down pix-struct))
         (right (letter-right pix-struct))
         (left (letter-left pix-struct))
         (y-start (if (or (= (get-y up) 0) (= (get-y up) 1))
                      (get-y up)
                      (- (get-y up) 2)))
         (x-start (if (or (= (get-x left) 0) (= (get-x left) 1))
                      (get-x left)
                      (- (get-x left) 2)))
         (y-end (if (or (= (get-y down) *max-y*) (= (get-y down) (- *max-y* 1)))
                    (get-y down)
                    (+ (get-y down) 2)))
         (x-end (if (or (= (get-x right) *max-x*) (= (get-x right) (- *max-x* 1)))
                    (get-x right)
                    (+ (get-x right) 2)))
         (height-pattern (- y-end y-start))
         (width-pattern (- x-end x-start))
         (colors-pattern 4)
         (pattern-dims (list height-pattern width-pattern colors-pattern))
         (pattern-image (make-array pattern-dims :element-type '(unsigned-byte 8))))
    (format t "pattern dims ~A ~%" pattern-dims)
    (format t "struct ~A ~%" pix-struct)
    (do ((image-y y-start (incf image-y))
         (pattern-y 0 (incf pattern-y)))
        ((= image-y y-end))
      (do ((image-x x-start (incf image-x))
           (pattern-x 0 (incf pattern-x)))
          ((= image-x x-end))
        (do ((z 0 (incf z)))
            ((= z colors-pattern))
          ;; (format t "mx ~A my ~A  px ~A py ~A ~%" image-x image-y
          ;;         pattern-x pattern-y)
          ;; (format t "mx ~A my ~A  px ~A py ~A ~%" image-x image-y
          ;;         pattern-x pattern-y)
          (setf (aref pattern-image pattern-y pattern-x z)
                (aref image image-y image-x z)))))
    pattern-image))

;; TECT для проверки crop-pattern отдельно от системы
(defun test-crop-alone ()
  (defparameter *test-image-path* "~/r/prj/picture->text/aaa.png")
  (defparameter *test-image* (load-png *test-image-path*))
  (defparameter *pattern-image-path* "~/r/prj/picture->text/pattern.png")
  (let ((struct (make-letter)))
    (setf (letter-up struct) (make-pix 90 0)
          (letter-down struct) (make-pix 90 50)
          (letter-left struct) (make-pix 100 0)
          (letter-right struct) (make-pix 150 80))
    ;;(defparameter pattern (crop-pattern struct *test-image*))))
     ;; (aref pattern 49 53 0)
    (let ((pattern (crop-pattern struct *test-image*)))
      (destructuring-bind (height  width  &optional colors)
          (array-dimensions pattern)
        (format t "pattern dims ~A ~A ~A ~%" height  width colors)
        (save-png width height *pattern-image-path* pattern)))))

;; (test-crop-alone)

;;ТЕСТ crop со всей системой без цикла
(defun test-crop-with-system ()
  (defparameter *test-image-path* "~/r/prj/picture->text/aaa.png")
  (defparameter *pattern-image-path* "~/r/prj/picture->text/pattern.png")
  (defparameter *test-image* (load-png *test-image-path*))
   (defparameter *array-of-letters-indx* 0)
  
  (defconstant *array-of-letters-amount* 500)
  (dotimes (i *array-of-letters-amount*)
      (setf (aref *array-of-letters* i) (make-letter)))
  (find-letter *test-image*)
  (let ((pattern (crop-pattern (aref *array-of-letters* 0) *test-image*)))
    (destructuring-bind (height  width  &optional colors)
        (array-dimensions pattern)
      (save-png width height *pattern-image-path* pattern))))

;; (test-crop-with-system)

;; ТЕСТ crop-pattern со всей системой: задача вырезать все шаблоны по координатам в массиве
(defun test-crop-with-system-in-cycle ()
  (defparameter *test-image-path* "~/r/prj/picture->text/aaa.png")
  (defparameter *test-image* (load-png *test-image-path*))
  (defparameter *array-of-letters-indx* 0)
  (dotimes (i *array-of-letters-amount*)
    (setf (aref *array-of-letters* i) (make-letter)))
  (find-letter *test-image*)
  (do ((i 0 (incf i)))
      ((= i *array-of-letters-amount*) 'done)
    (let* ((cur-struct (aref *array-of-letters* i))
           (pattern (if (letter-up cur-struct)
                        (crop-pattern cur-struct *test-image*)
                        nil)))
      (if pattern
          (destructuring-bind (height width  &optional colors)
              (array-dimensions pattern)
            (save-png width height (format nil
                                           "/home/ss/r/prj/picture->text/patterns/pattern~A.png"
                                           i)
                      pattern))
          (return-from test-crop-with-system-in-cycle 'done)))))
;; (test-crop-with-system-in-cycle)

;; (defun test ()
;;   (defparameter *test-image-path* "~/r/prj/picture->text/aaa.png")
;;   (defparameter *test-image* (load-png *test-image-path*))
;;    (defparameter *array-of-letters-indx* 0)
;;   
;;   (defconstant *array-of-letters-amount* 500)
;;   (dotimes (i *array-of-letters-amount*)
;;       (setf (aref *array-of-letters* i) (make-letter)))
;;   (put-pix (make-pix 207 40))
;;   (let ((cur-struct (aref *array-of-letters* 0)))
;;     (setf (aref *array-of-letters* *array-of-letters-indx*)
;;           (automat *queue-of-pixels* cur-struct *test-image*))
;;   (destructuring-bind (height width colors)
;;       (array-dimensions *test-image*)
;;     (save-png width height *new-image-path* *test-image*))))

(defun test ()
  (defparameter *test-image-path* "~/r/prj/picture->text/aaa.png")
  (defparameter *test-image* (load-png *test-image-path*))
   (defparameter *array-of-letters-indx* 0)
  
  (defconstant *array-of-letters-amount* 500)
  (dotimes (i *array-of-letters-amount*)
      (setf (aref *array-of-letters* i) (make-letter)))
  (find-letter *test-image*)
  (destructuring-bind (height width colors)
      (array-dimensions *test-image*)
    (save-png width height *new-image-path* *test-image*)))

(test)
