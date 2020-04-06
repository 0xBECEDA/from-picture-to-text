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


(defparameter *test-image-path* "~/r/prj/picture->text/test.png")
(defparameter *test-image* (load-png *test-image-path*))
(defparameter *new-image-path* "~/r/prj/picture->text/result.png")
(defparameter *array-of-letters-indx* 0)

(defparameter *array-of-letters-amount* 500)

(defparameter *queue-of-pixels* (make-queue))
(defparameter *max-x* 0)
(defparameter *max-y* 0)

(defparameter *r* 0)
(defparameter *g* 0)
(defparameter *b* 0)

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

(defparameter *array-of-letters* (make-array *array-of-letters-amount*))

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
      ;;(format t " automat: done struct ~A ~%" cur-struct)
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

(defun test-automat-alone (test-image-path new-image-path amount-of-structs r g b)
  (setf *test-image-path* test-image-path
        *new-image-path* new-image-path
        *test-image* (load-png *test-image-path*)
        *array-of-letters-indx* 0
        *array-of-letters-amount* amount-of-structs
        *max-x* (- (array-dimension *test-image* 1) 1)
        *max-y* (- (array-dimension *test-image* 0) 1)
        *r* r
        *b* b
        *g* g)
  (dotimes (i *array-of-letters-amount*)
    (setf (aref *array-of-letters* i) (make-letter)))
  (put-pix (make-pix 207 40))
  (let ((cur-struct (aref *array-of-letters* 0)))
    (setf (aref *array-of-letters* *array-of-letters-indx*)
          (automat *queue-of-pixels* cur-struct *test-image*))
    (destructuring-bind (height width colors)
        (array-dimensions *test-image*)
      (save-png width height *new-image-path* *test-image*))))

;; (test-automat-alone "~/r/prj/picture->text/aaa.png" "~/r/prj/picture->text/result.png"
;;                     100 248 121 29)

(defun test-automat-with-find-letter (test-image-path new-image-path amount-of-structs
                                      r g b)
  (setf *test-image-path* test-image-path
        *new-image-path* new-image-path
        *test-image* (load-png *test-image-path*)
        *array-of-letters-indx* 0
        *array-of-letters-amount* amount-of-structs
        *max-x* (- (array-dimension *test-image* 1) 1)
        *max-y* (- (array-dimension *test-image* 0) 1)
        *r* r
        *b* b
        *g* g)
  (dotimes (i *array-of-letters-amount*)
    (setf (aref *array-of-letters* i) (make-letter)))
  (find-letter *test-image*)
  (destructuring-bind (height width colors)
      (array-dimensions *test-image*)
    (save-png width height *new-image-path* *test-image*)))

;; (test-automat-with-find-letter
;;  "~/r/prj/picture->text/aaa.png" "~/r/prj/picture->text/result.png" 500 248 121 29)


;; ТЕСТ на заполнение пустой структуры с последующим переписыванием слотов
(defun test-check-sides (amount-of-structs)
  (setf *array-of-letters-indx* 0
        *array-of-letters-amount* amount-of-structs)
  (dotimes (i *array-of-letters-amount*)
    (setf (aref *array-of-letters* i) (make-letter)))
  (let ((cur-struct (aref *array-of-letters* 1)))
    (check-sides (make-pix 90 90) cur-struct)
    (format t "~A ~% "cur-struct)
    (check-sides (make-pix 30 40) cur-struct)
    (format t "~A ~% "cur-struct)
    (check-sides (make-pix 12 0) cur-struct)
    (format t "~A ~% "cur-struct)
    (check-sides (make-pix 14 32) cur-struct)
    (format t "~A ~% "cur-struct)
    (check-sides (make-pix 14 100) cur-struct)
    (format t "~A ~% "cur-struct)
    ))

;; (test-check-sides 5)
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
      ;; (format t "pattern dims ~A ~%" pattern-dims)
      ;; (format t "struct ~A ~%" pix-struct)
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
(defun test-crop-alone (test-image-path pattern-image-path)
  (setf *test-image-path* test-image-path
        *test-image* (load-png *test-image-path*))
  (let ((struct (make-letter)))
    (setf (letter-up struct) (make-pix 90 0)
          (letter-down struct) (make-pix 90 50)
          (letter-left struct) (make-pix 100 0)
          (letter-right struct) (make-pix 150 80))
    (let ((pattern (crop-pattern struct *test-image*)))
      (destructuring-bind (height  width  &optional colors)
          (array-dimensions pattern)
        (save-png width height pattern-image-path pattern)))))

;; (test-crop-alone "~/r/prj/picture->text/aaa.png"
;;                  "~/r/prj/picture->text/crops/pattern.png")

  ;;ТЕСТ crop со всей системой без цикла
(defun test-crop-with-system (test-image-path pattern-image-path amount-of-structs r g b)
  (setf *test-image-path* test-image-path
        *test-image* (load-png *test-image-path*)
        *array-of-letters-indx* 0
        *array-of-letters-amount* amount-of-structs
        *max-x* (- (array-dimension *test-image* 1) 1)
        *max-y* (- (array-dimension *test-image* 0) 1)
        *r* r
        *b* b
        *g* g)
    (dotimes (i *array-of-letters-amount*)
      (setf (aref *array-of-letters* i) (make-letter)))
    (find-letter *test-image*)
    (let ((pattern (crop-pattern (aref *array-of-letters* 0) *test-image*)))
      (destructuring-bind (height  width  &optional colors)
          (array-dimensions pattern)
        (save-png width height *pattern-image-path* pattern))))

;; (test-crop-with-system "~/r/prj/picture->text/aaa.png"
;;                        "~/r/prj/picture->text/crops/pattern.png" 5 248 121 29)

  ;; ТЕСТ crop-pattern со всей системой: задача вырезать все шаблоны по координатам в массиве
(defun test-crop-with-system-in-cycle
    (test-image-path pattern-image-path amount-of-structs r g b)
  (setf *test-image-path* test-image-path
        *test-image* (load-png *test-image-path*)
        *array-of-letters-indx* 0
        *array-of-letters-amount* amount-of-structs
        *max-x* (- (array-dimension *test-image* 1) 1)
        *max-y* (- (array-dimension *test-image* 0) 1)
        *r* r
        *b* b
        *g* g)
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
            (save-png width height
                      (format nil
                              "/home/ss/r/prj/picture->text/crops/pattern~A.png" i)
                      pattern))
          (return-from test-crop-with-system-in-cycle 'done)))))

;; (test-crop-with-system-in-cycle "~/r/prj/picture->text/aaa.png"
;;                                 "~/r/prj/picture->text/crops/pattern~A.png" 400 248 121 29)

  (defstruct node
    elt l r)

  (defun bst-insert (bst obj fn)
    (if (null bst)
        (make-node :elt obj)
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              bst
              (if (funcall fn obj elt)
                  (make-node
                   :elt elt
                   :l (bst-insert (node-l bst) obj fn)
                   :r (node-r bst))
                  (make-node
                   :elt elt
                   :r (bst-insert (node-r bst) obj fn)
                   :l (node-l bst)))))))

  (defun bst-find (bst obj fn)
    (if (null bst)
        nil
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              bst
              (if (funcall fn obj elt)
                  (bst-find (node-l bst) obj fn)
                  (bst-find (node-r bst) obj fn))))))

  (defun test-bst-insert (list fn)
    (let ((count (length list))
          (tree))
      (dotimes (i count)
        (setf tree (bst-insert tree (car list) fn))
        (setf list (cdr list)))
      tree))

  ;; (test-bst-insert '(2 5 9 8 3 1) #'<)

  (defun test-bst-find (list fn obj)
    (let ((tree (test-bst-insert list fn)))
      (bst-find tree obj fn)))

  ;;(test-bst-find '(2 5 9 8 3 1 10 7) #'< 7)
(defun merge-patterns (pattern1 pattern2)
  ;;(format t "merged-patterns! ~%")
  (destructuring-bind (height-pattern1 width-pattern1 colors-pattern1)
      (array-dimensions pattern1)
    (destructuring-bind (height-pattern2 width-pattern2 colors-pattern2)
        (array-dimensions pattern2)
      (assert (and (equal height-pattern1 height-pattern2)
                   (equal width-pattern1 width-pattern2)
                   (equal colors-pattern1 colors-pattern2)))
      (let ((merged-image (make-array (list height-pattern1
                                            width-pattern1 colors-pattern1)
                                      :element-type '(unsigned-byte 8))))
        (do ((y 0 (incf y)))
            (( = y height-pattern1))
          (do ((x 0 (incf x)))
              ((= x width-pattern1))
            (do ((z 0 (incf z)))
                ((= z colors-pattern1))
              (setf (aref merged-image y x z)
                    (round (float (/ (+ (aref pattern1 y x z)
                                        (aref pattern2 y x z)) 2)))))))
        merged-image))))

(defun test-merge-patterns (pattern1-path pattern2-path)
  (let* ((pattern1 (load-png pattern1-path))
        (pattern2 (load-png pattern2-path))
        (merged-image (merge-patterns pattern1 pattern2)))
          (destructuring-bind (height width &optional colors)
              (array-dimensions merged-image)
            (save-png width height "/home/ss/r/prj/picture->text/merged-image.png"
                      merged-image))))
;; ТЕСТ с двумя одинаковыми шаблонами

;; (test-merge-patterns "/home/ss/r/prj/picture->text/crops/pattern17.png"
;;                      "/home/ss/r/prj/picture->text/crops/pattern61.png")

;; ТЕСТ с двумя рзными шаблонами
;; (test-merge-patterns "/home/ss/r/prj/picture->text/crops/pattern17.png"
;;                      "/home/ss/r/prj/picture->text/crops/pattern32.png")

;; ТЕСТ с двумя разными шаблонами разных размеров
;; (test-merge-patterns "/home/ss/r/prj/picture->text/crops/pattern17.png"
;;                      "/home/ss/r/prj/picture->text/crops/pattern66.png")

(defun xor-image (y-start y-end x-start x-end pattern image)
  (destructuring-bind (height-pattern width-pattern colors-pattern)
      (array-dimensions pattern)
    (let ((xored-symbol-array (make-array (list height-pattern width-pattern
                                                colors-pattern)
                                          :element-type '(unsigned-byte 8))))
      ;; осуществляем xor пикселей
      (do ((image-y y-start (incf image-y))
           (xored-y 0 (incf xored-y))
           (pattern-y 0 (incf pattern-y)))
          ((= image-y y-end))
        (do ((image-x x-start (incf image-x))
             (xored-x 0 (incf xored-x))
             (pattern-x 0 (incf pattern-x)))
            ((= image-x x-end))
          (do ((z 0 (incf z)))
              ((= z 2))
            (setf (aref xored-symbol-array xored-y xored-x z)
                  (logxor (aref image image-y image-x z)
                          (aref pattern pattern-y pattern-x z))))))
      ;; поправляем альфа-канал
      (do ((y 0 (incf y)))
          ((= y height-pattern))
        (do ((x 0 (incf x)))
            ((= x width-pattern))
          (setf (aref xored-symbol-array y x 3) 255)))
      xored-symbol-array)))

;; ТЕСТ ксора двух изображений совместно в automat
(defun test-xor-image (test-image-path new-image-path save-crop-path amount-of-structs
                       r g b)
  (test-automat-with-find-letter test-image-path new-image-path amount-of-structs
                                 r g b)
  (let ((test-image (load-png test-image-path))
        (result-image (load-png new-image-path)))
    (destructuring-bind (height width colors)
        (array-dimensions result-image)
      (let ((xored-image (xor-image 0 (- height 1) 0 (- width 1) result-image
                                    test-image)))
        (destructuring-bind (height-xored width-xored &optional colors)
            (array-dimensions xored-image)
          (save-png width-xored height-xored save-crop-path xored-image)))
      )))

;; (test-xor-image "~/r/prj/picture->text/aaa.png" "~/r/prj/picture->text/result.png"
;;                 "~/r/prj/picture->text/xor.png"
;;                 500 248 121 29)

(defun count-inaccuracy (inaccuracy xored-image)
  (destructuring-bind (height width &optional colors)
      (array-dimensions xored-image)
    (let ((pix-inaccuracy-start (float (* (/ (* height width) 100) inaccuracy)))
          (pix-inaccuracy-end 0))
      ;; (format t "count-inaccuracy: start ~A ~%" pix-inaccuracy-start)
      (do ((y 0 (incf y)))
          ((= y height))
        (do ((x 0 (incf x)))
            ((= x width))
          (if (not (= (aref xored-image y x 0)
                      (aref xored-image y x 1)
                      (aref xored-image y x 2)
                      0))
              (progn
                (incf pix-inaccuracy-end)
                ;;(format t "count-inaccuracy: ~A ~%" pix-inaccuracy-end)
                (if (> pix-inaccuracy-end pix-inaccuracy-start)
                    (return-from count-inaccuracy
                      (float (/ (* pix-inaccuracy-end 100) (* height width)))))))))
      ;; (format t "count-inaccuracy: ~A ~%" pix-inaccuracy-end)
      ;; (format t "count-inaccuracy: height ~A width ~A ~%" height width)
      (float (/ (* pix-inaccuracy-end 100) (* height width))))))


;;ТЕСТ count-inaccuracy на 100% совпадающем изображении (ксорим 2 одианковых изображения)
(defun test-count-inaccuracy (path1 path2 xor-path inaccuracy)
  (let ((test-image1 (load-png path1))
        (test-image2 (load-png path2)))
    (destructuring-bind (height width colors)
        (array-dimensions test-image2)
      (let ((xored-image (xor-image 0 (- height 1) 0 (- width 1) test-image1
                                    test-image2)))
        (destructuring-bind (height-xored width-xored &optional colors)
            (array-dimensions xored-image)
          (save-png width-xored height-xored
                    xor-path xored-image))
        (count-inaccuracy inaccuracy xored-image)))))

;; (test-count-inaccuracy "/home/ss/r/prj/picture->text/rrr/pattern19.png"
;;                        "/home/ss/r/prj/picture->text/rrr/pattern21.png"
;;                        "/home/ss/r/prj/picture->text/xor.png"  20)

(defun make-patterns (patterns-path inaccuracy dir-ready-patterns-path num-patterns)
  (defun merdge-and-save-patterns (cur-pattern deleted-patterns-tree n)
    (cond ((< n 0) (values cur-pattern deleted-patterns-tree))
          ((bst-find deleted-patterns-tree n #'>)
           (merdge-and-save-patterns
            cur-pattern deleted-patterns-tree (- n 1)))
          (t
           (let* ((next-pattern (load-png (format nil patterns-path n))))
             (destructuring-bind (height-next width-next &optional colors-next)
                 (array-dimensions next-pattern)
               (destructuring-bind (height-cur width-cur &optional colors-cur)
                   (array-dimensions cur-pattern)
                 (if (and (equal height-next height-cur)
                          (equal width-next width-cur))
                     (let* ((xored-image (xor-image 0 (- height-next 1) 0 (- width-next 1)
                                                    next-pattern cur-pattern))
                            (cur-inaccuracy (count-inaccuracy inaccuracy xored-image)))
                       ;; (format t
                       ;; "make-patterns: n ~A cur-inaccuracy ~A inaccuracy ~A ~%" n
                       ;;         cur-inaccuracy inaccuracy)
                       (if (< cur-inaccuracy inaccuracy)
                           (progn
                             ;; (format t "merged! ~%")
                             (merdge-and-save-patterns
                              (merge-patterns cur-pattern next-pattern)
                              (bst-insert deleted-patterns-tree n #'>)
                              (- n 1)))
                           (progn
                             ;; (format t "not merged! ~%")
                             (merdge-and-save-patterns cur-pattern deleted-patterns-tree
                                                       (- n 1)))))
                     (progn
                       ;; (format t "not equal! n ~A ~%" n)
                       (merdge-and-save-patterns
                        cur-pattern deleted-patterns-tree (- n 1))))))))))
  (defun iter (deleted-patterns-tree n)
    (cond ((< n 1) ;; (format t "~A "deleted-patterns-tree))
           'done)
          ((bst-find deleted-patterns-tree n #'>)
           (progn
             ;; (format t "~% iter: pattern n ~A is deleted ~%" n)
             (iter deleted-patterns-tree (- n 1))))
          (t
           (let ((cur-pattern (load-png (format nil patterns-path n))))
             ;; (format t "~% iter: pattern n ~A ~%" n)
             (multiple-value-bind (merged-pattern tree)
                 (merdge-and-save-patterns cur-pattern deleted-patterns-tree (- n 1))
               (destructuring-bind (height width &optional colors)
                   (array-dimensions merged-pattern)
                 ;;  (format t dir-ready-patterns-path n)
                 (save-png width height
                           (format nil dir-ready-patterns-path n) merged-pattern)
                 (setf deleted-patterns-tree (bst-insert tree n #'>))
                 ;; (format t " ~% n ~A: ~A ~%" n deleted-patterns-tree)
                 (iter deleted-patterns-tree (- n 1))))))))
  (iter '() num-patterns))

;;ТЕСТ без интегрирования в систему (все шаблоны вырезаны заранее и положены в папку)
;; (make-patterns "/home/ss/r/prj/picture->text/crops/pattern~A.png" 5
;;                "/home/ss/r/prj/picture->text/patterns/pattern~A.png" 3)

;; ТЕСТ c интегрированием в систему
(defun test-make-patterns-with-system (test-image-path crops-path patterns-path inaccuracy
                                       amount-of-structs r g b nums-pattern)
  (test-crop-with-system-in-cycle
   test-image-path patterns-path amount-of-structs r g b)
  (make-patterns crops-path inaccuracy patterns-path nums-pattern))

;; (test-make-patterns-with-system "/home/ss/r/prj/picture->text/aaa.png"
;;                                 "/home/ss/r/prj/picture->text/crops/pattern~A.png"
;;                                 "/home/ss/r/prj/picture->text/patterns/pattern~A.png" 4
;;                                 500 248 121 29 50)

(defun compare-iter (y-start y-end x-start x-end height-symbol width-symbol
                     inaccuracy  patterns-path-list pattern-result image)
  ;; (format t patterns-path num-patterns)
  (cond ((and  (null patterns-path-list) (null pattern-result)) nil)
        ((and (null patterns-path-list) pattern-result) pattern-result)
        (t
         (let ((pattern-array (load-png (car patterns-path-list))))
           (destructuring-bind (height-array width-array &optional colors-array)
               (array-dimensions pattern-array)
             (if (and (equal height-array height-symbol)
                      (equal width-array width-symbol))
                 (progn
                   ;;(format t "~% true1 ~%")
                   (let* ((xored-image (xor-image y-start y-end x-start x-end
                                                  pattern-array image))
                          (cur-inaccuracy (count-inaccuracy inaccuracy xored-image)))
                     ;; (format t "~A ~A ~% " pattern-result cur-inaccuracy)
                     (if (or (and (null pattern-result) ( < cur-inaccuracy inaccuracy))
                             (and pattern-result
                                  (< cur-inaccuracy (cadr pattern-result))))
                         (progn
                           ;;(format t "true2 ~%")
                           (setf pattern-result (list (car patterns-path-list)
                                                      cur-inaccuracy))
                           (compare-iter
                            y-start y-end x-start x-end height-symbol width-symbol
                            inaccuracy (cdr patterns-path-list) pattern-result image))
                         (compare-iter
                          y-start y-end x-start x-end height-symbol width-symbol
                          inaccuracy (cdr patterns-path-list) pattern-result image )))
                   )
                 (compare-iter y-start y-end x-start x-end height-symbol width-symbol
                               inaccuracy (cdr patterns-path-list) pattern-result
                               image)))))))

(defun compare (image symbol-struct patterns-path-list inaccuracy)
  (let* ((up (letter-up symbol-struct))
         (down (letter-down symbol-struct))
         (right (letter-right symbol-struct))
         (left (letter-left symbol-struct))
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
         (height-symbol (- y-end y-start))
         (width-symbol (- x-end x-start)))
    (compare-iter y-start y-end x-start x-end height-symbol width-symbol
          inaccuracy patterns-path-list '() image)))


;; ТЕСТ с двумя одинаковыми изображениями
;; вырезает шаблон и накладывается на свои же координаты
(defun test-compare-same-images (test-image-path pattern-image-path inaccuracy)
  (setf *test-image-path* test-image-path
        *test-image* (load-png *test-image-path*))
  (let ((struct (make-letter)))
    (setf (letter-up struct) (make-pix 90 0)
          (letter-down struct) (make-pix 90 50)
          (letter-left struct) (make-pix 100 0)
          (letter-right struct) (make-pix 150 80))
    (let ((pattern (crop-pattern struct *test-image*)))
      (destructuring-bind (height  width  &optional colors)
          (array-dimensions pattern)
        (save-png width height pattern-image-path pattern)))
    (compare *test-image* struct (list pattern-image-path) inaccuracy)))

;; (test-compare-same-images "/home/ss/r/prj/picture->text/aaa.png"
;;                           "/home/ss/r/prj/picture->text/pattern.png" 20)

;; ТЕСТ сравнения одного изображения с разными шаблонами
;; шаблоны вырезаны заранее
(defun test-compare-one-image-few-patterns
    (test-image-path pattern-image-path amount-of-structs r g b inaccuracy)
  (setf *test-image-path* test-image-path
        *test-image* (load-png *test-image-path*)
        *max-x* (- (array-dimension *test-image* 1) 1)
        *max-y* (- (array-dimension *test-image* 0) 1)
        *array-of-letters-indx* 0
        *array-of-letters-amount* amount-of-structs
        *r* r
        *b* b
        *g* g)
  (dotimes (i *array-of-letters-amount*)
    (setf (aref *array-of-letters* i) (make-letter)))
  (find-letter *test-image*)
  (compare *test-image* (aref *array-of-letters* 1) pattern-image-path inaccuracy))

;; (test-compare-one-image-few-patterns
;;  "/home/ss/r/prj/picture->text/aaa.png"
;;  (list
;;   "/home/ss/r/prj/picture->text/patterns/pattern1.png"
;;   "/home/ss/r/prj/picture->text/patterns/pattern2.png"
;;   "/home/ss/r/prj/picture->text/patterns/pattern3.png"
;;   "/home/ss/r/prj/picture->text/patterns/pattern4.png")
;;  500 248 121 29 30)
  (defun get-symbol-from-the-path (pathname)
    (let ((substring (subseq pathname (- (length pathname) 5))))
      (aref substring 0)))


  ;; (get-symbol-from-the-path "/home/ss/r/prj/picture->text/patterns/4.png")

  ;; (get-symbol-from-the-path "/home/ss/r/patterns/4.png")

(defun write-down (string file)
  (let ((path (make-pathname :name file)))
    (with-open-file (str path :direction :output
                         :if-exists :supersede)

      (format str "~A" string)))
    'done)

;; (write-down "abdhjklds" "test-file.txt")

(defun crop-all-patterns (patterns-path)
  (do ((i 0 (incf i)))
      ((= i *array-of-letters-amount*) (- i 1 ))
    (let* ((cur-struct (aref *array-of-letters* i))
           (pattern (if (letter-up cur-struct)
                        (crop-pattern cur-struct *test-image*)
                        nil)))
      ;;(format t "pattern! ~%")
      (if pattern
          (destructuring-bind (height width  &optional colors)
              (array-dimensions pattern)
            (save-png width height
                      (format nil
                              patterns-path i)
                      pattern))
          (return-from crop-all-patterns (- i 1))))))


(defun collect-patterns (image-path crops-path final-patterns-path size-array-letters
                         r g b cur-color-tolerance pix-inaccuracy)
  (setf *test-image-path* image-path
        *test-image* (load-png *test-image-path*)
        *array-of-letters-amount* size-array-letters
        *array-of-letters-indx* 0
        *color-tolerance* cur-color-tolerance
        *max-x* (- (array-dimension *test-image* 1) 1)
        *max-y* (- (array-dimension *test-image* 0) 1)
        *r* r
        *b* b
        *g* g)
  (dotimes (i *array-of-letters-amount*)
    (setf (aref *array-of-letters* i) (make-letter)))
    (find-letter *test-image*)
    (let ((amount-of-crops
           (crop-all-patterns crops-path)))
      (make-patterns crops-path pix-inaccuracy final-patterns-path amount-of-crops)
      (format
       t
       "~% collect-patterns: I'm done. Please, rename patterns correctly for next job ~%")
      ))

;; ЗАПУСК первой ступени

;; (collect-patterns "/home/ss/r/prj/picture->text/test.png"
;;                   "/home/ss/r/prj/picture->text/crops/pattern~A.png"
;;                   "/home/ss/r/prj/picture->text/patterns/pattern~A.png"
;;                   500 248 121 29 100 5)
(defun change-cur-work-dir (work-dir-path)
  (sb-posix:chdir work-dir-path)
  (setf *default-pathname-defaults* (sb-ext:native-pathname
                                     (format nil "~A~A" (sb-posix:getcwd) "/"))))

(defun compare-all-symbols (patterns-dir-path image inaccuracy)
  (defun compare-all-symbols-iter (list-of-patterns-path symbols-string i)
    (if (= i *array-of-letters-amount*)
        symbols-string
        (let* ((cur-symbol-struct (aref *array-of-letters* i)))
          (if (not (letter-up cur-symbol-struct))
              (return-from compare-all-symbols-iter symbols-string)
              (let ((cur-pattern-path (compare image cur-symbol-struct
                                               list-of-patterns-path inaccuracy)))
                (if cur-pattern-path
                    (progn
                      ;;(format t "~A ~%" (car cur-pattern-path))
                      (let ((cur-symbol (get-symbol-from-the-path
                                         (format nil "~A"(car cur-pattern-path)))))
                        (setf symbols-string (concatenate 'string symbols-string
                                                          (format nil "~A"
                                                                  cur-symbol)))
                        (compare-all-symbols-iter
                         list-of-patterns-path symbols-string (incf i))))
                    (compare-all-symbols-iter
                     list-of-patterns-path symbols-string (incf i))))))))
  (change-cur-work-dir patterns-dir-path)
  (let ((all-patterns-paths-list (uiop:directory-files "./"))
        (all-symbols-string ""))
    (compare-all-symbols-iter all-patterns-paths-list all-symbols-string 0)))

(defun compare-and-write-down-all-symbols (patterns-dir-path image inaccuracy text-file)
  (write-down (compare-all-symbols patterns-dir-path image inaccuracy)
              text-file))

;; ЗАПУСК второй ступени
;; (compare-and-write-down-all-symbols "/home/ss/r/prj/picture->text/patterns"
;;                                     *test-image* 5 "file.txt")
