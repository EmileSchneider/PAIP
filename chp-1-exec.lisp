(defparameter *title* '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General Jr MD))

(defun last-name (name)
  (if (member (first (last name)) *title*)
      (nth (- (length name) 2) name)
      (last name)))

(defun pw (base exp)
  (if (= exp 0)
      1
      (* base (pw base (- exp 1)))))

(defun count-atoms (list)
  (length (remove nil (mapcar #'atom list))))

(defun count-anywhere (item list)
  (length (remove nil (mapcar (lambda (i) (eql i item)) list))))

(defun dot-product (listA listB)
  (if (or (eql (first listA) nil) (eql (first listB) nil))
      0
      (+ (* (first listA) (first listB))
         (dot-product (rest listA) (rest listB)))))
