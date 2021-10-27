
(defvar censor-face
  '(:foreground "black" :background "black")
  "Face to use for censoring")

(defun censor ()
  "Censor the current region"
  (interactive)
  (let ((overlay (make-overlay (region-beginning) (region-end))))
        (overlay-put overlay 'face censor-face)))

(defun censor-remove ()
  "Uncensor the current region"
  (interactive)
  (remove-overlays (region-beginning) (region-end) 'face censor-face))

(defun seq (start &optional end increment)
  "Helper function for getting sequences of numbers."
  (let ((end (or end start))
        (increment (abs (or increment 1)))
        (start (if end start 0)))
    (if (> end start)
        (loop for x from start below end by increment
              collect x)
        (loop for x downfrom start above end by increment
              collect x))))

(defun uniq (list)
  "Return a copy of list where members only occur once."
  (let ((result ()))
    (dolist (item list)
      (when (not (member item result))
        (push item result)))
    result))

(defun solve (a b c)
  "Solve a word equation of the form:
 a + b = c,
where each of a, b & c is a list of symbols.

example: (solve '(s e n d) '(m o r e) '(m o n e y)) "
  (let ((values '(0 1 2 3 4 5 6 7 8 9))
        (binding (uniq (append a b c)))
        (bind-hash (make-hash-table))
        (first-vars (mapcar 'car (list a b c))))
    (cl-labels
        ((convert (list)
                  (let ((result 0))
                    (dolist (name list)
                      (setq result (+ (* 10 result) (gethash name bind-hash))))
                    result))
         (solve1 (bindings-left values-left)
                 (if (null bindings-left)
                     (if (= (+ (convert a) (convert b)) (convert c))
                         (loop for var in binding
                               collecting (gethash var bind-hash))
                       nil)
                    (dolist (value (if (member (car bindings-left) first-vars)
                                       (remove 0 values-left) 
                                     values-left))
                      (puthash (car bindings-left) value bind-hash)
                      (let ((result (solve1 (rest bindings-left) (remove value values-left))))
                        (when result (return (loop for var in binding
                                                   collecting (cons var (gethash var bind-hash))))))))))
      (solve1 binding values))))

