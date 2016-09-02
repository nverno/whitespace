(require 'json)
(require 'cl-lib)

(defun batch-convert ()
  (defvar command-line-args-left)
  (let ((error nil))
    (while command-line-args-left
      (let* ((infile (car command-line-args-left))
             (outfile (concat (file-name-sans-extension infile) ".dat")))
        (message "%s -> %s" infile outfile)
       (build-el infile outfile))
      (setq command-line-args-left (cdr command-line-args-left)))))

(defun build-el (in out)
  (save-dat (format-dat (load-dat in)) (expand-file-name out)))

(defun load-dat (file)
  (let* ((json-key-type 'string))
    (json-read-file file)))

(defun save-dat (dat file)
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string dat))
      (write-region (point-min) (point-max) file))))

(defun format-dat (dat)
  "Format as an org table."
  (let ((names '("IMP" "Stack Manipulation [SPC]" "Arithmetic [TAB][SPC]"
                 "Heap Access [TAB][TAB]" "Flow Control [LF]"
                 "I/O [TAB][LF]" "Example")))
    (cl-loop
       for i from 0 to (1- (length names))
       collect (let* ((data (cdr (assoc-string (number-to-string i) dat)))
                      (head (append (cdr (assoc-string "head" data)) ()))
                      (rows (append (cdr (assoc-string "rows" data)) ()))
                      (cols (length head)))
                 `(,(nth i names) .
                   (,(append (cons head nil)
                             (cons 'hline nil) (split-list rows cols) nil)))))))

(defun split-list (lst n)
  (cl-loop for i from 0 to (1- (length lst)) by n
     collect (butlast (nthcdr i lst) (- (length lst) (+ n i)))))
