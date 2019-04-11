;;; whitespace-mode.el --- Whitespace major mode  -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/whitespace
;; Last modified: <2019-04-10.21>
;; Created: 31 August 2016

;;; Commentary:

;; Simple mode for whitespace lang

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'nvp-macro))
(declare-function orgtbl-to-orgtbl "org-table")

(defgroup wsp nil
  "Whitespace esoteric mode."
  :group 'languages)

(defface wsp-tab-face
  '((t (:background "pink" :foreground "#504945" :weight bold)))
  "Tab face."
  :group 'wsp)

(defface wsp-space-face
  '((t (:background "light blue" :foreground "#504945" :weight bold)))
  "Space face."
  :group 'wsp)


;; ------------------------------------------------------------
;;; Commands

(defun wsp-toggle-lf ()
  "Toggle [LF] display on/off."
  (interactive)
  (when (null buffer-display-table)
    (user-error "No `buffer-display-table' set in this buffer."))
  (if (aref buffer-display-table ?\C-j)
      (setf (aref buffer-display-table ?\C-j) nil)
    (aset buffer-display-table ?\C-j (string-to-vector "[LF]"))))

;;; Docs
(defvar wsp-docs
  (let* ((dir (nvp-path 'dfn :path (nvp-load-file-name)))
         (docs (expand-file-name "docs/tables.el" dir)))
    (with-temp-buffer
      (insert-file-contents-literally docs)
      (car (read-from-string (nvp-s 'bs))))))

(defun wsp-docs ()
  "Show docs in org tables."
  (interactive)
  (let* ((tname (ido-completing-read "Table: " wsp-docs))
         (dat (cadr (assoc-string tname wsp-docs))))
    (nvp-with-results-buffer (help-buffer)
      (org-mode)
      (save-excursion (insert (orgtbl-to-orgtbl dat nil)))
      (pop-to-buffer (current-buffer)))))

(defun wsp-wtf-p ()
  (interactive)
  (browse-url "http://compsoc.dur.ac.uk/whitespace/tutorial.html"))

;; ------------------------------------------------------------
;; Numbers: [SPC][TAB|SPC]..[SPC|TAB]*...[LF]
;;           ... [ - | + ]..[ 0 | 1 ] ...
(defun wsp-number-start ())


;; ------------------------------------------------------------
;;; Mode

(defconst wsp-mode-font-lock-keywords
  '(("\\(\t+\\)" (1 'wsp-tab-face))
    ("\\( +\\)" (1 'wsp-space-face))))

(defvar wsp-mode-display-table
  (eval-when-compile
    (let ((tab (make-display-table)))
      (pcase-dolist (`(,ascii . ,vec)
                     `((?\t . ,(string-to-vector "[TAB]"))
                       (?\  . ,(string-to-vector "[SPC]"))))
        (aset tab ascii vec))
      tab))
  "Display table mapping space/tab ascii values to display strings.")

(defvar wsp-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\t "w" st)
    (modify-syntax-entry ?\ "w" st)
    st)
  "Whitespace syntax")
  
(defvar wsp-mode-menu
  '("WS"
    ["Toggle LF" wsp-toggle-lf :help "Toggle line-feed display"]
    ["Info Tables" wsp-docs :help "Display info tables"]
    ["WTF" wsp-wtf-p :help "Open the tutorial page"]))

(defvar wsp-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil wsp-mode-menu)
    (define-key map (kbd "SPC")     #'self-insert-command)
    (define-key map (kbd "TAB")     #'self-insert-command)
    (define-key map (kbd "RET")     #'newline)
    (define-key map (kbd "C-c ?")   #'wsp-docs)
    (define-key map (kbd "C-c M-?") #'wsp-wtf-p)
    (define-key map (kbd "C-c C-l") #'wsp-toggle-lf)
    map))


;;;###autoload
(define-derived-mode wsp-mode prog-mode "WS"
  "Whitespace esoteric mode.\n
Commands:

\\{wsp-mode-map}"
  (setq buffer-display-table wsp-mode-display-table)
  (setq-local indent-tabs-mode t)
  (setq-local tab-always-indent nil)
  (setq font-lock-defaults (list wsp-mode-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ws\\'" . wsp-mode))

(provide 'wsp-mode)
;;; whitespace-mode.el ends here
