;;; whitespace-esoteric --- 

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/whitespace
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 31 August 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defgroup ws-eso nil
  "Whitespace esoteric mode."
  :group 'languages)

(defface ws-eso-tab-face
  '((t (:background "pink" :foreground "#504945" :weight bold)))
  "Tab face."
  :group 'ws-eso)

(defface ws-eso-space-face
  '((t (:background "light blue" :foreground "#504945" :weight bold)))
  "Space face."
  :group 'ws-eso)

(defvar ws-eso-font-lock
  '(("\\(\t+\\)" (1 'ws-eso-tab-face))
    ("\\( +\\)" (1 'ws-eso-space-face))))

(defun ws-eso-wtf-p ()
  (interactive)
  (browse-url "http://compsoc.dur.ac.uk/whitespace/tutorial.html"))

(defvar ws-eso-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\t "w" st)
    (modify-syntax-entry ?\ "w" st)
    st)
  "Whitespace syntax")
  
(defvar ws-eso-menu
  '("WS"
    ["Toggle LF" ws-eso-toggle-lf :help "Toggle line-feed display"]
    ["WTF" ws-eso-wtf-p]))

(defvar ws-eso-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil ws-eso-menu)
    (define-key map (kbd "SPC")     #'self-insert-command)
    (define-key map (kbd "TAB")     #'self-insert-command)
    (define-key map (kbd "RET")     #'newline)
    (define-key map (kbd "C-c ?")   #'ws-eso-wtf-p)
    (define-key map (kbd "C-c C-t") #'ws-eso-toggle-lf)
    map))

(defvar-local ws-eso-lf nil)
(defun ws-eso-toggle-lf ()
  "Toggle [LF] display on/off."
  (interactive)
  (if ws-eso-lf
    (ws-eso-set-display)
    (let ((ws-eso-characters (append ws-eso-characters
                                     '((10 . "[LF]")))))
      (ws-eso-set-display)))
  (setq ws-eso-lf (not ws-eso-lf)))

;; http://compsoc.dur.ac.uk/whitespace/whitespace-mode.el
(defvar ws-eso-characters
  '((9 . "[TAB]")
    (32 . "[SPC]"))
  "Alist of characters and mappings to the strings they should display as.")
	
(defun ws-eso-set-display ()
  "Set up the display of characters for Whitespace esoteric mode.

This walks across `ws-eso-characters' and sets the
`buffer-display-table' accordingly."
  (let ((display-table (make-display-table)))
    (dolist (el ws-eso-characters)
      (aset display-table (car el) (string-to-vector (cdr el))))
    (setq buffer-display-table display-table)))

;;;###autoload
(define-derived-mode ws-eso-mode prog-mode "WS"
  "Whitespace esoteric mode.\n
Commands:

\\{ws-eso-map}"
  (ws-eso-set-display)
  (setq-local indent-tabs-mode t)
  (setq-local tab-always-indent nil)
  (setq font-lock-defaults `(,ws-eso-font-lock))
  (when (featurep 'yas-minor-mode)
    (yas-minor-mode -1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ws\\'" . ws-eso-mode))

(provide 'ws-eso)

;;; whitespace-esoteric.el ends here
