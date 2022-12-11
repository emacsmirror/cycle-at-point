;;; cycle-at-point-preset-emacs-lisp-mode.el --- ELISP preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for ELISP.

;;; Code:

(require 'cycle-at-point-find-alphabet)

(defun cycle-at-point-preset-emacs-lisp-mode ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (list
   (list :data (list "t" "nil"))
   (list :data (list "and" "or"))
   (list :data (list "when" "unless"))
   (list :data (list "<" ">"))
   (list :data (list "/" "*"))
   (list :data (list "+" "-"))
   (list :data (list ">=" "<="))
   (list :data (list "car" "cdr"))
   (list :data (list "string-lessp" "string-greaterp"))
   (lambda () (cycle-at-point-find-alphabet-ascii))))

(provide 'cycle-at-point-preset-emacs-lisp-mode)
;;; cycle-at-point-preset-emacs-lisp-mode.el ends here
