;;; cycle-at-point-preset-python-mode.el --- Python preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for Python.

;;; Code:

(require 'cycle-at-point-find-alphabet)
(require 'cycle-at-point-find-integer)

(defun cycle-at-point-preset-python-mode ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (list
   (list :data (list "True" "False"))
   (list :data (list "&" "|"))
   (list :data (list "&=" "|="))
   (list :data (list "and" "or"))
   (list :data (list "is not" "is"))
   (list :data (list "<" ">"))
   (list :data (list "<=" ">="))
   (list :data (list "<<" ">>"))
   (list :data (list "<<=" ">>="))
   (list :data (list "!=" "=="))
   (list :data (list "/" "*"))
   (list :data (list "/=" "*="))
   (list :data (list "+" "-"))
   (list :data (list "+=" "-="))
   (lambda () (cycle-at-point-find-integer-bases :bases (list 2 8 10 16) :underscore-sep t))
   (lambda () (cycle-at-point-find-alphabet-ascii))))

(provide 'cycle-at-point-preset-python-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; cycle-at-point-preset-python-mode.el ends here
