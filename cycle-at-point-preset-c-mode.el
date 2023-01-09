;;; cycle-at-point-preset-c-mode.el --- C preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for C

;;; Code:

(require 'cycle-at-point-find-alphabet)
(require 'cycle-at-point-find-integer)

(defun cycle-at-point-preset-c-mode ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (list
   (list :data (list "true" "false"))
   (list :data (list "&" "|"))
   (list :data (list "&=" "|="))
   (list :data (list "&&" "||"))
   (list :data (list "<" ">"))
   (list :data (list "<=" ">="))
   (list :data (list "<<" ">>"))
   (list :data (list "<<=" ">>="))
   (list :data (list "!=" "=="))
   (list :data (list "/" "*"))
   (list :data (list "/=" "*="))
   (list :data (list "+" "-"))
   (list :data (list "+=" "-="))
   (list :data (list "++" "--"))
   (list :data (list "unsigned" "signed"))
   (lambda () (cycle-at-point-find-alphabet-ascii))
   (lambda () (cycle-at-point-find-integer-bases :bases (list 10 16)))))

(provide 'cycle-at-point-preset-c-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; cycle-at-point-preset-c-mode.el ends here
