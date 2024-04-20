;;; cycle-at-point-preset-cmake-mode.el --- CMAKE preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for CMAKE.

;;; Code:

(require 'cycle-at-point-find-alphabet)

(defun cycle-at-point-preset-cmake-mode ()
  "Return a preset list compatible with `cycle-at-point-list'."
  ;; Note that CMAKE is case insensitive.
  (list
   (list :data (list "TRUE" "FALSE") :case-fold t)
   (list :data (list "ON" "OFF") :case-fold t)
   (list :data (list "AND" "OR") :case-fold t)
   (list :data (list "VERSION_LESS" "VERSION_GREATER") :case-fold t)
   (list :data (list "VERSION_LESS_EQUAL" "VERSION_GREATER_EQUAL") :case-fold t)
   (lambda () (cycle-at-point-find-alphabet-ascii))))

(provide 'cycle-at-point-preset-cmake-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; cycle-at-point-preset-cmake-mode.el ends here
