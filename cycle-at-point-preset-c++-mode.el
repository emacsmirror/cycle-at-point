;;; cycle-at-point-preset-c++-mode.el --- C++ preset -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for C++

;;; Code:

(require 'cycle-at-point-preset-c-mode)
(defun cycle-at-point-preset-c++-mode ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (declare (important-return-value t))
  (append
   (cycle-at-point-preset-c-mode)
   (list
    ;; Mostly shared with C.
    (list :data (list "public" "private")))))

(provide 'cycle-at-point-preset-c++-mode)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; cycle-at-point-preset-c++-mode.el ends here
