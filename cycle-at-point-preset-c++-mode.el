;;; cycle-at-point-preset-c++-mode.el --- C++ preset -*- lexical-binding: t -*-
;; URL: https://gitlab.com/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for C++

;;; Code:

(require 'cycle-at-point-preset-c-mode)
(defun cycle-at-point-preset-c++-mode ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (append
    (cycle-at-point-preset-c-mode)
    (list
      ;; Mostly shared with C.
      (list :data (list "public" "private")))))

(provide 'cycle-at-point-preset-c++-mode)
;;; cycle-at-point-preset-c++-mode.el ends here
