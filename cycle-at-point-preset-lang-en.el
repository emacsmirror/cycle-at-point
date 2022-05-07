;;; cycle-at-point-preset-lang-en.el --- English preset -*- lexical-binding: t -*-
;; URL: https://codeberg.com/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Preset for Emacs Lisp

;;; Code:

(defun cycle-at-point-preset-lang-en ()
  "Return a preset list compatible with `cycle-at-point-list'."
  (list
    ;; Days of the week.
    (list
      :data (list "MONDAY" "TUESDAY" "WEDNESDAY" "THURSDAY" "FRIDAY" "SATURDAY" "SUNDAY")
      :case-fold t)
    (list :data (list "MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN") :case-fold t)
    ;; Months of the year.
    (list
      :data
      (list
        "JANUARY"
        "FEBRUARY"
        "MARCH"
        "APRIL"
        "MAY"
        "JUNE"
        "JULY"
        "AUGUST"
        "SEPTEMBER"
        "OCTOBER"
        "NOVEMBER"
        "DECEMBER")
      :case-fold t)))

(provide 'cycle-at-point-preset-lang-en)
;;; cycle-at-point-preset-lang-en.el ends here
