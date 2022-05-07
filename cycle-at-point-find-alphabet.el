;;; cycle-at-point-find-alphabet.el --- Alphabet finder -*- lexical-binding: t -*-
;; URL: https://codeberg.com/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Finder for alphabetical characters (handy for single variable names).

;;; Code:

(require 'thingatpt) ;; `bounds-of-thing-at-point'.

(defun cycle-at-point-find-alphabet-ascii ()
  "Return alphabetical characters matching the symbol at-point."
  (let
    (
      (result (list))
      (word (bounds-of-thing-at-point 'symbol))
      (word-upcase nil))
    (when word
      (setq word (buffer-substring-no-properties (car word) (cdr word)))
      (when (eq 1 (length word))
        (setq word-upcase (upcase word))
        (when (string-match-p "[A-Z]" word-upcase)
          (setq result
            (list
              "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" ;; Alphabet literal.
              "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
          (unless (string-equal word word-upcase)
            (setq result (mapcar (lambda (w) (downcase w)) result))))))
    (list :data result)))

(provide 'cycle-at-point-find-alphabet)
;;; cycle-at-point-find-alphabet.el ends here
