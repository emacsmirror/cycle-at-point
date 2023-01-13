;;; cycle-at-point-find-integer.el --- Integer finder -*- lexical-binding: t -*-
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Finder for integer number (handy for showing different representations).

;;; Code:

;; ---------------------------------------------------------------------------
;; Private Functions

(defun cycle-at-point-find-integer--format-binary (number &optional width fillchar)
  "Format NUMBER as binary.
Fill up to WIDTH with FILLCHAR (defaults to ?0) if binary
representation of NUMBER is smaller."
  (let ((nums (list))
        (fillchar (or fillchar ?0)))
    (while (> number 0)
      (push (number-to-string (% number 2)) nums)
      (setq number (truncate number 2)))
    (let ((len (length nums)))
      (apply #'concat
             (cond
              ((and width (< len width))
               (make-string (- width len) fillchar))
              (t
               ""))
             nums))))

(defun cycle-at-point-find-integer--format (num width base)
  "Format NUM with at least WIDTH space in BASE."
  (cond
   ((eq base 2)
    (cycle-at-point-find-integer--format-binary num width))
   ((eq base 8)
    (format (format "%%0%do" width) num))
   ((eq base 16)
    (format (format "%%0%dX" width) num))
   ((eq base 10)
    (format (format "%%0%dd" width) num))
   (t
    (error "Invalid base"))))


;; ---------------------------------------------------------------------------
;; Public Functions

(defun cycle-at-point-find-integer-bases (&rest args)
  "Return alternate bases at point.
Optional keyword ARGS:
:bases
  A list of bases which this mode supports.
`:undescore-sep'
  When true, ignore underscores."
  (let ((supported-bases (plist-get args :bases))
        (underscore-sep (plist-get args :underscore-sep))
        (any-chars "")
        (any-prefix "")
        (pt-init (point))
        (pt-beg (pos-bol))
        (result nil))

    (dolist (base supported-bases)
      (cond
       ((eq base 2)
        (setq any-prefix (concat any-prefix "bB"))
        (setq any-chars (concat any-chars "0-1")))
       ((eq base 8)
        (setq any-prefix (concat any-prefix "oO"))
        (setq any-chars (concat any-chars "0-7")))
       ((eq base 10)
        (setq any-chars (concat any-chars "0-9")))
       ((eq base 16)
        (setq any-prefix (concat any-prefix "xX"))
        (setq any-chars (concat any-chars "0-9a-fA-F")))))

    (save-excursion
      ;; Important not to skip over non-numbers.
      (goto-char (min (1+ (point)) (pos-eol)))
      (when (zerop
             (skip-chars-backward (concat
                                   any-chars any-prefix
                                   (cond
                                    (underscore-sep
                                     "_")
                                    (t
                                     "")))
                                  pt-beg))
        (goto-char pt-init))
      (let ((base 10)
            (pt-start (point))
            (c nil)
            (c-is-upper nil)
            (c-search nil))

        (when (looking-at (concat "0[" any-prefix "]"))
          (setq c (char-after (1+ (point))))
          (unless (string-equal (downcase (char-to-string c)) (char-to-string c))
            (setq c-is-upper t))
          (setq c (string-to-char (downcase (char-to-string c))))
          ;; Skip the prefix.
          (forward-char 2))

        (cond
         ((eq c ?b)
          (setq base 2)
          (setq c-search "[0-1_]+"))
         ((eq c ?o)
          (setq base 8)
          (setq c-search "[0-7_]+"))
         ((null c)
          (setq base 10)
          (setq c-search "[0-9_]+"))
         ((eq c ?x)
          (setq base 16)
          (setq c-search "[0-9_a-fA-F]+"))
         (t
          (error "Internal error")))

        (when (looking-at c-search)
          (let ((text (buffer-substring-no-properties pt-start (match-end 0))))
            ;; Strip prefix, also remove "_"
            (let ((text-eval (substring text (- (point) pt-start) nil)))
              (when underscore-sep
                (setq text-eval (string-replace "_" "" text-eval)))

              (let ((num (string-to-number text-eval base)))
                (dolist (base-data (list (cons 16 "0x") (cons 10 "") (cons 8 "0o") (cons 2 "0b")))
                  (let ((base-iter (car base-data))
                        (base-prefix (cdr base-data)))
                    ;; If this base is supported.
                    (when (member base-iter supported-bases)
                      (push (cond
                             ((eq base base-iter)
                              text)
                             (t
                              (when c-is-upper
                                (setq base-prefix (upcase base-prefix)))
                              (concat
                               base-prefix
                               (cycle-at-point-find-integer--format num 1 base-iter))))
                            result))))))))))
    (list :data result)))

(provide 'cycle-at-point-find-integer)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; cycle-at-point-find-integer.el ends here
