;;; cycle-at-point.el --- Cycle (rotate) the thing under the cursor -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2022-2024 Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>
;; Maintainer: Campbell Barton <ideasman42@gmail.com>
;; URL: https://codeberg.org/ideasman42/emacs-cycle-at-point
;; Version: 0.2
;; Created: 2022-02-05
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1") (recomplete "0.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; M-x cycle-at-point cycles the point at the cursor,
;; this should be bound to a key.

;;; Code:

;; ---------------------------------------------------------------------------
;; Require Dependencies

(require 'recomplete) ; `recomplete-with-callback'.

(eval-when-compile
  (require 'cycle-at-point-find-alphabet)
  (require 'cycle-at-point-find-integer)
  (require 'cycle-at-point-preset-c++-mode)
  (require 'cycle-at-point-preset-c-mode)
  (require 'cycle-at-point-preset-cmake-mode)
  (require 'cycle-at-point-preset-emacs-lisp-mode)
  (require 'cycle-at-point-preset-lang-en)
  (require 'cycle-at-point-preset-python-mode))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom cycle-at-point-preset-override nil
  "The symbol to use for the preset, when nil the `major-mode' is used.
You may wish to override this value to us a preset from a different major mode."
  :type '(choice (const nil) string))

(defvar-local cycle-at-point-list nil
  "Buffer local list of literals to cycle.

A function that returns a list is also supported.

When left unset this this auto-detected from the mode.

Each list item can contain keyword/value pairs:

`:data'
  Where the value is a list of strings,
  or a function that returns a list of strings when called (required).
`:case-fold'
  Where the value is a boolean for case insensitive matching
  (optional, nil by default).

  When true, matching the literals is case insensitive.
  Replacements follow the current case: lower, upper or title-case.")


;; ---------------------------------------------------------------------------
;; Private Functions

(defun cycle-at-point--cycle-words (cycle-data)
  "Return the bounds of the thing at point from CYCLE-DATA."
  (declare (important-return-value t))
  (let ((cycle-data-index 0) ; Only for error messages.
        (prefix "cycle-at-point")
        (pt (point))
        (line-beg (pos-bol))
        (line-end (pos-eol))

        (result nil))

    (while (and (null result) cycle-data)
      ;; Extract keyword arguments from `arg-data'.
      (let ((arg-data (pop cycle-data))

            (arg-case-fold nil)
            (arg-words nil))

        ;; May be callable.
        (when (functionp arg-data)
          (condition-case-unless-debug err
              (save-match-data
                (save-excursion
                  (unless (eq (point) pt)
                    (error "Internal error, unexpected point motion"))
                  (setq arg-data (funcall arg-data))))
            (error
             (user-error "%s: (error at index %d) unable to use callback (%S)"
                         prefix
                         cycle-data-index
                         err))))

        ;; Parse arguments.
        (while arg-data
          (let ((arg-current (pop arg-data)))
            (cond
             ((keywordp arg-current)
              (unless arg-data
                (user-error "%s: (error at index %d) keyword argument %S has no value!"
                            prefix
                            cycle-data-index
                            arg-current))
              (let ((v (pop arg-data)))
                (pcase arg-current
                  (:data
                   (cond
                    ;; Callback to generate data.
                    ((functionp v)
                     (condition-case-unless-debug err
                         (save-match-data
                           (save-excursion
                             (unless (eq (point) pt)
                               (error "Internal error, unexpected point motion"))
                             (setq v (funcall v))
                             (unless (listp v)
                               (error "Expected a list of strings, not %S = %S" (type-of v) v))))
                       (error
                        (user-error "%s: (error at index %d), :data callback failure %S"
                                    prefix
                                    cycle-data-index
                                    err))))
                    ((listp v))
                    (t
                     (error "%s: expected `:data', to be a list of strings, found %S"
                            prefix
                            (type-of v))))
                   (setq arg-words v))
                  (:case-fold
                   (cond
                    ((memq v (list nil t)))
                    (t
                     (error "%s: expected `:case-fold', to be nil or t" prefix)))
                   (setq arg-case-fold v))
                  (_ (error "Unknown argument %S" arg-current)))))
             (t
              (user-error
               "%s: (error at index %d) all arguments must be keyword, value pairs, found %S"
               prefix cycle-data-index (type-of arg-current))))))
        ;; Done parsing arguments.

        (let* ((case-fold-search arg-case-fold)
               (words-max 0)
               (words-length 0)
               (words-regex
                (concat
                 (mapconcat (lambda (literal)
                              (setq words-max (max words-max (length literal)))
                              (setq words-length (1+ words-length))
                              ;; Use groups so they can be checked.
                              (concat "\\(" (regexp-quote literal) "\\)"))
                            arg-words
                            "\\|")))

               ;; Anything outside this range wont overlap `pt'.
               (search-min (max (- pt words-max) line-beg))
               (search-max (min (+ pt words-max) line-end)))

          (save-match-data
            (save-excursion
              (goto-char search-min)
              (while (and (< (point) search-max) (re-search-forward words-regex search-max t))
                (let ((beg (match-beginning 0))
                      (end (match-end 0)))

                  ;; Even if the point has been found,
                  ;; it's possible the word found was shorter, meaning the point is not over it.
                  (cond
                   ;; Keep searching.
                   ((< end pt)
                    (goto-char (1+ beg)))
                   ;; Stop searching (past the point).
                   ((< pt beg)
                    (goto-char search-max))

                   ;; Overlapping match, now check delimiters (via syntax table).

                   ;; Delimit on unchanged syntax-class at the beginning.
                   ((and (< line-beg beg) (eq (syntax-after beg) (syntax-after (1- beg))))
                    ;; This match doesn't end at delimiters, keep searching.
                    (goto-char (1+ beg)))

                   ;; Delimit on unchanged syntax-class at the end.
                   ((and (< end line-end) (eq (syntax-after end) (syntax-after (1- end))))
                    ;; This match doesn't end at delimiters, keep searching.
                    (goto-char (1+ beg)))

                   ;; Cycle word list!
                   (t
                    (let ((i 0)
                          (not-found t))
                      (while (and not-found (< i words-length))
                        (cond
                         ((match-string-no-properties (1+ i))
                          (setq i (1- i))
                          (setq not-found nil))
                         (t
                          (setq i (1+ i)))))

                      ;; Move the current word last.
                      (setq i (mod (1+ i) words-length))
                      (let ((word-orig (buffer-substring-no-properties beg end)))
                        ;; Match the case of the existing word (when case is folded).
                        (when arg-case-fold
                          (setq arg-words
                                (cond
                                 ((string-equal (upcase word-orig) word-orig)
                                  (mapcar (lambda (w) (upcase w)) arg-words))
                                 ((string-equal (downcase word-orig) word-orig)
                                  (mapcar (lambda (w) (downcase w)) arg-words))
                                 (t
                                  (mapcar
                                   (lambda (w) (upcase-initials (downcase w))) arg-words)))))

                        (setq arg-words
                              (append (seq-subseq arg-words i) (seq-subseq arg-words 0 i)))

                        ;; Include this literal word
                        ;; in case of minor differences in case or spacing.
                        (setq arg-words (cons word-orig (cdr arg-words)))

                        (unless (string-equal (downcase (car arg-words)) (downcase word-orig))
                          (error "Internal error"))))

                    (setq result (cons arg-words (cons beg end)))))))))))
      (setq cycle-data-index (1+ cycle-data-index)))

    (or result (cons nil nil))))

(defun cycle-at-point--impl-cycle-get-data-for-mode ()
  "Return data associated with a major mode."
  (declare (important-return-value t))
  (cond
   (cycle-at-point-list
    cycle-at-point-list)
   (t
    (let ((preset (cycle-at-point-preset nil t)))
      (unless preset
        ;; TODO: detect language.
        (setq preset (cycle-at-point-preset "lang-en")))
      preset))))

(defun cycle-at-point-impl (cycle-index fn-cache)
  "Cycle case styles using the choice at CYCLE-INDEX.
Argument FN-CACHE stores the result for reuse."
  (declare (important-return-value t))
  (pcase-let ((`(,result-choices ,word-beg ,word-end) (or fn-cache '(nil nil nil))))
    ;; Call when not cached.
    (unless result-choices
      (let ((cycle-data (cycle-at-point--impl-cycle-get-data-for-mode)))
        (cond
         ((null cycle-data)
          (message "No cycle for mode %S" major-mode))
         (t
          (pcase-let ((`(,words . (,beg . ,end)) (cycle-at-point--cycle-words cycle-data)))
            (cond
             ((null words)
              (message "No cycle symbol found at point!"))
             (t
              (let ((target (buffer-substring-no-properties beg end)))
                (cond
                 ((null target)
                  ;; Trim the string since it can contain newlines.
                  (message "No cycle for %S found!" (string-trim target)))
                 (t
                  (setq result-choices words)
                  (setq word-beg beg)
                  (setq word-end end))))))))))

      (when result-choices
        (setq fn-cache (list result-choices word-beg word-end))))

    (when result-choices
      (let ((word-at-index (nth (mod cycle-index (length result-choices)) result-choices)))
        (recomplete-replace-in-region word-at-index word-beg word-end))

      (list result-choices fn-cache))))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun cycle-at-point-preset (&optional preset-id quiet)
  "Load a preset for current mode.
The first is PRESET-ID to override the current `major-mode'.
The second is QUIET, when non-nil, don't show a message
when the preset isn't found."
  (declare (important-return-value t))
  (unless preset-id
    (setq preset-id
          (cond
           (cycle-at-point-preset-override
            cycle-at-point-preset-override)
           (t
            (symbol-name major-mode)))))
  (let ((preset-sym (intern (concat "cycle-at-point-preset-" preset-id))))
    (when (condition-case err
              (progn
                (require preset-sym)
                t)
            (error (unless quiet
                     (message "cycle-at-point: preset %S not found! (%S)" preset-id err))
                   nil))
      (funcall preset-sym))))

;;;###autoload
(defun cycle-at-point (arg)
  "Cycle through a list of known values.
ARG is the offset to cycle, default is 1, -1 to cycle backwards."
  (declare (important-return-value nil))
  (interactive "p")
  ;; Pass 1 to start at the second item (the current word is always the first).
  (recomplete-with-callback 'cycle-at-point-impl arg 1))

(provide 'cycle-at-point)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; cycle-at-point.el ends here
