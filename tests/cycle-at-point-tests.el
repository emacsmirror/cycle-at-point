;;; cycle-at-point-tests.el --- Cycle at point tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-2.0-or-later
;; Copyright (C) 2022  Campbell Barton

;;; Usage:

;; These tests are typically executed by a shell script, e.g:
;;
;;   ./tests/cycle-at-point-tests.sh
;;
;; If you wish to run this directly from the command line,
;; you may use the following command:
;;
;;    emacs -batch -l ../recomplete/recomplete.el -l tests/cycle-at-point-tests.el -f ert-run-tests-batch-and-exit
;;
;; TIP: to run tests on file change (on Linux).
;;
;;    bash -c 'while true; do inotifywait -e close_write $(find . -name "*.el") ; emacs -batch -l ../recomplete/recomplete.el -l tests/cycle-at-point-tests.el -f ert-run-tests-batch-and-exit ; done'
;;

;;; Code:


;; ---------------------------------------------------------------------------
;; Setup Environment

;; FIXME: The mere act of loading a file shouldn't cause such changes.
(setq python-indent-guess-indent-offset nil)

(add-to-list 'load-path (concat (file-name-directory load-file-name) ".."))
(require 'cycle-at-point)


;; ---------------------------------------------------------------------------
;; Test Macros

;; Simplify test declaration.
(defmacro ert-deftest-decl-pair (test-id text-initial text-expected char-index mode)
  "Create a test named TEST-ID using TEXT-INITIAL TEXT-EXPECTED as a result."
  `(ert-deftest ,test-id ()
     (with-temp-buffer
       (buffer-enable-undo)
       (funcall ,mode)
       (insert ,text-initial)
       (goto-char (+ (point-min) ,char-index))
       (call-interactively 'cycle-at-point)
       (should (equal ,text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Test (Basic)
;;
;; Tests that should meet an expected output.

;; Single words.
(ert-deftest-decl-pair py-boolean "True" "False" 0 'python-mode)
(ert-deftest-decl-pair c-boolean "true" "false" 0 'c-mode)
(ert-deftest-decl-pair c++-boolean "true" "false" 0 'c++-mode)
(ert-deftest-decl-pair emacs-boolean "nil" "t" 0 'emacs-lisp-mode)
(ert-deftest-decl-pair text-day-upper "WEDNESDAY" "THURSDAY" 0 'text-mode)
(ert-deftest-decl-pair text-day-lower "friday" "saturday" 0 'text-mode)
(ert-deftest-decl-pair text-day-mixed-case "Saturday" "Sunday" 0 'text-mode)
(ert-deftest-decl-pair py-is-isnot "is" "is not" 0 'python-mode)
(ert-deftest-decl-pair py-isnot-is "is not" "is" 0 'python-mode)

;; ---------------------------------------------------------------------------
;; Test (Complex)
;;
;; More complex words.

(ert-deftest-decl-pair py-is-isnot-complex "if A is B:" "if A is not B:" 5 'python-mode)
(ert-deftest-decl-pair c-and "(A)&&(B)" "(A)||(B)" 3 'c-mode)
(ert-deftest-decl-pair c-less-equal "(A)>=(B)" "(A)<=(B)" 3 'c-mode)
(ert-deftest-decl-pair c-equals "(A)!=(B)" "(A)==(B)" 3 'c-mode)
(ert-deftest-decl-pair c-greater "A>B" "A<B" 1 'c-mode)

(ert-deftest-decl-pair py-number-decimal-to-nex "65536" "0x10000" 0 'python-mode)
(ert-deftest-decl-pair py-number-binary-to-octal "0b101010" "0o52" 0 'python-mode)

(provide 'cycle-at-point-tests)
;;; cycle-at-point-tests.el ends here
