;;; shelter-R.el --- shelter command for editing  R code  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: shelter, ess, Rstat, R

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:



(require 'cl-lib)

(require 'shelter-utils)

(require 'camp)
(require 'fort)
(require 'tent)

(defgroup shelter-r nil
  "Shelter commands for editing R source code"
  :prefix "shelter-r-"
  :group 'shelter )

(defcustom shelter-r-eval-style 'stay
  "Style of evaluation for R buffers."
  :type '(choice
          (const :tag "Stay at point" stay)
          (const :tag "Step to next element" step)
          (const :tag "Go to buffer" go))
  :group 'shelter-r)

(defvar shelter-r-opening-delim
  "(\\|{\\|\\["
  "ess opening delimiter")

(defvar shelter-r-closing-delim
  ")\\|}\\|\\]\\|[A-Za-Z\\\"]\\\""
  "ess closing delimiter")

(defun shelter-r--bwd-list ( )
  "Move backward list element conservatively"
  (ignore-errors
    (backward-list 2)
    (forward-list)))

(defmacro shelter-r--with-proc (proc &rest body)
  "Execute BODY in an environment where"
  (declare (indent 1))
  `(let ((,proc (get-process ess-current-process-name)))
     ,@body))

(defmacro shelter-r--keep (p &rest body)
  "Evaluate BODY with point before body is evaluated bound to P."
  (declare (indent 1))
  `(let ((,p (point)))
     (camp-stay ,@body)))

(defun shelter-r--send-string (str)
  "Send STR to the current R process."
  (shelter-r--with-proc pr
    (ess-send-string pr str)))

(defun shelter-r--eval-sexp ()
  "Eval sexp before point."
  (shelter-r--keep end
    (backward-list) (backward-sexp)
    (ess-eval-region (point) end nil nil)))

(defsubst shelter-r--eval-region (beg end)
  "Wrapper around `ess-eval-region'"
  (ess-eval-region beg end nil nil))

(defun shelter-r--eval-outline ()
  "Evaluate current outline subtree."
  (shelter-r--keep beg
    (outline-end-of-subtree)
    (shelter-r--eval-region beg (point))))

;;;###autoload
(defun shelter-r-roxify ()
  "Call `ess-roxy-toggle-roxy-region' on region."
  (interactive)
  (camp-stay
    (backward-sentence)
    (shelter-r--keep beg
      (forward-sentence)
      (ess-roxy-toggle-roxy-region beg (point)))))

;; TODO: define command to select and comment wip
;; TODO: define command to fold wip (hideshow maybe?)
;; (defun shelter-r--wip-comment (beg end)
;;   (interactive "r")
;;   ())


(defmacro shelter-cmd (&rest args)
  `(lambda () (interactive)
     ,@(mapcar
        (lambda (f) `(funcall ,f))
        args)))

(defmacro shelter-r--execute (cmd &optional msg)
  `(ess-execute ,cmd 'buffer nil ,msg))

(defmacro shelter-r--exe-defuns (&rest bindings)
  `(progn
     ,@(mapcar
        (lambda (bdg)
          `(defun ,(elt bdg 0) ()
             ,(elt bdg 3)
             (shelter-r--execute
              ,(elt bdg 1)
              ,(elt bdg 2))))
        bindings)))

(shelter-r--exe-defuns
 (shelter-r--full-width
  "options(width=10000, length=99999)" "> full width"
  "Set full terminal width.")
 (shelter-r--list-files
  "list.files()" "> ls"
  "List files in current directory.")
 (shelter-r--getwd
  "getwd()" "> getwd"
  "Get current R working directory"))


(defun shelter-r--wip-get ()
  (camp-stay
   (list
    (re-search-backward "^## \/\\*")
    (re-search-forward "^## .+\\*\/"))))

(defmacro shelter-r--wip-apply (cmd)
  `(apply ,cmd (shelter-r--wip-get)))

(defun shelter-r-wip-delete ()
  (interactive)
  (shelter-r--wip-apply #'kill-region))

(defun shelter-r-wip-eval ()
  (interactive)
  (shelter-r--wip-apply #'shelter-r--eval-region))

(defun shelter-r-wip-comment ()
  (interactive)
  (shelter-r--wip-apply #'comment-region))


(cl-symbol-macrolet
    ((opdel shelter-r-opening-delim)
     (cldel shelter-r-closing-delim)
     (outln outline-regexp))

  (defcamp shelter-r-eval
    "Evaluate R semantic blocks into the corresponding buffer."
    if reg   cmd 'ess-eval-region
    at cldel do  (shelter-r--eval-sexp)
    at outln do  (shelter-r--eval-outline))

  (defcamp shelter-r-eval-defun
    "Evaluate functions or paragraphs."
    at cldel cmd 'ess-eval-function-or-paragraph)

  (defcamp shelter-r-execute
    "Execute R commands"
    bk cldel tent
    '(("s" ess-set-working-directory "setwd")
      ("g" (shelter-cmd 'shelter-r--getwd) "getwd")
      ("w" ess-execute-screen-options "set width")
      ("W" (shelter-cmd 'shelter-r--full-width) "full width")
      ("l" (shelter-cmd 'shelter-r--list-files) "list files")))

  (defcamp shelter-r-roxygen
    "Roxygen related commands"
    if reg do (call-interactively #'ess-eval-region)
    bk cldel tent
    '(("u" ess-roxy-update-entry "update")
      ("r" shelter-r-roxify "roxify")
      ("h" ess-roxy-hide-all "hide")))

  (defcamp shelter-r-up
    "Up movement in R source code."
    at outln cmd 'outline-previous-visible-heading
    at "^#'" do  (re-search-backward "^#'" nil t)
    bk cldel do  (shelter-r--bwd-list))

  (defcamp shelter-r-down
    "Down movements in R source code."
    at outln cmd 'outline-next-visible-heading
    at "^#'" do  (progn (re-search-forward "^#'" nil t 2) (beginning-of-line))
    bk cldel do  (ignore-errors (forward-list)))

  (defcamp shelter-r-right
    "Right movements in R source code."
    bk cldel do (up-list)
    at outln do (outline-show-subtree))

  (defcamp shelter-r-left
    "Left movements in R source code."
    bk cldel cmd 'backward-sexp
    at outln cmd 'outline-hide-subtree)

  (defcamp shelter-r-wip
    "Work in progress cookies related editing."
    bk cldel tent '(("k" shelter-r-wip-delete "kill")
                    ("e" shelter-r-wip-eval   "eval")
                    ("c" shelter-r-wip-comment "comment")))) ;cl-symbol-macrolet

(fort-define-keys
 :map ess
 :simple
  ("d" 'ess-rdired
   "s" 'ess-switch-process
   "SPC s" 'save-buffer))

(camp-define-keys
 :map ess
 :simple
  ("s" 'shelter-r-up
   "t" 'shelter-r-down
   "r" 'lesspy-right
   "c" 'lesspy-left
   "e" 'shelter-r-eval
   "p" 'shelter-r-eval-defun
   "x" 'shelter-r-execute
   "R" 'shelter-r-roxygen
   "w" 'shelter-r-wip))


(provide 'shelter-R)
;;; shelter-R.el ends here
