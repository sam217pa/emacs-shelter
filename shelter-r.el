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

;; This file provides commands for editing R code using the `ess'
;; emacs extension. The idea is to be able to edit R code as you would
;; normally, but that otherwise self-inserting commands have behaviors
;; that depend on the local context around point. For instance, the
;; _e_ key could be used to evaluate the previous S-expression after
;; a closing paren, to evaluate the preceding function definition
;; after a closing curly bracket, to evaluate the paragraph at an
;; empty line, and so on.

;;; Code:

;; TODO: [2019-02-11 21:56] add autoload cookies, but where?

(require 'cl-lib)
(require 'thingatpt)
(require 'eap)
(require 'shelter-utils)
(require 'camp)
(require 'fort)
(require 'tent)

;;;; Silence byte-compiler

(defmacro shelter-r--declare-functions (&rest funs)
  "Silence the byte-compiler."
  (declare (indent 0))
  `(progn ,@(mapcar (lambda (x) `(declare-function ,x "ess-site")) funs)))

(shelter-r--declare-functions ess-get-process-buffer
  ess-switch-to-ESS ess-mark-function-or-para ess-send-string
  ess-eval-region ess-roxy-toggle-roxy-region ess-execute
  ess-goto-beginning-of-function-or-para)

(defvar ess-current-process-name nil)

;;;; Customisation

(defgroup shelter-r nil
  "Shelter commands for editing R source code"
  :prefix "shelter-r-"
  :group 'shelter )

;; FIXME: implement default evaluation style.
(defcustom shelter-r-eval-style 'stay
  "Style of evaluation for R buffers."
  :type '(choice
          (const :tag "Stay at point" stay)
          (const :tag "Step to next element" step)
          (const :tag "Go to buffer" go))
  :group 'shelter-r)

(defvar shelter-r-opening-delim
  "(\\|{\\|\\["
  "Ess opening delimiters")

(defvar shelter-r-closing-delim
  ")\\|}\\|\\]\\|[A-Za-Z\\\"]\\\""
  "Ess closing delimiters")

(defun shelter-r--bwd-list ( )
  "Move backward list element conservatively"
  (ignore-errors
    (backward-list 2)
    (forward-list)))

(defmacro shelter-r--with-proc (proc &rest body)
  "Execute BODY in an environment where PROC is bound to
`ess-current-process-name'."
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

(defun shelter-r--get-function-name ()
  (camp-stay
   (backward-list)
   (shelter-r--keep end
     (backward-sexp)
     (buffer-substring-no-properties (point) end))))

(defun shelter-r--help ()
  (ess-help (shelter-r--get-function-name)))

(defun shelter-r--eval-sexp ()
  "Eval sexp before point."
  (eap
   (shelter-r--keep end
     (backward-list) (backward-sexp)
     (shelter-r--eval-region (point) end))))

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

(defmacro shelter-cmd (&rest args)
  `(lambda () (interactive)
     ,@(mapcar
        (lambda (f) `(funcall ,f))
        args)))

(defmacro shelter-r--iess-buf (&rest body)
  "Eval BODY with current buffer bound to current ess process
buffer."
  `(with-current-buffer (ess-get-process-buffer)
    ,@body))

(defmacro shelter-r--execute (cmd &optional msg)
  `(ess-execute ,cmd 'buffer nil ,msg))

(defmacro shelter-r--exe-defuns (&rest bindings)
  `(progn
     ,@(mapcar
        (lambda (bdg)
          `(defun ,(elt bdg 0) ()
             ,(elt bdg 3)
             (interactive)
             (shelter-r--execute
              ,(elt bdg 1)
              ,(elt bdg 2))))
        bindings)))

(defun shelter-r--render ()
  "Render the current file using rmarkdown::render"
  (interactive)

  (let* ((fn   (buffer-file-name))
         (rprt (concat (file-name-directory fn) "/reports/"))
         (figs (concat (file-name-directory fn) "/figures/"))
         (pref (file-name-base fn))
         (cmd  (format "rmarkdown::render(\"%s\", output_file=\"reports/%s.pdf\")"
                       fn pref)))
    (unless (file-exists-p rprt) (mkdir rprt))
    (unless (file-exists-p figs) (mkdir figs))
    (shelter-r--execute cmd "render")))

(defvar-local shelter-r--fig-count 0
  "Counter of fig in current R buffer.")

(defun shelter-r--fig (caption)
  (interactive "sFig Caption: ")
  (cl-incf shelter-r--fig-count)
  (let ((idx shelter-r--fig-count))
    (if (string-empty-p caption)
        (insert
         (format "#+ chunk%s, cache=TRUE" idx))
      (let ((cap (format "\\\\label{fig:fig%s}%s" idx caption)))
        (insert
         (format "#+ fig%s, fig.cap=\"%s\"" idx cap))))))

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

;;;; WIP editing
;;
;; WIP regions are regions of a buffer surrounded by /* */, as
;; described in the spin documentation
;; <https://github.com/yihui/knitr/blob/master/inst/examples/knitr-spin.R>.
;; They can be used to wrap a region that is to be removed later or
;; a just a simple draft or attempt at something.

;; TODO: define command to fold wip (hideshow maybe?)

(defun shelter-r--wip-get ()
  "Return the beginning and end point of the work-in-progress
cookies surrounding point."
  (camp-stay
   (list
    (re-search-backward "^## \/\\*")
    (re-search-forward "^## .+\\*\/"))))

(defmacro shelter-r--wip-apply (cmd)
  "Apply CMD to the work-in-progress around point."
  `(apply ,cmd (shelter-r--wip-get)))

(defun shelter-r-wip-delete ()
  "Delete the work-in-progress around point."
  (interactive)
  (shelter-r--wip-apply #'kill-region))

(defun shelter-r-wip-eval ()
  "Evaluate the whole work-in-progress block around point."
  (interactive)
  (shelter-r--wip-apply #'shelter-r--eval-region))

(defun shelter-r-wip-comment ()
  "Comment out the work-in-progress around point."
  (interactive)
  (shelter-r--wip-apply #'comment-region))

(defun shelter-r-wip-narrow ()
  "Narrow buffer to current work-in-progress."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (shelter-r--wip-apply #'narrow-to-region)))

(defun shelter-r--act (key &optional arg)
  "Call the function bound to KEY with ARG as arguments."
  (funcall (key-binding (kbd key)) arg))

;;;; Evaluation

(defun shelter-r--eval-defun ()
  "Evaluate function definition or paragraph in R code.

When point precedes the %>% magrittr pipe or a ggplot '+'
overloaded operator, this functions evaluates only from beginning
of paragraph (often the beginning of pipeline) to point."
  (interactive)
  (if (and (camp-at " %>%$\\| \\+$")
           (camp-bk shelter-r-closing-delim))
      (eap
       (shelter-r--keep end
         (ess-goto-beginning-of-function-or-para)
         (shelter-r--eval-region (point) end)))
    (eap-wrap #'ess-eval-function-or-paragraph)))

(defsubst shelter--botapt (thing)
  "Bound of THING at point."
  (let ((botapt (bounds-of-thing-at-point thing)))
    (list (car botapt) (cdr botapt))))

(defsubst shelter-r--enparen (&optional arg)
  "Insert parenthesis around stuffs."
  (let ((parens-require-spaces nil))
    (camp-or (insert-parentheses arg))))

(cl-symbol-macrolet
    ((opdel shelter-r-opening-delim)
     (cldel shelter-r-closing-delim)
     (outln outline-regexp))

  (camp-defkey "e" ess
    "Evaluate R semantic blocks in the corresponding buffer."
    if reg   cmd 'eap-eval-region
    bk "\}"  do  (shelter-r--eval-defun)
    bk cldel do  (shelter-r--eval-sexp)
    at outln do  (shelter-r--eval-outline))

  (camp-defkey "p" ess
    "Evaluate functions or paragraphs.

See `shelter-r--eval-defun'"
    bk ","   burn (call-interactively #'outline-previous-visible-heading)
    bk cldel cmd 'shelter-r--eval-defun)

  (camp-defkey "x" ess
    "Execute R commands in a `tent'."
    at "#'"  pause
    ((pause-prompt "RENDER: [r]ender")
     "r" (shelter-r--render))
    bk cldel tent
    '(("s" ess-set-working-directory "setwd")
      ("g" shelter-r--getwd "getwd")
      ("w" ess-execute-screen-options "set width")
      ("W" shelter-r--full-width "full width")
      ("l" shelter-r--list-files "list files")))

  (camp-defkey "R" ess
    "Commands bound to R."
    if reg do (call-interactively #'ess-eval-region)
    bk cldel tent
    '(("u" ess-roxy-update-entry "update")
      ("r" shelter-r-roxify "roxify")
      ("h" ess-roxy-hide-all "hide")))

  (camp-defkey "s" ess
    "Up movement in R source code."
    bk ","   burn (save-buffer)
    at outln cmd 'outline-previous-visible-heading
    at "^#'" do  (re-search-backward "^#'" nil t)
    bk cldel do  (shelter-r--bwd-list))

  (camp-defkey "t" ess
    "Down movements in R source code."
    at outln cmd 'outline-next-visible-heading
    at "^#'" do  (progn (re-search-forward "^#'" nil t 2) (beginning-of-line))
    bk cldel do  (ignore-errors (forward-list)))

  (camp-defkey "r" ess
    "Right movements in R source code."
    if reg   cmd 'eap-eval-region
    bk cldel do (up-list)
    at outln do (outline-show-subtree))

  (camp-defkey "c" ess
    "Left movements in R source code."
    bk ","   burn (shelter-r--iess-buf (comint-interrupt-subjob))
    bk cldel cmd 'backward-sexp
    at outln cmd 'outline-hide-subtree)

  (camp-defkey "k" ess
    "Kill actions in R source code."
    at "#" do (shelter-r--act "C-k")
    bk cldel do (apply #'kill-region (shelter--botapt 'paragraph)))

  (camp-defkey "z" ess
    "Switch to buffer"
    bk "," burn (ess-switch-to-ESS t))

  (camp-defkey "w" ess
    "Work in progress cookies related editing."
    bk cldel pause ((pause-prompt "WIP:\n[k]ill / [e]val / [c]omment / [n]arrow")
                    "k" (shelter-r-wip-delete)
                    "e" (shelter-r-wip-eval)
                    "c" (shelter-r-wip-comment)
                    "n" (shelter-r-wip-narrow)))

  (camp-defkey "n" ess
    "Actions bound to the n key in `camp-ess-map'."
    at outln cmd 'outline-narrow-to-subtree
    bk "\{" do (if (buffer-narrowed-p)
                   (widen)
                 (let ((begend (bounds-of-thing-at-point 'sexp)))
                   (narrow-to-region (1+ (car begend))
                                     (1- (cdr begend))))))

  (camp-defkey "m" ess
    "Actions bound to the m key in `camp-ess-map'."
    at outln cmd 'outline-mark-subtree
    if (or (camp-at "\{") (camp-bk "\}"))
    do (progn (ess-mark-function-or-para)
            (exchange-point-and-mark)))

  (camp-defkey "i" ess
    "Actions bound to the i key in `camp-ess-map'.

Related to indentation mostly."
    if reg cmd 'indent-region
    bk cldel do (camp-stay
                 (ess-mark-function-or-para)
                 (call-interactively 'indent-region)))

  (camp-defkey "d" ess
    "Actions bound to the d key in `camp-ess-map'."
    bk "," burn (ess-rdired))

  (camp-defkey "l" ess
    "Actions bound to the l key in `camp-ess-map'."
    bk "\)\\|\]" do (eap-wrap #'ess-eval-line))

  (camp-defkey "(" ess
    "Actions bound to the paren key in `camp-ess-map'."
    btwn ("[[:alpha:]]" "[[:alpha:]]")
    do (shelter-r--enparen 1))

  (camp-defkey "'" ess
    "Default insertions of quotes for roxygen comments."
    bk "," burn (shelter-r--act "M-x")
    if (looking-back "## " (point-at-bol))
    do (progn (delete-char -2) (insert "' ")))

  (camp-defkey "+" ess
    "Default insertions of fig for spin scripts."
    if (looking-back "^## " (point-at-bol))
    do (progn (delete-char -3)
              (call-interactively #'shelter-r--fig)))

  (camp-defkey "h" ess
    "Actions bound to the h key in `camp-ess-map'."
    bk "," burn (call-interactively #'ess-display-help-on-object)
    bk "\)" do (shelter-r--help))

  (camp-defkey "*" ess
    "Actions bound to the * key in `camp-ess-map'."
    bk "," burn (call-interactively #'outline-insert-heading))) ;cl-symbol-macrolet

(define-key camp-ess-map (kbd "C-<return>") #'eap-control-enter)

(fort-define-keys
 :map ess
 :simple
  ("d" 'ess-rdired
   "s" 'ess-switch-process
   "SPC s" 'save-buffer))

(provide 'shelter-r)
;;; shelter-R.el ends here
