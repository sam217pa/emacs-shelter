;;; camp-utils.el --- define camp macros for dressing camp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: convenience

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

;;;; Require

(require 'tent)

;;;; Camper

(defsubst camp--rom-p ()
  "Return t if `buffer-read-only' is."
  (or buffer-read-only))

(defun camp--seconds (x)
  "Handles the second keyword.

(X must be a list of four elements.)"
  (pcase (elt x 1)
    ('reg '(region-active-p))
    ('rom '(camp--rom-p))
    (_ (elt x 1))))

(defsubst camp--fc (f)
  "Wraps (funcall F) in `ignore-errors' so that camp is mostly silent for functions like `forward-sexp'."
  (ignore-errors (funcall f)))

(defun camp--kwd (arg)
  "Handles camp action keywords.

ARG must be a list of four elements as returned by `camp'.

Three action keywords are recognized:
- CALL: funcall the next argument
- DO  : just execute the next argument.
- TENT: call `tent' on the next argument.
- CMD : call next argument interactively"
  (pcase (elt arg 2)
    ('call `(camp--fc ,(elt arg 3)))
    ('do    (elt arg 3))
    ('tent `(tent ,(elt arg 3)))
    ('cmd  `(call-interactively ,(elt arg 3)))
    (_ (error "Unrecognized camp keyword"))))

(defun camp--conds (arg)
  "Handles camp condition keywords.

ARG must be a list of four elements as returned by `camp'.

Three condition keywords are recognized. The actions elements are
executed only if:

- AT: point is `looking-at' next element (see also `camp-at')
- BK: point is `looking-back' next element (see also `camp-bk')
- IF: condition returns true. Condition can be shortened to REG
      if it checks for `region-active-p'."
  (or
   (pcase (elt arg 0)
     ('at `((camp-at ,(elt arg 1))
            ,(camp--kwd arg) t))
     ('bk `((camp-bk ,(elt arg 1))
            ,(camp--kwd arg) t))
     ('if `(,(camp--seconds arg)
            ,(camp--kwd arg) t)))
   (error "Unrecognized camp keyword")))

;;;###autoload
(defmacro camp (&rest args)
  "Dress camp at point.

Camp is a wrapper around `cond' that execute different actions in
different contexts, specified in ARGS . Its clauses can be
separated into 'conditions' and 'actions'. Actions are executed
if conditions are satisfied.

A typical camp call would be:

  (camp
   at \"xx\" do (message \"yes\")
   bk \"yy\" do (message \"no\")
   if (region-active-p) do (kill-ring-save) ; equivalent to:
   if reg do (kill-ring-save))

Three conditions can be checked: AT, BK and IF (see also
`camp--conds'). Three actions can be executed: DO, CALL and
TENT (see also `camp--kwd').

Each camp clauses _must_ include four elements."
  (declare (debug t) (indent 0))
  `(cond
    ,@(mapcar #'camp--conds (camp--group args 4))))

;;;###autoload
(defmacro defcamp (name docstring &rest args)
  "Define a command NAME documented by DOCSTRING that dress `camp' at ARGS."
  (declare (debug t) (doc-string 2) (indent defun))
  `(defun ,name (&optional arg)
     ,docstring
     (interactive "p")
     (or (camp ,@args)
         (self-insert-command arg))))

;;;; Helpers

;;;###autoload
(defsubst camp-at (re)
  "Return t if point is looking at RE.

Alias for `looking-at'."
  (looking-at re))

;;;###autoload
(defsubst camp-bk (re)
  "Return t if point is looking back at RE.

Search for RE to the character right before point.

Alias for `looking-back'."
  (looking-back re (1- (point))))

;;;###autoload
(defmacro camp-or (clause &rest body)
  "Equivalent to `or' that recursively wraps each CLAUSE of BODY
in `ignore-errors'."
  (declare (indent 0))
  (if (null body)
      `(ignore-errors ,clause)
    `(or (ignore-errors ,clause)
         (camp-or ,@body))))

(defmacro camp-stay (&rest body)
  "Evaluates BODY but leaves point at point.

Equivalent to `save-excursion'."
  `(save-excursion ,@body))

(defun camp--group (source n)
  "Divide SOURCE list in N groups and stack together the last
elements.
"
  (if (zerop n) (error "Zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;;; Keys

(defsubst camp--defkeys (keys map)
  "Helper for `define-key' that define KEYS in keymap MAP.

KEYS must be a list of the form (\"KEY\" . \"BINDING\") where KEY
is suitable to `define-key'."
  (cl-loop
   for key in keys
   do (define-key map (kbd (car key)) (cdr key))))

;;;###autoload
(defmacro camp-defkeymap (name docstring keys)
  "Define a new camp minor mode keymap for mode NAME.

NAME should not include \"-mode\". For instance, to define a new
camp keymap for org-mode, one would call (camp-defkeymap org
...).

DOCSTRING is the documentation of the keymap. KEYS are passed to
`define-key' by `camp--defkeys'."
  (declare (indent 1) (doc-string 2))
  (let ((kmp (intern (format "camp-%s-map" name))))
    `(progn
       (defvar ,kmp (make-sparse-keymap) ,docstring)
       (camp--defkeys ,keys ,kmp))))

(provide 'camp-utils)
;;; camp-utils.el ends here.
