;;; shelter-lisp.el --- shelter commands for lisp and emacs lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: convenience, lisp

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

(require 'camp-utils)
(require 'fort)

(defsubst camp--at-paren-p ()
  "Returns t if point is after a closing paren."
  (or (camp-bk "\)") (camp-at "\(")))

(defcamp camp-lisp-mark
  "Mark sexp at point"
  at "\(" do (mark-sexp  1)
  bk "\)" do (mark-sexp -1))

;;; Emacs lisp

(defcamp camp-emacs-lisp-next
  "Next semantic element in emacs lisp"
  bk "\)" do (camp-or (forward-list) (up-list))
  at "\(" do (camp-or (progn (forward-list 2) (backward-sexp))
                      (up-list)))

(defcamp camp-emacs-lisp-prev
  "Previous semantic element in emacs lisp"
  at "\(" do (camp-or (backward-list) (up-list -1))
  bk "\)" do (camp-or (progn (backward-list 2) (forward-sexp))
                      (up-list -1)))

(defcamp camp-emacs-lisp-fwd
  "Forward semantic element in emacs lisp"
  if (camp--at-paren-p) do (camp-or (forward-sexp)))

(defcamp camp-emacs-lisp-bwd
  "Backward semantic element in emacs lisp"
  if (camp--at-paren-p) do (camp-or (backward-sexp)))

(defcamp camp-emacs-lisp-navigate
  "Actions for navigating in emacs lisp."
  if (camp--at-paren-p)
  tent '(("b" backward-sexp "back sexp")
         ("f" forward-sexp  "fwd  sexp")
         ("p" backward-list "back list")
         ("n" forward-list  "fwd  list")))

(defcamp camp-emacs-lisp-eval
  "Actions for evaluating in emacs lisp."
  at "\(" do (camp-or (camp-stay (forward-sexp)
                                 (call-interactively #'eval-last-sexp)))
  if (camp--at-paren-p)
  tent '(("b" eval-buffer "buffer")
         ("e" eval-last-sexp "last sexp")
         ("E" pp-eval-last-sexp "pretty last sexp")
         ("x" eval-defun "defun")
         ("m" pp-macroexpand-last-sexp "macro expand")))

(defcamp camp-emacs-lisp-exec
  "Execute stuffs in emacs lisp"
  if (camp--at-paren-p) do (fort))

(camp-define-keys
 :map emacs-lisp
 :simple
 ("e" 'camp-emacs-lisp-eval
  "n" 'camp-emacs-lisp-next
  "p" 'camp-emacs-lisp-prev
  "b" 'camp-emacs-lisp-bwd
  "f" 'camp-emacs-lisp-fwd
  "x" 'camp-emacs-lisp-exec
  "m" 'camp-lisp-mark))

(fort-define-keys
 :map emacs-lisp
 :simple
  ("b" 'eval-buffer
   "B" 'byte-compile-file
   "t" 'next-line
   "s" 'previous-line
   "r" 'forward-char
   "c" 'backward-char))

;;; Common Lisp

(defcamp camp-lisp-eval
  "Camp evaluation for common lisp"
  if (camp--at-paren-p)
  tent '(("e" slime-eval-last-expression "sexp")
         ("m" slime-macroexpand-1 "expand-1")))

(camp-define-keys
 :map lisp
 :simple
  ("x" 'camp-lisp-eval))

;;; Scheme

(defcamp camp-scheme-eval
  "Evaluation for scheme."
  at "\(" do (camp-or
               (camp-stay
                (forward-sexp)
                (call-interactively #'geiser-eval-last-sexp)))
  if (camp--at-paren-p)
  tent '(("b" geiser-eval-buffer "buffer")
         ("e" geiser-eval-last-sexp "last sexp")
         ("x" geiser-eval-definition "defun")
         ("m" geiser-expand-last-sexp "macro expand")))


(camp-define-keys
 :map scheme
 :simple
  ("n" 'camp-emacs-lisp-next
   "p" 'camp-emacs-lisp-prev
   "b" 'camp-emacs-lisp-bwd
   "f" 'camp-emacs-lisp-fwd
   "e" 'camp-scheme-eval))

(provide 'shelter-lisp)
;;; shelter-lisp.el ends here.
