;; -*- lexical-binding: t -*-

(require 'camp-utils)

(defsubst camp--at-paren-p ()
  "Returns t if point is after a closing paren."
  (or (camp-bk "\)") (camp-at "\(")))

;;; Emacs lisp

(defcamp camp-emacs-lisp-navigate
  "Actions for navigating in emacs lisp."
  if (camp--at-paren-p)
  mm '(("b" 'backward-sexp "back sexp")
       ("f" 'forward-sexp  "fwd  sexp")
       ("p" 'backward-list "back list")
       ("n" 'forward-list  "fwd  list")))

(defcamp camp-emacs-lisp-eval
  "Actions for evaluating in emacs lisp."
  if (camp--at-paren-p)
  mm '(("b" 'eval-buffer        "whole buffer")
       ("s" 'eval-last-sexp     "last sexp")
       ("i" 'indent-region      "indent region")
       ("c" 'byte-compile-file  "byte compile")))

(camp-defkeymap emacs-lisp
  "Camp keymap for emacs lisp"
  '(("x" . camp-emacs-lisp-eval)
    ("f" . camp-emacs-lisp-navigate)))

;;; Common Lisp

(defcamp camp-lisp-eval
  "Minimenu evaluation for common lisp"
  if (camp--at-paren-p)
  mm '(("e" 'slime-eval-last-expression "sexp")
       ("m" 'slime-macroexpand-1 "expand-1")))

(camp-defkeymap lisp
  "Camp keymap for common lisp"
  '(("x" . camp-lisp-eval)))

(provide 'camp-lisp)
