;; -*- lexical-binding: t -*-

(require 'camp)

(defun camp--at-paren-p ()
  "Returns t if point is after a closing paren."
  (looking-back "\)" (1- (point))))

(camp-defmacro camp-defun-at-paren
  "Define minimenu function for lisp when point is after
a closing paren."
  (camp--at-paren-p))

(camp-defun-at-paren camp-emacs-lisp-navigate
  "Actions for navigating in emacs lisp."
  '(("b" 'backward-sexp "back sexp")
    ("f" 'forward-sexp  "fwd  sexp")
    ("p" 'backward-list "back list")
    ("n" 'forward-list  "fwd  list")))

(camp-defun-at-paren camp-emacs-lisp-eval
  "Actions for evaluating in emacs lisp."
  '(("b" 'eval-buffer        "whole buffer")
    ("s" 'eval-last-sexp     "last sexp")
    ("i" 'indent-region      "indent region")
    ("c" 'byte-compile-file  "byte compile")))

;;;###autoload
(minimenu-define-minor-mode emacs-lisp
  "Minimenu minor-mode for emacs-lisp"
  '(("x" . camp-emacs-lisp-eval)
    ("f" . camp-emacs-lisp-navigate)))

(camp-defun-at-paren minimenu-lisp-eval
  "Minimenu evaluation for common lisp"
  '(("e" 'slime-eval-last-expression "sexp")
    ("m" 'slime-macroexpand-1        "expand-1")))

;;;###autoload
(minimenu-define-minor-mode common-lisp
  "Minimenu minor mode for common lisp"
  '(("x" . minimenu-lisp-eval)))

(provide 'minimenu-lisp)
