;; -*- lexical-binding: t -*-

(require 'minimenu)
(require 'camp)

(defcustom minimenu-lisp-actions
  '(("b" (:fun eval-buffer    :desc "whole buffer"))
    ("s" (:fun eval-last-sexp :desc "last sexp"))
    ("i" (:fun indent-region  :desc "indent region"))
    ("c" (:fun byte-compile-file :desc "byte compile")))
  "Standard actions defined for lisp"
  :group 'minimenu-lisp
  :type 'alist)

(defcustom minimenu-lisp-navigate
  '(("b" (:fun backward-sexp  :desc "back sexp"))
    ("f" (:fun forward-sexp   :desc "fwd  sexp"))
    ("p" (:fun backward-list  :desc "back list"))
    ("n" (:fun forward-list   :desc "fwd  list")))
  "Standard navigations command for lisp"
  :group 'minimenu-lisp
  :type 'alist)

(defun minimenu-lisp--at-paren-p ()
  "Returns t if point is after a closing paren."
  (looking-back "\)" (1- (point))))

(camp-defmacro minimenu-lisp-defun-paren
  "Define minimenu function for lisp when point is after
a closing paren."
  (minimenu-lisp--at-paren-p))

(minimenu-lisp-defun-paren minimenu-lisp-nav
  "Action for navigating in emacs lisp."
  minimenu-lisp-navigate)

(minimenu-lisp-defun-paren minimenu-lisp-eval
  "Actions for evaluating in emacs lisp."
  minimenu-lisp-actions)

;;;###autoload
(minimenu-define-minor-mode emacs-lisp
  "Minimenu minor-mode for emacs-lisp"
  '(("x" . minimenu-lisp-eval)
    ("f" . minimenu-lisp-nav)))

(provide 'minimenu-lisp)
