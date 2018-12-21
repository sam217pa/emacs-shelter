;; -*- lexical-binding: t -*-

(require 'minimenu)

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

(defun minimenu--at-paren-p ()
  (looking-back "\)" (1- (point))))

(defmacro minimenu-lisp-defun (name doc col)
  (declare (debug t)
           (indent 0))
  `(defun ,name (&optional arg)
     ,doc
     (interactive "p")
     (if (minimenu--at-paren-p)
         (minimenu--call ,col)
       (self-insert-command arg))))

(minimenu-lisp-defun minimenu-lisp-nav "" minimenu-lisp-navigate)
(minimenu-lisp-defun minimenu-lisp-eval "" minimenu-lisp-actions)

(let ((keys '(("x" . minimenu-lisp-eval)
              ("f" . minimenu-lisp-nav))))
  (cl-loop
   for x in keys
   do (define-key emacs-lisp-mode-map (kbd (car x)) (cdr x))))


(provide 'minimenu-lisp)
