;; -*- lexical-binding: t -*-

(require 'minimenu)
(require 'easy-mmode)

;; TODO: write docstring for camp macros [2019-01-03 16:57]
;; FIXME: handle cases when function is called directly from minibuffer [2019-01-03 00:49]

;;; Camper

(defun camp--region-p (x)
  (if (eql (elt x 1) 'reg)
      '(region-active-p)
    (elt x 1)))

(defsubst camp--fc (f)
  (ignore-errors (funcall f)))

(defun camp--kwd (arg)
  (or
   (cl-case (elt arg 2)
     ('call `(camp--fc ,(elt arg 3)))
     ('do    (elt arg 3))
     ('mm   `(minimenu-call ,(elt arg 3))))
   (error "Unrecognized camp keyword.")))

(defun camp--conds (arg)
  (or
   (cl-case (elt arg 0)
     ('at `((camp-at ,(elt arg 1))
            ,(camp--kwd arg) t))
     ('bk `((camp-bk ,(elt arg 1))
            ,(camp--kwd arg) t))
     ('if `(,(camp--region-p arg)
            ,(camp--kwd arg) t)))
   (error "Unrecognized camp keyword.")))

;;;###autoload
(defmacro camp (&rest args)
  (declare (debug t) (indent 0))
  `(cond
   ,@(cl-loop
      for a in (camp--group args 4)
      collect (camp--conds a))))

;;;###autoload
(defmacro defcamp (name docstring &rest args)
  (declare (debug t) (doc-string 2) (indent defun))
  `(defun ,name (&optional arg)
     ,docstring
     (interactive "p")
     (or (camp ,@args)
         (self-insert-command arg))))

;; (defun camp--beyond ()
;;  camp-minor-mode
;;  this-single-command)

;;; Helpers

(defsubst camp-at (re) (looking-at re))
(defsubst camp-bk (re) (looking-back re (1- (point))))

;;; Minor mode definer

(defmacro camp-define-mode (name docstring keys)
  "Helper for defining a minimenu-assisted minor mode.

NAME is the name of the mode that is to be assisted.

DOCSTRING is the documentation string of the minor mode.

KEYS is a cons list of key-function association that constitutes
the camp minor mode."
  (declare (debug t) (doc-string 2) (indent 1))
  (let
      ((mm-minor-mode (make-symbol (format "camp-%s-minor-mode" name)))
       (mm-keymap (make-symbol (format "camp-%s-mode-map" name)))
       (mm-group (make-symbol (format "camp-%s" name)))
       (mm-lighter " mm"))
    `(progn
       (defvar ,mm-keymap (make-sparse-keymap))
       (easy-mmode-define-keymap ,keys ,mm-keymap)
       (define-minor-mode ,mm-minor-mode
         ,docstring
         :keymap ,mm-keymap
         :group ,mm-group
         :lighter ,mm-lighter))))

(defun camp--group (source n)
  (if (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defsubst camp--defkeys (keys map)
  (cl-loop
   for key in keys
   do (define-key map (kbd (car key)) (cdr key))))

(defmacro camp-defkeymap (name docstring keys)
  (declare (indent 1) (doc-string 2))
  (let ((kmp (make-symbol (format "camp-%s-map" name))))
    `(progn
       (defvar ,kmp (make-sparse-keymap) ,docstring)
       (camp--defkeys ,keys ,kmp))))


(provide 'camp-utils)
