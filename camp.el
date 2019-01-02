;; -*- lexical-binding: t -*-

(require 'minimenu)

;;;###autoload
(defmacro camp-defmacro (-name -docstring -condition)
  "Returns a macro for creating context dependent minimenu
assisted functions.

-NAME is the name of the output macro. -DOCSTRING is the
docstring of the macro. -CONDITION is the condition that triggers
the ``minimenu--call''.

Returns a macro named _NAME with arguments NAME, DOCSTRING and
COLLECTION that returns an interactive function which can easily
be bound to a key. This function is of the form

  (if (-CONDITION)
      (minimenu--call COLLECTION)
     (self-insert-command arg))

so that ``minimenu--call'' is only called when condition is
satisfied, otherwise the key just self-insert."
  (declare (debug t) (indent defun) (doc-string 2))
  `(defmacro ,-name (name docstring collection)
     (declare (debug t) (indent defun) (doc-string 2))
     ,-docstring
       `(defun ,name (&optional arg)
          ,docstring
          (interactive "p")
          (if ,',-condition
              (minimenu-call ,collection)
            (self-insert-command arg)))))

(defun camp--define-keys (keymap keys)
  "Short helper that define KEYS in keymap.

Keys should be a proper cons list of the given form:
'((\"a\" . function-1)
  (\"b\" . function-2))"
  (cl-loop
   for x in keys
   do (define-key keymap (kbd (car x)) (cdr x))))

;;; Minor mode definer

(defmacro camp-define-mode (name docstring keys)
  "Helper for defining a minimenu-assisted minor mode.

NAME is the name of the mode that is to be assisted.

DOCSTRING is the documentation string of the minor mode.

KEYS is a cons list of key-function association that constitutes
the camp minor mode."
  (declare (debug t) (doc-string 2) (indent 1))
  (let
      ((mm-minor-mode (intern (format "camp-%s-minor-mode" name)))
       (mm-keymap (intern (format "camp-%s-mode-map" name)))
       (mm-group (intern (format "camp-%s" name)))
       (mm-lighter " mm"))
    `(progn
       (defvar ,mm-keymap (make-sparse-keymap))
       (camp--define-keys ,mm-keymap ,keys)
       (define-minor-mode ,mm-minor-mode
         ,docstring
         :keymap ,mm-keymap
         :group ,mm-group
         :lighter ,mm-lighter))))


(provide 'camp)
