;; -*- lexical-binding: t -*-

(require 'easy-mmode)
(require 'minimenu)

;; FIXME: handle cases when function is called directly from minibuffer
;; [2019-01-03 00:49]

;;;###autoload
(defmacro defcamper (-name -docstring -condition)
  "Returns a macro for creating context dependent minimenu
assisted functions.

-NAME is the name of the output macro. -DOCSTRING is the
docstring of the macro. -CONDITION is the condition that triggers
the ``minimenu-call''.

Returns a macro named _NAME with arguments NAME, DOCSTRING and
COLLECTION that returns an interactive function which can easily
be bound to a key. This function is of the form

  ((-CONDITION)
      (minimenu-call COLLECTION)
     (self-insert-command arg))

so that ``minimenu-call'' is only called when condition is
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


(provide 'camp)
