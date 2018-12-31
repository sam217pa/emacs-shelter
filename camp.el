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
  (declare (debug t) (indent defun))
  `(defmacro ,-name (name docstring collection)
     ,-docstring
       `(defun ,name (&optional arg)
          ,docstring
          (interactive "p")
          (if ,',-condition
              (minimenu--call ,collection)
            (self-insert-command arg)))))

(provide 'camp)
