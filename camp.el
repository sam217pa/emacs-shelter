;;; camp.el --- dress camp commands -*- lexical-binding: t; -*-

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

(require 'camp-utils)
(require 'aeolian)

(defun camp--default-map (map)
  "Handle cases when MAP is the default `camp-keymap'."
  (if (eq map 'camp-keymap)
      'camp-keymap
    (intern (format "camp-%s-map" map))))

(cl-defmacro camp-define-keys (&key map simple)
  "Define keys when camp is dressed.

This function adds commands defined in :SIMPLE to the camp keymap :MAP.
Keybindings should be declared with

  (\"a\" 'commanda \"b\" 'commandb)

and so on, where A and B are keybindings that will be bound to
their corresponding command.

The camp keymap :MAP will be created if needed. To bind keys to
all camp keymap, use `camp-keymap' as the argument to :MAP."
  (let ((kmp (camp--default-map map)))
    `(progn
       (unless (boundp ',kmp)
         (defvar ,kmp (make-sparse-keymap)
           ,(format "Camp sharp keymap for %s mode" map)))
       ,@(mapcar
          (lambda (pair)
            `(define-key ,kmp
               (kbd ,(car pair)) ,(cadr pair)))
          (camp--group simple 2)))))

(defmacro camp-defkey (key map docstring &rest camp)
  "Define a function named camp-MAP-KEY documented by DOCSTRING
that expand the CAMP clauses.

KEY will be bound to camp-MAP-KEY in the MAP keymap, and
documented by DOCSTRING. For instance,

  (camp-defkey \"s\" text \"documentation\" ...)

defines a command named camp-text-s with ... as `camp' clauses.

See also `defcamp'."
  (declare (doc-string 3) (indent defun))
  (let ((f (intern (format "camp-%s-%s" map key))))
    `(progn
       (defcamp ,f ,docstring ,@camp)
       (define-key ,(camp--default-map map) (kbd ,key) #',f))))

(aeolian-sharp-minor camp
  "Dress camp for current major mode.

Activate the camp assisted commands for current major mode."
  :lighter " camp ")

(provide 'camp)
;;; camp.el ends here.
