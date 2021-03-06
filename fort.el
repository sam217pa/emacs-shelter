;;; fort.el --- commands retranched in read-only mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: keybindings, extensions

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

(defcustom fort-default-key "x"
  "Default keybinding for leaving fort."
  :type 'string
  :group 'fort)

;;;; Keybindings

(defun fort--default-keymap (map)
  "Handles cases when map is default keymap"
  (if (eq map 'fort-keymap)
      'fort-keymap
    (intern (format "fort-%s-map" map))))

(defmacro fort-iron (&rest args)
  "Inhibit Read Only Now.

Execute ARGS in a temporary deactivated read-only mode."
  (declare (debug t) (indent 2))
  `(lambda () (interactive)
     (let ((inhibit-read-only t))
       ,@(mapcar
          (lambda (f) `(call-interactively ,f))
          args))))

(cl-defmacro fort-define-keys (&key map simple iron)
  "Define key retranched in `fort'.

Keys will be added to the fort keymap :MAP. All keys declared
after :SIMPLE are, well, simple keybindings, in the form
of (\"a\" 'command \"b\" 'command), where A and B are
keybindings.

All keys declared after :IRON are wrapped into `fort-iron', which
inhibits read-only mode for the execution of the command only.
Use it for commands that are to modify the buffer. Keys are
declared the same way as for :SIMPLE.

To bind keys to all fort keymap, use `fort-keymap' as argument
to :MAP."
  (declare (debug t))
  (let ((kmp (fort--default-keymap map)))
    `(progn
       (unless (boundp ',kmp)
         (defvar ,kmp (make-sparse-keymap)
           ,(format "Fort sharp keymap for %s mode" map)))
       ,@(mapcar
          (lambda (pair)
            `(define-key ,kmp
               (kbd ,(car pair)) ,(cadr pair)))
          (camp--group simple 2))
       ,@(mapcar
          (lambda (pair)
            `(define-key ,kmp (kbd ,(car pair))
               (fort-iron ,(cadr pair))))
          (camp--group iron 2)))))

;;;; Minor mode

(defun fort--toggle ()
  "Toggle `fort-minor-mode'."
  (if (camp--rom-p) (fort-minor-mode +1)
    (fort-minor-mode -1)))

;;;###autoload
(defun fort ()
  "Toggle a `fort-minor-mode' assisted read-only buffer."
  (interactive)
  (read-only-mode 'toggle)
  (fort--toggle))

;;;###autoload
(aeolian-sharp-minor fort
  "Fort entranches buffer in read-only mode.

Think bunker editing.

No self-inserting commands will self-insert, which means they can
be remmaped to other commands for the time being. It provides
a kind of modal editing."
  :lighter " fort ")

(fort-define-keys
 :map fort-keymap
 :simple
  (fort-default-key 'fort))

(provide 'fort)
;;; fort.el ends here
