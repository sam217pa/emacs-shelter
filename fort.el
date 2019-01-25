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
(require 'camp-macs)

(defcustom fort-default-key "x"
  "Default keybinding for leaving fort."
  :type 'string
  :group 'fort)

(defmacro fort-defkeymap (name docstring &optional keys)
  "Define a new fort minor mode keymap for mode NAME.

NAME should not include \"-mode\". For instance, to define a new
fort keymap for org-mode, one would call (fort-defkeymap org
...).

DOCSTRING is the documentation of the keymap. KEYS are passed to
`define-key' by `camp--defkeys'."
  (declare (indent 1) (doc-string 2))
  (let ((kmp (intern (format "fort-%s-map" name))))
    `(progn
       (defvar ,kmp (make-sparse-keymap) ,docstring)
       (when ,keys (camp--defkeys ,keys ,kmp))
       (define-key ,kmp (kbd fort-default-key) 'fort))))

;;; Minor mode

(defsubst fort--toggle ()
  "Toggle `fort-minor-mode'."
  (if (camp--rom-p) (fort-minor-mode +1)
    (fort-minor-mode -1)))

;;;###autoload
(defun fort ()
  "Toggle a `fort-minor-mode' assisted read-only buffer."
  (interactive)
  (toggle-read-only)
  (fort--toggle))

;;;###autoload
(camp-sharp-minor fort
  "Fort entranches buffer in read-only mode.

Think bunker editing.

No self-inserting commands will self-insert, which means they can
be remmaped to other commands for the time being. It provides
a kind of modal editing."
  :lighter " fort ")

(provide 'fort)
;;; fort.el ends here
