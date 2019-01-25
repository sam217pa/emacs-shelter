;;; camp-macs.el --- macro helpers for tent, camp and fort.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: macros

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

(eval-when-compile
  (require 'cl-lib))

(require 'easy-mmode)
(require 'camp-utils)

(defmacro camp-key-definer (mmp)
  "Return a macro named MMP-define-keys that wraps `define-key'
around the minor-mode prefixed with MMP.

For exemple, (camp-key-definer camp) returns the macro
`camp-define-keys' that can be called
  (camp-define-keys (\"x\" 'command))
to add command to the default `camp-keymap'."
  (let ((defkey (intern (format "%s-define-keys" mmp)))
        (mmp-str (format "%s" mmp)))
    `(progn
       (cl-defmacro ,defkey (args &key (keymap nil))
         ,(format "Add key-bindings in ARGS to \`%s-keymap\', or to\
 \`%s-KEYMAP-map\' when :KEYMAP is non nil." mmp-str mmp-str)
         (let ((kmp (if keymap
                        (intern (format "%s-%s-map" ,mmp-str keymap))
                      (intern (format "%s-keymap" ,mmp-str)))))
           `(progn
              ,@(mapcar
                 (lambda (pair)
                   `(define-key ,kmp
                      (kbd ,(car pair))
                      ',(cadadr pair)))
                 (camp--group args 2))))))))

;; (defmacro camp-aif (test-form then-form &optional else-form)
;;   (declare (indent 1))
;;   `(let ((it ,test-form))
;;      (if it ,then-form ,else-form)))

;; (defmacro camp-awhen (test-form &body body)
;;   (declare (indent 1))
;;   `(camp-aif ,test-form
;;      (progn ,@body)))

(defun camp-macs--shorten-mjr ()
  "Return major-mode named stripped off its \"mode\" suffix if need be."
  (replace-regexp-in-string
   "-mode" ""
   (symbol-name major-mode)))

(defmacro camp-macs--map-for-major (mmp)
  "Return map for major mode associated with minor mode prefixed with MMP."
  (let* ((mjr (camp-macs--shorten-mjr))
         (mnr (format "%s" mmp))
         (map (format "%s-%s-map" mnr mjr)))
    `(progn
       ,(intern-soft map))))

(defmacro camp-macs--activater (mmp)
  "Activate the minor mode prefixed with MMP by adding the corresponding
map for major mode to the default map."
  (let ((mjr-map-fun (intern (format "%s--major-mode-map" mmp)))
        (mode-keymap (intern (format "%s-keymap" mmp)))
        (mm-name     (intern (format "%s-minor-mode" mmp)))
        (mm-override (intern (format "%s-minor-mode-override" mmp))))
    `(let ((map (when (,mjr-map-fun)
                  (set-keymap-parent (,mjr-map-fun) ,mode-keymap)
                  (,mjr-map-fun))))
       (if ,mm-override
           (push (cons ',mm-name map)
                 minor-mode-overriding-map-alist)
         (setf (cdr (assoc ',mm-name minor-mode-map-alist))
               map)))))

(defmacro camp-macs--deactivater (mmp)
  "Remove traces of minor mode prefixed with MMP from the environment."
  (let ((mm-name (intern (format "%s-minor-mode" mmp))))
    `(setq minor-mode-overriding-map-alist
           (assq-delete-all
            ',mm-name
            minor-mode-overriding-map-alist))))

;;;###autoload
(defmacro camp-sharp-minor (prefix docstring &rest keys)
  "Define a sharp minor mode, a minor mode with different flavors
depending on current major mode.

Let a sharp minor mode be a mode with a default keymap, common to
all invocations of it, and a sharp keymap, a keymap that depends
on the current major mode. There can be as many sharp keymap as
major modes, but only the sharp keymap corresponding to major
mode will be associated to the buffer that calls the minor mode.

The defined minor mode is called PREFIX-minor-mode, documented by
DOCSTRING. KEYS will be passed to `define-minor-mode', to enable
further package description.

This macro defines two variables and four function:
- PREFIX-keymap, the default keymap for minor mode, empty by default.
- PREFIX-minor-mode-override, set to t by default. It sets whether or
    not the minor mode overrides other keybindings in the keymap.

- PREFIX--activate, activate the default keymap and the sharp keymap
	for current major mode.
- PREFIX--deactivate, removes all traces of minor-mode and sharp keymap from
	current minor keymaps.
- PREFIX--major-mode-map, returns the sharp keymap associated to major mode.
- PREFIX-define-keys, simpler binding to either PREFIX-keymap, the default
	minor-mode map, or to a sharp keymap."
  (declare (doc-string 2)
           (indent defun))
  (let
      ((mnr-name   (intern (format "%s-minor-mode" prefix)))
       (activate   (intern (format "%s--activate" prefix)))
       (deactivate (intern (format "%s--deactivate" prefix)))
       (lkup-map   (intern (format "%s--major-mode-map" prefix)))
       (keymap     (intern (format "%s-keymap" prefix)))
       (kmp-ovrd   (intern (format "%s-minor-mode-override" prefix))))
    `(progn
       (defvar ,keymap (make-sparse-keymap))
       (defvar ,kmp-ovrd t)
       (defun ,lkup-map   () (camp-macs--map-for-major ,prefix))
       (defun ,activate   () (camp-macs--activater ,prefix))
       (defun ,deactivate () (camp-macs--deactivater ,prefix))
       (camp-key-definer ,prefix)
       (define-minor-mode ,mnr-name
         ,docstring
         :keymap ,keymap
         :group ',prefix
         ,@keys
         (if ,mnr-name (,activate) (,deactivate))))))

(provide 'camp-macs)
;;; camp-macs.el ends here
