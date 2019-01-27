;;; aeolian.el --- define major-mode dependent minor modes -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: minor-modes

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

;; Aeolian provides a single macro, aeolian-sharp-minor, which helps
;; to define a minor mode whose flavor depends on the major-mode it is
;; associated to.

;;; Code:

(require 'cl-lib)
(require 'easy-mmode)

;; note: recursive but shouldn't overflow, or else there is a big
;; problem: no mode should inherit from 9999 parents.
(defun aeolian--derived-modes (mode)
  "Return all modes that mode derives from."
  (let ((drvd (get mode 'derived-mode-parent)))
    (if drvd
        (cons mode
              (aeolian--derived-modes drvd))
      (cons mode nil))))

(defun aeolian--shorten-mjr (mode)
  "Return major-mode named stripped off its \"mode\" suffix if need be."
  (replace-regexp-in-string
   "-mode" ""
   (symbol-name mode)))

(defun aeolian--map-for-major (mmp)
  "Return map for major mode associated with minor mode prefixed with MMP."
  (mapcar #'intern-soft
          (mapcar
           (lambda (mjr) (format "%s-%s-map" mmp mjr))
           (mapcar #'aeolian--shorten-mjr
                   (aeolian--derived-modes major-mode)))))

(defun aeolian--activater (mmp)
  "Activate the minor mode prefixed with MMP by adding the corresponding
map for major mode to the default map."
  (let ((mjr-map-fun (intern (format "%s--major-mode-map" mmp)))
        (mode-keymap (intern (format "%s-keymap" mmp)))
        (mm-name     (intern (format "%s-minor-mode" mmp)))
        (mm-override (intern (format "%s-minor-mode-override" mmp))))
    (let ((maps (funcall mjr-map-fun)))
       (cl-loop
        for mapname in maps
        do (when mapname
             (let ((map (eval mapname)))
               (set-keymap-parent map (eval mode-keymap))
               (if mm-override
                   (push (cons mm-name map)
                         minor-mode-overriding-map-alist)
                 (setf (cdr (assoc mm-name minor-mode-map-alist))
                       map))))))))

(defun aeolian--deactivater (mmp)
  "Remove traces of minor mode prefixed with MMP from the environment."
  (let ((mm-name (intern (format "%s-minor-mode" mmp))))
    (setq minor-mode-overriding-map-alist
          (assq-delete-all
           mm-name
           minor-mode-overriding-map-alist))))

;;;###autoload
(defmacro aeolian-sharp-minor (prefix docstring &rest keys)
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

This macro defines two variables and four functions:
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
       (defun ,lkup-map   ()
         ,(format "Return %s sharp keymap associated to major mode." prefix)
         (aeolian--map-for-major ',prefix))
       (defun ,activate   ()
         ,(format "Activate %s sharp keymap by adding to and overriding `minor-mode-map-alist'"
                  prefix)
         (aeolian--activater ',prefix))
       (defun ,deactivate ()
         ,(format "Deactivate %s sharp keymap by removing traces of it from `minor-mode-map-alist'"
                  prefix)
         (aeolian--deactivater ',prefix))
       (define-minor-mode ,mnr-name
         ,docstring
         :keymap ,keymap
         :group ',prefix
         ,@keys
         (if ,mnr-name (,activate) (,deactivate))))))

(provide 'aeolian)
;;; aeolian.el ends here
