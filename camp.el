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
(require 'camp-lisp)
(require 'camp-org)

(defvar camp-keymap (make-sparse-keymap))

(defvar camp-minor-mode-override t
  "Put keys in `camp-keymap' into `minor-mode-overriding-map-alist'.")

(defun camp--major-mode-map ()
  "Return the camp map associated with major mode."
  (let ((mm (replace-regexp-in-string
             "-mode" ""
             (symbol-name major-mode))))
    (eval
     (intern-soft (format "camp-%s-map" mm)))))

(defun camp--activate ()
  "Take care of adding the camp keymap to the current keymap."
  (let*
      ((major-camp-map (camp--major-mode-map))
       (map (when major-camp-map
              (set-keymap-parent major-camp-map camp-keymap)
              major-camp-map)))
    (if camp-minor-mode-override
        (push `(camp-minor-mode . ,map)
              minor-mode-overriding-map-alist)
      (setf (cdr (assoc 'camp-minor-mode minor-mode-map-alist))
            map))))

(defun camp--deactivate ()
  "Remove traces of camp activation in the minor mode alist."
  (setq minor-mode-overriding-map-alist
        (assq-delete-all
         'camp-minor-mode minor-mode-overriding-map-alist)))

;;;###autoload
(define-minor-mode camp-minor-mode
  "Dress camp for current major mode.

Activate the camp assisted commands for current major mode."
  :keymap camp-keymap
  :group 'camp
  :lighter " camp "
  (if camp-minor-mode
      (camp--activate)
    (camp--deactivate)))

(provide 'camp)
;;; camp.el ends here.
