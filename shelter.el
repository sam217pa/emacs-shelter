;;; shelter.el --- tent, camp, fort and so forth     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Maintainer: Samuel Barreto <samuel.barreto8@gmail.com>
;; Version: 0.1.1
;; Created: 2019-01-25
;; Keywords: keybindings, keys, config

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

;; Shelter is a collection of presets that provides commands for
;; editing when in what could be called a "secure" position, i.e. when
;; point is after or before a character that delimits a semantic
;; entity, or when buffer is in read-only mode.
;;
;; It does so with the help of the `tent' package, that deploys
;; a temporary mini-menu in an overlay at point; the `camp' package,
;; that defines self-inserting commands which action depends on local
;; context; and the `fort' package, that defines commands only
;; effective in read-only mode.
;;
;; The `camp' and `fort' package defines minor modes using the
;; `aeolian' package. It allows defining minor-modes whose flavor
;; depends on the associated major-mode, the so-called sharp minor
;; modes.



;;; Code:

(require 'tent)
(require 'camp)
(require 'fort)

;; shelter presets
(require 'shelter-text)
(require 'shelter-org)
(require 'shelter-lisp)

(defcustom shelter-remap-read-only t
  "Whether to remap read-only keybindings (C-x C-q) to `fort'.
Defaults to t."
  :type 'boolean
  :group 'shelter)

(when shelter-remap-read-only
  (global-set-key [remap read-only-mode] #'fort))

;; defaults fort fort-keymap, serves as example.
(fort-define-keys
 :map fort-keymap
 :iron   ("o" 'open-line
          "u" 'undo)
 :simple ("e" 'end-of-line
          "a" 'beginning-of-line
          "f f" 'find-file
          "i s" 'isearch-forward
          "i r" 'isearch-backward
          "b b" 'switch-to-buffer
          "b o" 'switch-to-buffer-other-window
          "SPC s" 'save-buffer))        ; spacemacs like.

;;;###autoload
(defalias 'shelter-mode 'camp-minor-mode)

(provide 'shelter)
;;; shelter.el ends here
