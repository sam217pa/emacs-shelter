;;; shelter.el --- tent, camp, fort and so forth     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Maintainer: Samuel Barreto <samuel.barreto8@gmail.com>
;; Version: 0.1.0
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

;;

;;; Code:

(require 'tent)
(require 'camp)
(require 'fort)

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

(fort-define-keys
 :map fort-keymap
 :iron
 ("o" 'open-line
  "u" 'undo)
 :simple
 ("e" 'end-of-line
  "a" 'beginning-of-line
  "f f" 'find-file
  "i s" 'isearch-forward
  "i r" 'isearch-backward
  "b b" 'switch-to-buffer
  "b o" 'switch-to-buffer-other-window
  "SPC s" 'save-buffer))

;;;###autoload
(defalias 'shelter-mode 'camp-minor-mode)

(provide 'shelter)
;;; shelter.el ends here
