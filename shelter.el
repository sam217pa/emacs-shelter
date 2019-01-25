;;; shelter.el --- tent, camp, fort and so forth     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: keybindings, conv, convenience

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

(fort-define-keys
 ("f f" 'find-file
  "i s" 'isearch-forward
  "i r" 'isearch-backward
  "b b" 'switch-to-buffer
  "b o" 'switch-to-buffer-other-window))

(provide 'shelter)
;;; shelter.el ends here
