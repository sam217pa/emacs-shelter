;;; camp-text.el --- camp helpers for text modes     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: convenience, tent, text, editing

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

(defun camp-bwd-sentence (&optional arg)
  (interactive)
  (backward-sentence arg)
  (backward-char 1))

(defun camp-mark-bwd-sentence (&optional arg)
  (interactive)
  (camp-bwd-sentence arg)
  (mark-end-of-sentence (or arg 1)))

(defcamp camp-text
    "Camp commands for editing text."
    bk sentence-end-base tent
    '(("s" save-buffer "save buffer")
      ("b" camp-bwd-sentence "bwd sentence")
      ("f" forward-sentence "fwd sentence")
      ("m" camp-mark-bwd-sentence "mark")))

(provide 'camp-text)
;;; camp-text.el ends here
