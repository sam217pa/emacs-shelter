;;; shelter-org.el --- shelter commands for org mode       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: convenience, org

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
(require 'fort)

;; silence byte-compiler
(defvar org-outline-regexp)

(cl-symbol-macrolet
    ((at-outl (and (camp-at org-outline-regexp)
                   (camp-bk "^\**"))))

  (defcamp camp-org-heading
    "Function to encrypt and decrypt at org heading.

Serves as complement of speed commands for org buffers"
    if at-outl
    tent '(("e" org-encrypt-entry "encrypt")
           ("d" org-decrypt-entry "decrypt"))))

(camp-define-keys
 :map org
 :simple
  ("x" 'camp-org-heading))

(provide 'shelter-org)
;;; shelter-org.el ends here.
