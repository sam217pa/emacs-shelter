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
(require 'camp-macs)

(cl-defmacro camp-define-keys (&key map simple)
  "doc"
  (let ((kmp (intern (format "camp-%s-map" map))))
    `(progn
       (unless ,kmp
         (defvar ,kmp
           ,(format "Camp sharp keymap for %s mode" map)))
       ,@(mapcar
          (lambda (pair)
            `(define-key ,kmp
               (kbd ,(car pair)) ,(cadr pair)))
          (camp--group simple 2)))))

(camp-sharp-minor camp
  "Dress camp for current major mode.

Activate the camp assisted commands for current major mode."
  :lighter " camp ")

(provide 'camp)
;;; camp.el ends here.
