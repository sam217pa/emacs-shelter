;;; shelter-utils.el --- utilities for shelter       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel Barreto

;; Author: Samuel Barreto <samuel.barreto8@gmail.com>
;; Keywords: utilities, macros, shelter

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

(defmacro shelter-with-gensyms (symbols &rest body)
  "The classic with gensyms macro that evaluate BODY with
gensymed SYMBOLS."
  (declare (indent 1))
  (assert (cl-every #'symbolp symbols))
  `(let (,@(mapcar (lambda (s) `(,s (gensym))) symbols))
     ,@body))

(defmacro shelter-letgs (bindings &rest body)
  "Execute BODY in an environment where BINDINGS are bound to
gensymed symbols."
  (declare (indent 1))
  `(shelter-with-gensyms (,@(mapcar #'car bindings))
     (let (,@(mapcar (lambda (bd) `(,@bd)) bindings))
       ,@body)))

(defmacro shelter-letgs* (bindings &rest body)
  "Same as `shelter-letgs' but with a `let*' binding."
  (declare (indent 1))
  `(shelter-with-gensyms (,@(mapcar #'car bindings))
     (let* (,@(mapcar (lambda (bd) `(,@bd)) bindings))
       ,@body)))

(provide 'shelter-utils)
;;; shelter-utils.el ends here
