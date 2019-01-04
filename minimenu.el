;;; minimenu.el --- display commands in a minimenu at point  -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'mule-util)                    ; for truncate-string-to-width

(defcustom minimenu-max-lines 5
  "Maximum number of lines to be displayed in minimenus."
  :type 'integer
  :group 'minimenu)

(defcustom minimenu-max-description-length 15
  "Maximum number of character for key description."
  :type 'integer
  :group 'minimenu)

;;;; Minimenu displaying

(defvar minimenu-ov nil
  "Default minimenu overlay.

Is redefined each time an overlay is required by a minimenu
function.")

(defvar minimenu--old-cursor-type nil
  "Cursor type variable to save the old cursor type.

It can be restored after the overlay has been cleaned up.")

(defun minimenu--ov-cleanup ()
  "Reset the minimenu overlay.

Set the ``minimenu-ov'' to nil and delete the corresponding
overlay."
  (when (overlayp minimenu-ov)
    (delete-overlay minimenu-ov)
    (setq minimenu-ov nil)
    (unless cursor-type
      (setq cursor-type minimenu--old-cursor-type)))
  (message "Minimenu off"))

(defun minimenu--overlay (str)
  "Display STR in the ``minimenu-ov'' overlay."
  (let ((beg (point))
        (end (1+ (line-end-position))))
    (when cursor-type
      (setq minimenu--old-cursor-type cursor-type)
      (setq cursor-type nil))
    (setq minimenu-ov (make-overlay beg end))
    (overlay-put minimenu-ov 'display str)
    (overlay-put minimenu-ov 'before-string "\n")
    (overlay-put minimenu-ov 'after-string "")
    (overlay-put minimenu-ov 'priority 9999)
    nil))

(defsubst minimenu--unpropertize (str)
  (substring-no-properties str))

(defun minimenu--propertize (str &rest arg)
  (propertize (minimenu--unpropertize str) 'face arg))

(defun minimenu--propertize-key (key-value)
  "Add text properties to the key in KEY-VALUE."
  (minimenu--propertize (car key-value) 'link))

(defun minimenu--propertize-value (key-value max-pad)
  "Add text properties to the value in KEY-VALUE."
  (let*
      ((v (cdr key-value))
       (str (concat v (make-string (- max-pad (length v)) ?\s))))
    (minimenu--propertize str 'highlight)))

(defmacro minimenu--propertize-bg (str)
  "Rectify overlay background to default background color"
  `(propertize
    ,str
    'font-lock-face `(:background ,(face-background 'default)
                                  :foreground ,(face-foreground 'default))))

(defun minimenu--propertize-key-value (key-value max-pad)
  "Returns a correctly formatted and propertized key-functions.

MAX-PAD is the length of the longest string in KEY-VALUE, so that
highlight of shorter string are of the same length."
  (if (null key-value)
      ""
    (let*
        ((key (minimenu--propertize-key key-value))
         (fun (minimenu--propertize-value key-value max-pad))
         (str (format "%s - %s" key fun)))
      (minimenu--propertize-bg str))))

(defsubst minimenu--max-text-length (col)
  "Returns the maximum text length of collection."
  (apply #'max (cl-loop for x in col collect (length (cdr x)))))

(defun minimenu--map2concat (col padding f &optional args)
  (apply
   #'concat
   (cl-loop
    for x in (camp--group col 2)
    collect
    (format "%s %s\n%s"
            (funcall f (car x) args)
            (funcall f (cadr x) args)
            padding))))

(defun minimenu--flatten-col (col padding)
  "Flatten the collection COL and add PADDING if necessary.

Return collection COL as a single string that is left padded
so that text is displayed aligned to the point.

Text is propertized correctly."
  (minimenu--propertize-bg
   (concat
    padding
    (minimenu--map2concat
     col padding
     #'minimenu--propertize-key-value
     (minimenu--max-text-length col))
    "\n")))

(defsubst minimenu--empty-width ()
  (- (point) (point-at-bol)))

(defun minimenu--left-pad ()
  "Returns an empty string of width corresponding to number of
characters between point and beginning of line."
  (if (bolp) "" (make-string (minimenu--empty-width) ?\s)))

(defun minimenu--display (col)
  "Display collection left padded in the `minimenu-ov' overlay."
  (let*
      ((padding (minimenu--left-pad))
       (str (minimenu--flatten-col (minimenu--make-print col)
                                   padding)))
    (minimenu--overlay str)))

(defun minimenu--truncate-desc (str)
  (truncate-string-to-width
   str
   (1+  minimenu-max-description-length)
   nil nil "…"))

(defsubst minimenu--get-fun (x)
  (elt x 1))

(defsubst minimenu--get-desc (x)
  (minimenu--truncate-desc (elt x 2)))

(defun minimenu--make-collection (col)
  (cl-loop
   for x in col
   collect (list (car x) (minimenu--get-fun x))))

(defun minimenu--make-print (col)
  (cl-loop
   for x in col
   collect (cons (car x) (minimenu--get-desc x))))

(defun minimenu--set-transient-map (col)
  (let ((map (make-sparse-keymap)))
    (cl-loop
     for key in (minimenu--make-collection col)
     do (define-key map (car key) (cadr key)))
    (set-transient-map map nil #'minimenu--ov-cleanup)))

;;;###autoload
(defun minimenu-call (col)
  "Display COL alist in an overlay and call the function
associated to input key."
  ;; display the collection
  (minimenu--display col)
  ;; parse next user key
  (minimenu--set-transient-map col)
  (message "Minimenu on"))

(provide 'minimenu)
;;; minimenu.el ends here
