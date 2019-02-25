;;; tent.el --- display commands in a menu at point  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019  Samuel BARRETO
;;
;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: convenience, keybindings, menu
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The tent.el package provides a single function =tent= that displays
;; a list of commands and their corresponding keybindings in a small
;; overlayed menu.
;;
;; In a similar vein as Hydra <https://github.com/abo-abo/hydra>, it
;; provides a simple way to increase the number of
;; within-two-key-strokes-available commands without resorting to
;; complicated solutions. Unless hydra though, the menu is a simple
;; tabular menu that is visible right under point, so that focus is
;; not lose --- it matters on large screens.
;;
;;; Notes:
;;
;; TODO: [2019-01-06 16:30] make transient map redisplay overlay if permanent
;; FIXME: [2019-01-06 19:19] bug when point is at eob, no tent is displayed.
;; TODO: [2019-01-08 13:26] add a timer so that tent is displayed only after timer seconds of inactivity
;;
;;; Code:

(require 'cl-lib)
(require 'mule-util)                    ; for truncate-string-to-width

(defcustom tent-max-lines 5
  "Maximum number of lines to be displayed in tents."
  :type 'integer
  :group 'tent)

(defcustom tent-max-description-length 15
  "Maximum number of character for key description."
  :type 'integer
  :group 'tent)

;;;; Tent displaying

(defvar tent-ov nil
  "Default tent overlay.

Is redefined each time an overlay is required by a tent
function.")

(defvar tent--old-cursor-type nil
  "Cursor type variable to save the old cursor type.

It can be restored after the overlay has been cleaned up.")

(defun tent--ov-cleanup ()
  "Reset the tent overlay.

Set the ``tent-ov'' to nil and delete the corresponding
overlay."
  (when (overlayp tent-ov)
    (delete-overlay tent-ov)
    (setq tent-ov nil)
    (unless cursor-type
      (setq cursor-type tent--old-cursor-type)))
  (message "Tent off"))

(defun tent--overlay (str)
  "Display STR in the ``tent-ov'' overlay."
  (let ((beg (point))
        (end (1+ (line-end-position))))
    (when cursor-type
      (setq tent--old-cursor-type cursor-type)
      (setq cursor-type nil))
    (setq tent-ov (make-overlay beg end))
    (overlay-put tent-ov 'display str)
    (overlay-put tent-ov 'before-string "\n")
    (overlay-put tent-ov 'after-string "")
    (overlay-put tent-ov 'priority 9999)
    nil))

(defsubst tent--unpropertize (str)
  "Return STR with no text properties."
  (let ((s str))
    (set-text-properties 0 (1- (length s)) nil s)
    s))

(defun tent--propertize (str arg)
  "Return STR with face associated to ARG."
  (propertize (tent--unpropertize str) 'face arg))

(defun tent--propertize-key (key-value)
  "Add text properties to the key in KEY-VALUE."
  (tent--propertize (car key-value) 'link))

(defun tent--propertize-value (key-value max-pad)
  "Add text properties to the value in KEY-VALUE."
  (let*
      ((v (cdr key-value))
       (str (concat v (make-string (- max-pad (length v)) ?\s))))
    (tent--propertize str 'highlight)))

(defun tent--propertize-bg (str)
  "Rectify overlay background to default background color"
  str)

(defun tent--propertize-key-value (key-value max-pad)
  "Returns a correctly formatted and propertized key-functions.

MAX-PAD is the length of the longest string in KEY-VALUE, so that
highlight of shorter string are of the same length."
  (if (null key-value)
      ""
    (let*
        ((key (tent--propertize-key key-value))
         (fun (tent--propertize-value key-value max-pad))
         (str (format "%s - %s" key fun)))
      (tent--propertize-bg str))))

(defsubst tent--max-text-length (col)
  "Returns the maximum text length of collection."
  (apply #'max (cl-loop for x in col collect (length (cdr x)))))

(defun tent--map2concat (col padding f &optional args)
  "Map F to collection COL and concat them with PADDING.

ARGS are passed to F."
  (apply
   #'concat
   (cl-loop
    for x in (camp--group col 2)
    collect
    (format "%s %s\n%s"
            (funcall f (car x) args)
            (funcall f (cadr x) args)
            padding))))

(defun tent--flatten-col (col padding)
  "Flatten the collection COL and add PADDING if necessary.

Return collection COL as a single string that is left padded
so that text is displayed aligned to the point.

Text is propertized correctly."
  (tent--propertize-bg
   (concat
    padding
    (tent--map2concat
     col padding
     #'tent--propertize-key-value
     (tent--max-text-length col))
    "\n")))

(defsubst tent--empty-width ()
  "Return width between point and point at bol."
  (- (point) (point-at-bol)))

(defun tent--left-pad ()
  "Returns an empty string of width corresponding to number of
characters between point and beginning of line."
  (if (bolp) "" (make-string (tent--empty-width) ?\s)))

(defun tent--display (col)
  "Display collection left padded in the `tent-ov' overlay."
  (let*
      ((padding (tent--left-pad))
       (str (tent--flatten-col (tent--make-print col)
                                   padding)))
    (tent--overlay str)))

(defun tent--truncate-desc (str)
  "Truncate STR so that it is `tent-max-description-length'
long."
  (truncate-string-to-width
   str
   (1+  tent-max-description-length)
   nil nil "…"))

(defsubst tent--get-fun (x)
  "Return the fun associated to X."
  (elt x 1))

(defsubst tent--get-desc (x)
  "Return the description associated to X."
  (tent--truncate-desc (elt x 2)))

(defun tent--make-collection (col)
  "Return a list with car key and cadr function for each element
of collection COL."
  (cl-loop
   for x in col
   collect (list (car x) (tent--get-fun x))))

(defun tent--make-print (col)
  "Return a list with car key and cdr description for each
element of collection COL."
  (cl-loop
   for x in col
   collect (cons (car x) (tent--get-desc x))))

(defun tent--set-transient-map (col)
  "Return a transient map with each key/bindings in COL associated to it.

The transient map calls `tent--ov-cleanup' on exit so that the
menu is removed."
  (let ((map (make-sparse-keymap)))
    (cl-loop
     for key in (tent--make-collection col)
     do (define-key map (kbd (car key)) (cadr key)))
    (set-transient-map map nil #'tent--ov-cleanup)))

;;;###autoload
(defun tent (col)
  "Display COL alist in an overlay and call the function
associated to input key."
  ;; display the collection
  (tent--display col)
  ;; parse next user key
  (tent--set-transient-map col)
  (message "Tent on"))

(provide 'tent)
;;; tent.el ends here
