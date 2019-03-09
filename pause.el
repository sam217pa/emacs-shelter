;;; pause.el --- Transient map at point. Put game on pause  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Samuel BARRETO

;; Author: Samuel BARRETO <samuel.barreto8@gmail.com>
;; Keywords: keymap, keybindings

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

;; TODO: pause should also appear at end of buffer
;;       but don't know how. yet. [2019-02-25 23:07]

;;; Code:

(require 'cl-lib)

(defgroup pause nil
  "Simple transient map at point."
  :prefix "pause-"
  :group 'pause)

(defun pause--group (source n)
  "Divide SOURCE list in N groups and stack together the last
elements."
  (if (zerop n) (error "Zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;;###autoload
(defun pause-prompt (prompt)
  "Add text properties to special cookies in PROMPT.

Highlight text between square brackets with `highlight',
between round brackets with `font-lock-builtin-face',
between curly brackets with `font-lock-constant-face'."
  (declare (doc-string 1))
  (let ((str (propertize prompt 'face 'default))
        (regexp-colors '(("\\[[-_A-Za-z0-9=]+\\]" . font-lock-function-name-face)
                         ("([-_A-Za-z0-9=]+)"     . font-lock-keyword-face)
                         ("{[-_A-Za-z0-9=]+}"     . font-lock-constant-face))))
    (cl-loop
     for re-color in regexp-colors
     do (let ((start 0))
          (while (string-match (car re-color) str start)
            (let ((beg (1+ (match-beginning 0)))
                  (end (1- (match-end 0))))
              (setq start end)
              (add-text-properties
               beg end `(face ,(cdr re-color))
               str)))))
    str))

(defmacro pause--remov (ov)
  "Delete overlay OV and reset the containing variable."
  `(when (overlayp ,ov)
     (delete-overlay ,ov)
     (setq ,ov nil)))

(defmacro pause--ov-puts (ov &rest args)
  "Put ARGS properties to overlay OV.

ARGS should be grouped by two. Wraps `overlay-put'."
  (declare (indent 2))
  `(progn
     ,@(mapcar (lambda (props) `(overlay-put ,ov ,@props))
               (pause--group args 2))))

(defmacro pause--ov-display (str ct ov)
  "Display STR in an overlay OV at point, temporarily disabling
cursor type CT."
  `(let ((beg (point)) (end (1+ (line-end-position))))
    (when cursor-type
      (setq ,ct cursor-type)
      (setq cursor-type nil))
    (setq ,ov (make-overlay beg end))
    (pause--ov-puts ,ov
        'display ,str
        'before-string "\n"
        'after-string "\n"
        'priority 9999)
    nil))

(defun pause--pad (str)
  "Pad STR with empty char on the left so that it appears below
point."
  (let ((pad (if (bolp)
                 ""
               (make-string (- (point) (point-at-bol)) ?\s))))
    (mapconcat
     #'identity
     (split-string (concat pad str) "\n")
     (concat "\n" pad))))

(defmacro pause (persist-p prompt &rest args)
  "Put game in pause with a transient map.

This macro defines a transient keymap (see `set-transient-map')
with an overlay popup describing the available commands. The
overlay is put directly below point.

  (pause t
    \"Line spacing:\nIncrease [+] / Decrease [-]\"
    \"+\" (cl-incf line-spacing)
    \"-\" (cl-decf line-spacing))

In this exemple a transient map is defined, documented on the
second line. Third and fourth line maps + and - to a command that
increase the line-spacing. Any other key escape the transient map
and remove the popup.

PERSIST-P should be set to t if the map is to stay active after
a key present in the keymap has been pressed.

PROMPT is a message prompted out in an overlay below point.

ARGS is a list of key and the command they trigger. Commands are
wrapped into a (lambda () (interactive)) form, so that
non-interactive functions are directly called."
  (declare (indent defun) (doc-string 2))
  (let ((ov         (gensym "pause-ov-"))
        (ct         (gensym "pause-ct-"))
        (kmp        (gensym "pause-kmp-"))
        (ov-cleanup (gensym "pause-ov-clnp-")))
    `(let ((,kmp (make-sparse-keymap)) ,ov ,ct)
       (cl-flet
           ;; remove overlay and set cursor type back to normal
           ((,ov-cleanup ()
               (pause--remov ,ov)
               (pause--remov pause--ovback)
               (unless cursor-type (setq cursor-type ,ct))))
         ,@(cl-loop
            for kf in (pause--group args 2)
            collect `(define-key ,kmp (kbd ,(car kf))
                       (lambda () (interactive) ,(cadr kf))))
         (pause--ov-display (pause--pad ,prompt) ,ct ,ov)
         (set-transient-map ,kmp ,persist-p #',ov-cleanup)
         (message "...")))))

;;;###autoload
(defun pause-this-key ()
  "Return the key sequence of command at point."
  (key-description (this-single-command-raw-keys)))



(provide 'pause)
;;; pause.el ends here
