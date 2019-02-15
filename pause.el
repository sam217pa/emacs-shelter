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

;;

;;; Code:

(require 'cl-lib)
(require 'camp-utils)

;;;###autoload
(defun pause-prompt (prompt)
    "Add text properties to special cookies in PROMPT.

Highlight text between square brackets with `highlight',
between round brackets with `font-lock-builtin-face',
between curly brackets with `font-lock-constant-face'."
    (let ((str prompt)
          (regexp-colors '(("\\[[-_A-Za-z0-9=]+\\]" . 'highlight)
                           ("([-_A-Za-z0-9=]+)"     . 'font-lock-builtin-face)
                           ("{[-_A-Za-z0-9=]+}"     . 'font-lock-constant-face))))
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

(defmacro pause--ov-puts (ov &rest args)
  "Put ARGS properties to overlay OV.

ARGS should be grouped by two. Wraps `overlay-put'."
  (declare (indent 2))
  `(progn
     ,@(mapcar (lambda (props) `(overlay-put ,ov ,@props))
               (camp--group args 2))))

(defmacro pause (persist-p prompt &rest args)
  (declare (indent defun))
  (let ((ov         (gensym "pause-ov-"))
        (ct         (gensym "pause-ct-"))
        (kmp        (gensym "pause-kmp-"))
        (ov-cleanup (gensym "pause-ov-clnp-"))
        (ov-display (gensym "pause-ov-disp-"))
        (ov-pad     (gensym "pause-ov-pad-")))
    `(let ((,kmp (make-sparse-keymap)) ,ov ,ct)
       (cl-flet
           ;; remove overlay and set cursor type back to normal
           ((,ov-cleanup ()
               (when (overlayp ,ov)
                 (delete-overlay ,ov)
                 (setq ,ov nil))
               (unless cursor-type (setq cursor-type ,ct)))
            ;; display str in overlay at point
            ;; remove the cursor for now
            (,ov-display (str)
              (let ((beg (point)) (end (1+ (line-end-position))))
                (when cursor-type
                  (setq ,ct cursor-type)
                  (setq cursor-type nil))
                (setq ,ov (make-overlay beg end))
                (pause--ov-puts ,ov
                    'display str
                    'before-string "\n"
                    'after-string ""
                    'priority 9999)
                nil))
            ;; pad string with empty char on the left so that it appears
            ;; below point
            (,ov-pad (str)
               (let ((pad (if (bolp) "" (make-string (- (point)
                                                        (point-at-bol))
                                                     ?\s))))
                 (mapconcat #'identity
                            (split-string (concat pad str) "\n")
                            (concat "\n" pad)))))
         ,@(cl-loop
            for kf in (sam--group args 2)
            collect `(define-key ,kmp (kbd ,(car kf))
                       (lambda () (interactive) ,(cadr kf))))
         (,ov-display (,ov-pad (pause-prompt ,prompt)))
         (set-transient-map ,kmp ,persist-p #',ov-cleanup)
         (message "...")))))

(provide 'pause)
;;; pause.el ends here
