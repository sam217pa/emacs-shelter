;;; shelter-text.el --- camp helpers for text modes     -*- lexical-binding: t; -*-

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

;;;; Customize

(defgroup shelter-text nil
  "Commands for text editing at shelter."
  :group 'shelter
  :prefix "shelter-text-")

(defcustom shelter-text-remap-sentence-navigation nil
  "Whether to remap the forward and backward sentence commands to
their camp counterpart, `camp-bwd-sentence' and `camp-fwd-sentence'."
  :type 'boolean
  :group 'shelter-text)

(when shelter-text-remap-sentence-navigation
  (global-set-key [remap forward-sentence] 'camp-fwd-sentence)
  (global-set-key [remap backward-sentence] 'camp-bwd-sentence))

;;;###autoload
(defun camp-bwd-sentence (&optional arg)
  "Wrapper around `backward-sentence' that tries to go to the
punctuation sign."
  (interactive)
  (backward-sentence arg)
  (unless (bolp)
    (backward-char 1)))

;;;###autoload
(defalias 'camp-fwd-sentence 'forward-sentence)

;;;###autoload
(defun camp-mark-bwd-sentence (&optional arg)
  (interactive)
  (camp-bwd-sentence arg)
  (mark-end-of-sentence (or arg 1)))

;;;###autoload
(defcamp camp-outline
    "Camp commands when looking at outlines"
    at outline-regexp tent
    '(("a" outline-show-all "show all")
      ("e" outline-show-entry "show entry")
      ("B" outline-show-children "show children")
      ("k" outline-show-branches "show branches")
      ("s" outline-show-subtree "show subtree")

      ("o" outline-hide-other "hide other")
      ("q" outline-hide-sublevels "hide sublevels")
      ("t" outline-hide-body "hide body")
      ("c" outline-hide-entry "hide entry")
      ("d" outline-hide-subtree "hide subtree")
      ("l" outline-hide-leaves "hide leaves")

      ("b" outline-backward-same-level "bkwd same lvl")
      ("f" outline-forward-same-level "frwd same lvl")
      ("u" outline-up-heading "up head")
      ("p" outline-previous-visible-heading "prev head")
      ("n" outline-next-visible-heading "next head")

      ("T" outline-insert-heading "insert head")
      ("v" outline-move-subtree-down "move down")
      ("^" outline-move-subtree-up "move up")
      ("<" outline-promote "promote")
      (">" outline-demote "demote")
      ("@" outline-mark-subtree "mark subtree")))



(defsubst camp-fwd-paragraph ()
  (forward-paragraph 1))

(defsubst camp-bwd-paragraph ()
  (backward-paragraph 1))

(cl-symbol-macrolet
    ((bk-stce  (or (camp-bk sentence-end-base) (bolp)))
     (at-para  (and (bolp) (camp-at paragraph-start)))
     (at-p-beg (and (bolp) (camp-stay (forward-line -1)
                                      (and (bolp) (eolp)))))
     (emptyl   (and (bolp) (eolp))))

  (defcamp camp-text
    "Camp commands for editing text."
    if bk-stce tent
    '(("x" fort "fort")
      ("s" save-buffer "save buffer")
      ("b" camp-bwd-sentence "bwd sentence")
      ("f" forward-sentence "fwd sentence")
      ("m" camp-mark-bwd-sentence "mark")))

  (defcamp camp-text-up
    "Go up"
    if at-p-beg do (forward-line -1))

  (defcamp camp-text-down
    "Go down"
    if emptyl do (forward-line 1))

  (defcamp camp-text-next
    "Commands for next semantic element when at camp."
    at "" do (forward-page)
    if at-para do (camp-fwd-paragraph)
    if bk-stce do (camp-fwd-sentence))

  (defcamp camp-text-prev
    "Commands for previous semantic element when at camp."
    at "" do (backward-page)
    if at-para do (camp-bwd-paragraph)
    if bk-stce do (camp-bwd-sentence))

  (defcamp camp-text-execute
    "Execute commands when camp is dressed in text mode."
    if bk-stce do (fort)))

(camp-define-keys
 :map text
 :simple ("," 'camp-text
          "t" 'camp-text-down
          "s" 'camp-text-up
          "x" 'camp-text-execute
          "o" 'camp-outline
          "n" 'camp-text-next
          "p" 'camp-text-prev))

(fort-define-keys
 :map text
 :simple ("h" 'mark-whole-buffer))

(provide 'shelter-text)
;;; shelter-text.el ends here
