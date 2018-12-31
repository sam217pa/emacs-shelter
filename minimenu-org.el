;; -*- lexical-binding: t -*-

(require 'minimenu)
(require 'camp)

(camp-defmacro minimenu-org-defun-heading
  "Define minimenu function for org mode when point is at
heading."
  (and (looking-at org-outline-regexp)
       (looking-back "^\**")))

(minimenu-org-defun-heading mm-org-heading
  "Function to encrypt and decrypt at org heading.

Serves as complement of speed commands for org buffers"
  '(("e" 'org-encrypt-entry "encrypt")
    ("d" 'org-decrypt-entry "decrypt")))

;;;###autoload
(minimenu-define-minor-mode org
  "Minimenu minor mode for org"
  '(("x" . mm-org-heading)))
