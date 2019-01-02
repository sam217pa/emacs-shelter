;; -*- lexical-binding: t -*-

(require 'camp)

(camp-defmacro camp-org-defun-at-heading
  "Define minimenu function for org mode when point is at
heading."
  (and (looking-at org-outline-regexp)
       (looking-back "^\**" (point-at-bol))))

(camp-org-defun-at-heading camp-org-heading
  "Function to encrypt and decrypt at org heading.

Serves as complement of speed commands for org buffers"
  '(("e" 'org-encrypt-entry "encrypt")
    ("d" 'org-decrypt-entry "decrypt")))

;;;###autoload
(camp-define-mode org
  "Minimenu minor mode for org"
  '(("x" . camp-org-heading)))
