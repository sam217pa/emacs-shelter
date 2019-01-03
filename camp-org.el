;; -*- lexical-binding: t -*-

(require 'camp)

(defcamper camp-org-defun-at-heading
  "Define minimenu function for org mode when point is at
heading."
  (and (camp-at org-outline-regexp)
       (camp-bk "^\**")))

(camp-org-defun-at-heading camp-org-heading
  "Function to encrypt and decrypt at org heading.

Serves as complement of speed commands for org buffers"
  '(("e" 'org-encrypt-entry "encrypt")
    ("d" 'org-decrypt-entry "decrypt")))

;;;###autoload
(camp-define-mode org
  "Camp minor mode for org"
  '(("x" . camp-org-heading)))
