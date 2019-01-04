;; -*- lexical-binding: t -*-

(require 'camp-utils)

(defcamp camp-org-heading
  "Function to encrypt and decrypt at org heading.

Serves as complement of speed commands for org buffers"
  if (and (camp-at org-outline-regexp)
          (camp-bk "^\**"))
  mm '(("e" 'org-encrypt-entry "encrypt")
       ("d" 'org-decrypt-entry "decrypt")))

(camp-defkeymap org
  "Camp minor mode for org"
  '(("x" . camp-org-heading)))

(provide 'camp-org)
