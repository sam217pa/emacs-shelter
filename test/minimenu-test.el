;; -*- lexical-binding: t -*-

(require 'minimenu)
(require 'camp)

(defvar minimenu--test-col
  '(("a" . "counsel-colors-web")
    ("b" . "counsel-colors-web")))

(minimenu-call
 '(("e" (:fun (lambda () (interactive) (message "patate")) :desc "encrypt"))
   ("d" (:fun org-decrypt-entry :desc "decrypt"))))

(defun minimenu--test-fun ()
  (interactive)
  (message "this is a minimenu test"))

(let*
    ((col
      '(("e" (lambda () (interactive) (message "patate")))
        ("d" 'minimenu--test-fun "test"))))
  (camp
    bk "\)" call #'forward-sexp))

(not
 (cond
  (t (forward-sexp))
  (else nil)))

(defcamp camp-x ""
  bk "\)" do (message "cuicui")
  at "\]" do (message "patate")
  if (region-active-p) do (message "region"))



(define-key emacs-lisp-mode-map (kbd "ç") 'camp-x)
