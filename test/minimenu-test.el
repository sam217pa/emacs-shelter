;; -*- lexical-binding: t -*-

(require 'minimenu)
(require 'camp)

(minimenu-call
 '(("e" (:fun (lambda () (interactive) (message "patate")) :desc "message"))
   ("d" (:fun counsel-colors-web :desc "colors web"))))

(defun minimenu--test-fun ()
  (interactive)
  (message "this is a minimenu test"))

(let*
    ((col
      '(("e" (lambda () (interactive) (message "patate")))
        ("d" minimenu--test-fun "test"))))
  (camp
    bk "\)" mm col))

(minimenu--rectify-col
 '(("e" (lambda () (interactive) (message "patate")))
   ("d" minimenu--test-fun "test")))

(defun camp-x ()
  (interactive)
  (message "%s" (this-single-command-raw-keys)))

(defcamp camp-x ""
  if rom do (fort)
  bk "\)" do (fort)
  at "\]" do (message "patate")
  if reg do (message "region")) 

(define-key emacs-lisp-mode-map (kbd "x") 'camp-x)

(define-key fort-emacs-lisp-map (kbd "SPC s") 'save-buffer)

(fort-define-keys
 ("b b" 'ivy-switch-buffer
  "b s" 'save-buffer))