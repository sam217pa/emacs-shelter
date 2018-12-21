;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar minimenu--test-col '(("a" . "counsel-colors-web")
                             ("b" . "counsel-colors-web")))

(defvar minimenu-ov nil
  "minimenu overlay")

(defun minimenu-ov-cleanup ()
  (when (overlayp minimenu-ov)
    (delete-overlay minimenu-ov)
    (setq minimenu-ov nil)))

(defun minimenu-overlay (str)
  (setq minimenu-ov (make-overlay (1- (point)) (line-end-position)))
  (overlay-put minimenu-ov 'display str)
  (overlay-put minimenu-ov 'after-string ""))

(defun minimenu--propertize-key (key-value)
  (propertize (car key-value)
              'face 'underline))

(defun minimenu--propertize-value (key-value)
  (let* ((v (cdr key-value))
         (text (if (symbolp v)
                   (symbol-name v)
                 v)))
    (propertize text 'face 'bold-italic)))

(defun minimenu--flatten-col (col padding)
  (mapconcat
   (lambda (x)
     (let
         ((key (minimenu--propertize-key x))
          (fun (minimenu--propertize-value x)))
       (format "%s - %s" key fun)))
   col
   (concat "\n" padding)))

(defun minimenu--display (col)
  (let*
      ((padding (make-string (1- (- (point) (point-at-bol))) ?\s))
       (str (minimenu--flatten-col col padding)))
    (minimenu-overlay str)))

(defun minimenu--call-candidate-function (key col)
  (let ((x (assoc key col)))
    (if x
        (let ((f (cdr x)))
          (call-interactively (if (symbolp f) f (intern f))))
      (user-error "Wrong input key"))))

(defun minimenu--validate-col (col)
  (unless
      (and (cl-loop for x in col always (consp x))
           (cl-loop for x in col always (not (listp (cdr x)))))
    (error "Wrong data type")))

(defun minimenu--call (col)
  ;; (minimenu--validate-col col)
  (let ((col-print (minimenu--make-print col))
        (col-fun (minimenu--make-collection col)))
    (minimenu--display col-print)
    (let ((key (single-key-description (read-key "Chose key: "))))
      (minimenu-ov-cleanup)
      (cond
       ((equal key "C-g")
        (message "Quit"))
       (t
        (minimenu--call-candidate-function key col-fun))))
    (minimenu-ov-cleanup)))

(defun minimenu--make-collection (col)
  (cl-loop
   for x in col
   collect
   (cons (car x)
         (plist-get (cadr x) :fun))))

(defun minimenu--make-print (col)
  (cl-flet ((get-desc (x)
             (or (plist-get (cadr x) :desc)
                 (symbol-name (plist-get (cadr x) :fun)))))
    (cl-loop
     for x in col
     collect (cons (car x) (get-desc x)))))

(provide 'minimenu)
