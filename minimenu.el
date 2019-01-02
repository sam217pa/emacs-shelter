;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defcustom minimenu-max-lines 5
  "Maximum number of lines to be displayed in minimenus."
  :type 'integer
  :group 'minimenu)

(defvar minimenu-ov nil
  "Default minimenu overlay.

Is redefined each time an overlay is required by a minimenu
function.")

(defvar minimenu--old-cursor-type nil
  "Cursor type variable to save the old cursor type.

It can be restored after the overlay has been cleaned up.")

(defun minimenu-ov-cleanup ()
  "Reset the minimenu overlay.

Set the ``minimenu-ov'' to nil and delete the corresponding
overlay."
  (when (overlayp minimenu-ov)
    (delete-overlay minimenu-ov)
    (setq minimenu-ov nil)
    (unless cursor-type
      (setq cursor-type minimenu--old-cursor-type))))

(defun minimenu--overlay (str)
  "Display STR in the ``minimenu-ov'' overlay."
  (let ((beg (if (bolp) (point) (1- (point))))
        (end (line-end-position)))
    (when cursor-type
      (setq minimenu--old-cursor-type cursor-type)
      (setq cursor-type nil))
    (setq minimenu-ov (make-overlay beg end))
    (overlay-put minimenu-ov 'display str)
    (overlay-put minimenu-ov 'before-string "\n")
    (overlay-put minimenu-ov 'after-string "")
    (overlay-put minimenu-ov 'priority 9999)))

(defsubst minimenu--unpropertize (str)
  (substring-no-properties str))

(defun minimenu--propertize (str &rest arg)
  (propertize (minimenu--unpropertize str) 'face arg))

(defun minimenu--propertize-key (key-value)
  "Add text properties to the key in KEY-VALUE."
  (minimenu--propertize (car key-value) 'bold))

(defun minimenu--propertize-value (key-value max-pad)
  "Add text properties to the value in KEY-VALUE."
  (let*
      ((v (cdr key-value))
       (text (if (symbolp v) (symbol-name v) v))
       (str (concat text (make-string (- max-pad (length text)) ?\s))))
    (minimenu--propertize str 'highlight)))

(defmacro minimenu--propertize-bg (str)
  "Rectify overlay background to default background color"
  `(propertize
   ,str
   'font-lock-face `(:background ,(face-background 'default))))

(defun minimenu--propertize-key-value (key-value max-pad)
  "Returns a correctly formatted and propertized key-functions.

MAX-PAD is the length of the longest string in KEY-VALUE, so that
highlight of shorter string are of the same length."
  (if (null key-value)
      ""
    (let*
        ((key (minimenu--propertize-key key-value))
         (fun (minimenu--propertize-value key-value max-pad))
         (str (format "%s - %s" key fun)))
      (minimenu--propertize-bg str))))

(defsubst minimenu--max-text-length (col)
  "Returns the maximum text length of collection."
  (apply #'max (cl-loop for x in col
                        collect (length (cdr x)))))

(defun minimenu--group (source n)
  (if (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (cl-subseq source 0 n) acc))
                       (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun minimenu--map2concat (col padding f &optional args)
  (mapconcat
   #'identity
   (mapcar
    (lambda (x)
      (format "%s %s"
              (funcall f (car x) args)
              (funcall f (cadr x) args)))
    (minimenu--group col 2))
   (concat "\n" padding)))

(defun minimenu--flatten-col (col padding)
  "Flatten the collection COL and add PADDING if necessary.

Return collection COL as a single string that is left padded
so that text is displayed aligned to the point.

Text is propertized correctly."
  (minimenu--propertize-bg
   (concat
    padding
    (minimenu--map2concat
     col padding
     #'minimenu--propertize-key-value
     (minimenu--max-text-length col))
    "\n")))


(defun minimenu--left-pad ()
  "Returns an empty string of width corresponding to number of
characters between point and beginning of line."
  (let ((width (- (point) (point-at-bol))))
    (if (bolp) "" (make-string (1- width) ?\s))))

(defun minimenu--display (col)
  "Display collection left padded in the ``minimenu-ov''
overlay."
  (let*
      ((padding (minimenu--left-pad))
       (str (minimenu--flatten-col col padding)))
    (minimenu--overlay str)))

(defun minimenu--call-candidate-function (key col)
  "Call the function associated to KEY in COL."
  (let ((x (assoc key col)))
    (if x
        (let ((f (cdr x)))
          (call-interactively (if (symbolp f) f (intern f))))
      (message "Quit"))))

(defmacro minimenu--get-fun (x)
  `(plist-get (cadr ,x) :fun))

(defmacro minimenu--get-desc (x)
  `(plist-get (cadr ,x) :desc))

(defun minimenu--make-collection (col)
  (cl-loop
   for x in col
   collect
   (cons (car x)
         (minimenu--get-fun x))))

(defun minimenu--make-print (col)
  (cl-flet ((get-desc (x)
             (or (minimenu--get-desc x)
                 (symbol-name (minimenu--get-fun x)))))
    (cl-loop
     for x in col
     collect (cons (car x) (get-desc x)))))

(defun minimenu--sanitize-key-desc (col)
  "Returns the plist representation of keys in COL."
  (cl-flet*
      ((f (fun) (intern (symbol-name (cadr fun))))
       (g (fun) (symbol-name (cadr fun)))
       (h (x) (list
               (elt x 0)
               `(:fun  ,(f (elt x 1))
                 :desc ,(or (elt x 2) (g (elt x 1)))))))
    (cl-loop for x in col collect (h x))))

(defsubst minimenu--plist-p (col)
  "Check that every element of COL has a :fun property."
  (cl-loop
   for x in col
   always (plist-member (cadr x) :fun)))

(defun minimenu--rectify-col (col)
  "Returns the plist representation of keys in COL.

Checks that COL is a plist representation of keys-functions and
if not, sanitize them with ``minimenu--sanitize-key-desc''."
  (if (minimenu--plist-p col)
      col
    (or (minimenu--sanitize-key-desc col)
        (error "At least one element of COL have no :fun."))))

;;;###autoload
(defun minimenu--call (col)
  "Display COL alist in an overlay and call the function
associated to input key."
  (let*
      ((col (minimenu--rectify-col col))
       ;; col-print is the printed representation of col
       (col-print (minimenu--make-print col))
       ;; col-fun is the alist representation of col
       (col-fun (minimenu--make-collection col)))
    ;; display the collection
    (minimenu--display col-print)
    ;; read input key
    (let ((key (single-key-description
                (read-key "Choose minimenu key ..."))))
      (minimenu-ov-cleanup)
      (cond
       ((equal key "C-g")
        (message "Quit"))
       (t
        (minimenu--call-candidate-function key col-fun))))))

(provide 'minimenu)
