;;; badger-theme.el --- An extension of the wombat color theme for Emacs 24

;;; Commentary:

;;; I got frustrated with the wombat theme in Emacs, so I changed it.

;;; Credits:

;;; Code:

(deftheme badger "The Badger color theme")

;;; Badger Color Pallette

(defvar badger-colors-alist
  '(("badger"))
  "List of Badger colors. 
Each element has the form (NAME . HEX).")

;; Borrowed from Bozhidar Batsov
(defmacro badger-with-color-variables (&rest body)
  "`let' bind all colors defined in `badger-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   badger-colors-alist))
     ,@body))

(provide 'badger-theme)
;;; badger-theme.el ends here
