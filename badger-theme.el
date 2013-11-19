
;;; badger-theme.el --- An extension of the wombat color theme for Emacs 24

;;; Commentary:

;;; I got frustrated with the wombat theme in Emacs, so I changed it.

;;; Credits:

;;; Code:

(deftheme badger "The Badger color theme")

;;; Badger Color Pallette

(defvar badger-colors-alist
  '(("badger-kw" . "#8AC6F2")
    ("badger-fg" . "#F6F3E8")
    ("badger-fg+1" . "#FBF9F3")
    ("badger-bg-1" . "#170C0C")
    ("badger-bg-05" . "#1B1B1B")
    ("badger-bg" . "#242424")
    ("badger-bg+1" . "#353535")
    ("badger-orange" . "#E5786D")
    ("badger-orange-2" . "#962524")
    ("badger-warn" . "magenta")
    ("badger-succ" . "cyan"))
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


(badger-with-color-variables 
  (custom-theme-set-faces
   'badger
;;;;;; Built-in
   '(button ((t (:underline t))))
   `(link ((t (:bold t :foreground ,badger-kw :underline t :weight bold))))
   `(link-visited ((t (:foreground ,badger-orange-2 :underline t :weight normal))))
   `(default ((t (:foreground ,badger-fg :background ,badger-bg))))
   `(cursor ((t (:foreground ,badger-fg :background ,badger-fg+1)))) 
   `(escape-glyph ((t (:foreground ,badger-orange :bold t))))
   `(fringe ((t (:foreground ,badger-fg :background ,badger-bg+1))))
   `(header-line ((t (:foreground ,badger-orange 
                                  :background ,badger-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,badger-bg-05))))
   `(success ((t (:foreground ,badger-succ :weight bold))))
   `(warning ((t (:foreground ,badger-warn :weight bold)))) 
   
   

   ))

(provide-theme 'badger)
;;; badger-theme.el ends here
