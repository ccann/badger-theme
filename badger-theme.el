
;;; badger-theme.el --- An extension of the wombat color theme for Emacs 24

;;; Commentary:

;;; I got frustrated with the wombat theme in Emacs, so I changed it.

;;; Credits:

;;; Code:

(deftheme badger "The Badger color theme")

;;; Badger Color Pallette

(defvar badger-colors-alist
  '(("badger-blue"     . "#8AC6F2")
    ("badger-fg"       . "#F6F3E8")
    ("badger-bg"       . "#242424")
    ("badger-charcoal" . "#656868")
    ("badger-salmon"   . "#F28B86")
    ("badger-violet"   . "#BF93C3")
    ("badger-orange"   . "#EA9847")
    ("badger-green"    . "#86B187")
    ("badger-lime"     . "#84C452")
    ("badger-yellow"   . "#EDEB71")
    ("badger-fg+1"     . "#FBF9F3")
;;    ("badger-bg-1"     . "#170C0C")
    ("badger-bg-05"    . "#1B1B1B")
    ("badger-bg+1"     . "#353535")
    ("badger-dark-violet" . "#635770")
    ("badger-dv-invert"   . "#9AA68E")
    ("badger-warn"     . "magenta")
    ("badger-succ"     . "cyan")
    )
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
   `(link ((t (:bold t :foreground ,badger-blue :underline t :weight bold))))

;;   `(link-visited ((t (:foreground ,badger-salmon-2 :underline t :weight normal))))

   ;; ordinary text. Its background color is used as the frame's background color. 
   `(default ((t (:foreground ,badger-fg :background ,badger-bg))))

   ;;The :background attribute of this face specifies the color of the text cursor
   `(cursor ((t (:background ,badger-fg+1)))) 

   ;; The face for displaying control characters and escape sequences
   `(escape-glyph ((t (:foreground ,badger-salmon :bold t))))

   ;; The face for the narrow fringes to the left and right of windows on graphic displays.
   `(fringe ((t (:foreground ,badger-fg :background ,badger-bg-05))))

   ;; fixed line displayed at the top of the emacs window, not in XEmacs
   ;; `(header-line ((t (:foreground ,badger-salmon 
   ;;                                :background ,badger-bg-1
   ;;                                :box (:line-width -1 :style released-button)))))

   ;;text highlighting in various contexts, when the mouse cursor is moved over a hyperlink. 
   `(highlight ((t (:background ,badger-bg+1))))

   ;; “lazy matches” for Isearch and Query Replace (matches other than the current one). 
   ;; `(lazy-highlight ((t )))
   `(success ((t (:foreground ,badger-succ :weight bold))))
   `(warning ((t (:foreground ,badger-warn :weight bold)))) 

   ;; This face is used for displaying an active region 
   `(region ((t (:background ,badger-charcoal))))

;; >>>>> mode-line
   `(mode-line ((t (:background ,badger-dark-violet :foreground "black"))))
   `(mode-line-inactive ((t (:background ,badger-bg+1 :foreground ,badger-fg))))
   `(mode-line-buffer-id ((t (:foreground ,badger-lime))))
   `(minibuffer-prompt ((t (:foreground ,badger-lime))))
   
;; >>>>> font-lock
   `(font-lock-keyword-face ((t (:foreground ,badger-blue))))                    
   `(font-lock-builtin-face ((t (:foreground ,badger-orange))))                  
;;   `(font-lock-constant-face ((t (:foreground ,badger-salmon))))               
;;   `(font-lock-doc-face ((t (:foreground ,badger-charcoal))))                    
   `(font-lock-comment-face ((t (:foreground ,badger-charcoal))))
   `(font-lock-function-name-face ((t (:foreground ,badger-salmon :weight normal)))) 
   `(font-lock-string-face ((t (:foreground ,badger-green))))
   `(font-lock-variable-name-face ((t (:foreground ,badger-violet))))
   `(font-lock-type-face ((t (:foreground ,badger-yellow))))   
   `(font-lock-constant-face ((t (:foreground ,badger-lime))))
   `(font-lock-warning-face ((t (:foreground ,badger-yellow :weight bold))))

;; >>>>> eshell 
   `(eshell-prompt ((t (:foreground ,badger-lime))))
   `(eshell-ls-archive ((t (:foreground ,badger-orange :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,badger-violet :weight normal))))
   `(eshell-ls-executable ((t (:foreground ,badger-yellow :weight normal))))
   `(eshell-ls-unreadable ((t (:foreground ,badger-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,badger-succ :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,badger-blue :weight bold))))

;; >>>>> Org mode
   ;; `(org-agenda-date-today
   ;;   ((t (:foreground ,zenburn-fg+1 :slant italic :weight bold))) t)
   ;; `(org-agenda-structure
   ;;   ((t (:inherit font-lock-comment-face))))
   ;; `(org-archived ((t (:foreground ,zenburn-fg :weight bold))))
   ;; `(org-checkbox ((t (:background ,zenburn-bg+2 :foreground ,zenburn-fg+1
   ;;                                 :box (:line-width 1 :style released-button)))))
   ;; `(org-date ((t (:foreground ,zenburn-blue :underline t))))
   ;; `(org-deadline-announce ((t (:foreground ,zenburn-red-1))))
   ;; `(org-done ((t (:bold t :weight bold :foreground ,zenburn-green+3))))
   ;; `(org-formula ((t (:foreground ,zenburn-yellow-2))))
   ;; `(org-headline-done ((t (:foreground ,zenburn-green+3))))
   ;; `(org-hide ((t (:foreground ,zenburn-bg-1))))
   ;; `(org-level-1 ((t (:foreground ,zenburn-orange))))
   ;; `(org-level-2 ((t (:foreground ,zenburn-green+4))))
   ;; `(org-level-3 ((t (:foreground ,zenburn-blue-1))))
   ;; `(org-level-4 ((t (:foreground ,zenburn-yellow-2))))
   ;; `(org-level-5 ((t (:foreground ,zenburn-cyan))))
   ;; `(org-level-6 ((t (:foreground ,zenburn-green+2))))
   ;; `(org-level-7 ((t (:foreground ,zenburn-red-4))))
   ;; `(org-level-8 ((t (:foreground ,zenburn-blue-4))))
   ;; `(org-link ((t (:foreground ,zenburn-yellow-2 :underline t))))
   ;; `(org-scheduled ((t (:foreground ,zenburn-green+4))))
   ;; `(org-scheduled-previously ((t (:foreground ,zenburn-red-4))))
   ;; `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
   ;; `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
   ;; `(org-special-keyword ((t (:foreground ,zenburn-fg-1 :weight normal))))
   ;; `(org-table ((t (:foreground ,zenburn-green+2))))
   ;; `(org-tag ((t (:bold t :weight bold))))
   ;; `(org-time-grid ((t (:foreground ,zenburn-orange))))
   ;; `(org-todo ((t (:bold t :foreground ,zenburn-red :weight bold))))
   ;; `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((t (:bold t :foreground ,zenburn-red :weight bold :underline nil))))
   ;; `(org-column ((t (:background ,zenburn-bg-1))))
   ;; `(org-column-title ((t (:background ,zenburn-bg-1 :underline t :weight bold))))
   ;; `(org-mode-line-clock ((t (:foreground ,zenburn-fg :background ,zenburn-bg-1))))
   ;; `(org-mode-line-clock-overrun ((t (:foreground ,zenburn-bg :background ,zenburn-red-1))))
   ;; `(org-ellipsis ((t (:foreground ,zenburn-yellow-1 :underline t))))
   ;; `(org-footnote ((t (:foreground ,zenburn-cyan :underline t))))

   

   ))


(provide-theme 'badger)
;;; badger-theme.el ends here
