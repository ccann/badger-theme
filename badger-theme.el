;;; badger-theme.el --- A theme for Emacs 24

;;; Commentary:
;;; Based loosely on both the wombat and tomorrow themes.

;;; Currently supports:
;;; - [X] font-lock
;;; - [ ] 
;;; - [X] eshell 
;;; - [X] org-mode

;;; Credits: 
;;; I followed Bozhidar Batsov's style in zenburn.el, and used his macro.

;;; Code:

(deftheme badger "The Badger color theme")

;;; Badger Color Pallette
(defvar badger-colors-alist
  '(("badger-blue"     . "#8AC6F2")
    ("badger-fg"       . "#F6F3E8")
    ("badger-bg"       . "#1C1C1C")
    ("badger-bg-1"     . "#171717")
    ("badger-charcoal" . "#656868")
    ("badger-salmon"   . "#F28B86")
    ("badger-violet"   . "#BF93C3")
    ("badger-orange"   . "#EA9847")
    ("badger-green"    . "#86B187")
    ("badger-lime"     . "#84C452")
    ("badger-yellow"   . "#EDEB71")
    ("badger-fg+1"     . "#FBF9F3")
    ("badger-teal"     . "#65A399")
    ("badger-pink"     . "#E18CBB")
    ("badger-brown"    . "#AC8952")
    ("badger-red"      . "#E2434C")
    ("badger-dull-red" . "#A55662")
    
;;    ("badger-bg-1"     . "#170C0C")
    ("badger-bg-05"    . "#1B1B1B")
    ("badger-bg+1"     . "#353535")
    ("badger-dark-violet" . "#635770")
    ("badger-darker-violet"  .   "#433F4F")
    ("badger-dv-invert"   . "#9AA68E")
    ("badger-link"       .  "#8ACDAA")
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

;; >>>>> Built-in
   '(button ((t (:underline t))))
   `(link ((t (:bold t :foreground ,badger-blue :underline t :weight bold))))

;;   `(link-visited ((t (:foreground ,badger-salmon-2 :underline t :weight normal))))

   ;; ordinary text. Its background color is used as the frame's background color. 
   `(default ((t (:foreground ,badger-fg :background ,badger-bg))))

   ;;The :background attribute of this face specifies the color of the text cursor
   `(cursor ((t (:background ,badger-salmon)))) 

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
   `(success ((t (:foreground ,badger-link :weight bold))))
   `(warning ((t (:foreground ,badger-pink :weight bold)))) 

   ;; This face is used for displaying an active region 
   `(region ((t (:background ,badger-charcoal))))

;; >>>>> mode-line
;;   `(mode-line ((t (:background ,badger-dk :foreground ,"gray"))))
   `(mode-line    ((,class (:foreground ,badger-charcoal
                                        :background ,"black"
                                        :box (:line-width -1 :style released-button)))
                   (t :inverse-video t)))

   `(mode-line-inactive ((t (:background ,badger-bg+1 :foreground ,badger-charcoal))))
   `(mode-line-buffer-id ((t (:foreground ,badger-salmon))))
   `(minibuffer-prompt ((t (:foreground ,badger-lime))))
;;   `(mode-line-highlight ((t (:foreground ,badger-lime))))

;; >>>>> powerline
;;   `(powerline-active1 ((t (:background ,"black" :inherit mode-line))))
;;   `(powerline-active2 ((t (:background ,"black" :inherit mode-line))))
;;   `(powerline-inactive1 ((t (:background ,badger-bg-05))))
;;   `(powerline-inactive2 ((t (:background ,badger-bg+1))))

;; >>>>> font-lock
   `(font-lock-keyword-face ((t (:foreground ,badger-blue))))                    
   `(font-lock-builtin-face ((t (:foreground ,badger-orange))))                  
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
   `(eshell-ls-special ((t (:foreground ,badger-blue :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,badger-link :weight bold))))

;; >>>>> Org mode
   `(org-document-info-keyword ((t (:foreground ,badger-dv-invert))))
   `(org-document-title ((t (:foreground ,badger-salmon :height 1.50))))
   `(org-archived ((t (:foreground ,badger-fg :weight bold))))
   `(org-checkbox ((t (:foreground ,badger-fg+1 :foreground ,badger-dv-invert 
                                   :box (:line-width 1 :style released-button)))))
   `(org-done ((t (:foreground ,badger-lime :strike-through t))))
   `(org-todo ((t (:foreground ,badger-red))))
   `(org-formula ((t (:foreground ,badger-violet))))
   `(org-headline-done ((t (:strike-through t :foreground ,badger-charcoal))))
   `(org-hide ((t (:foreground ,badger-bg)))) 
   `(org-level-1 ((t (:foreground ,badger-blue))))
   `(org-level-2 ((t (:foreground ,badger-violet))))
   `(org-level-3 ((t (:foreground ,badger-orange))))
   `(org-level-4 ((t (:foreground ,badger-yellow))))
   `(org-level-5 ((t (:foreground ,badger-salmon))))
   `(org-level-6 ((t (:foreground ,badger-green))))
   `(org-level-7 ((t (:foreground ,badger-brown))))
   `(org-level-8 ((t (:foreground ,badger-teal))))
   `(org-link ((t (:foreground ,badger-link :underline t))))
   
   `(org-agenda-date ((t (:foreground ,badger-blue))))
   `(org-deadline-announce ((t (:foreground ,badger-dull-red))))
   `(org-date ((t (:foreground ,badger-link :underline t))))
   `(org-agenda-date-today  ((t (:foreground ,badger-salmon :slant italic))))
   `(org-agenda-structure  ((t (:inherit font-lock-comment-face))))
   ;; `(org-scheduled ((t (:foreground ,zenburn-green+4))))x
   ;; `(org-scheduled-previously ((t (:foreground ,zenburn-red-4))))
   ;; `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
   ;; `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
   ;; `(org-time-grid ((t (:foreground ,zenburn-orange))))
   ;; `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))

   `(org-special-keyword ((t (:foreground ,badger-dv-invert :weight normal))))
   `(org-table ((t (:foreground ,badger-dv-invert))))
   `(org-tag ((t (:bold t :foreground ,badger-orange :strike-through nil))))
   `(org-warning ((t (:bold t :foreground ,badger-pink :weight bold))))
   `(org-column ((t (:background ,badger-bg-1))))
   `(org-column-title ((t (:background ,badger-bg-1 :foreground ,badger-lime :underline t))))
   `(org-mode-line-clock ((t (:foreground ,badger-yellow))))
   `(org-footnote ((t (:foreground ,badger-link :underline t))))
   `(org-code ((t (:foreground ,badger-dv-invert))))
   `(org-verbatim ((t (:inherit org-code))))

   ))


(provide-theme 'badger)
;;; badger-theme.el ends here
