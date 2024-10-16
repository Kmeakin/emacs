;; Aliases that make elisp a bit less ugly in my opinion
(defconst true t)
(defconst false nil)

;; Setup `use-package'
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(require 'use-package-ensure)
(setopt use-package-always-ensure      true
        package-enable-at-startup      false
        use-package-compute-statistics true)

(setopt use-package-always-ensure true  ;; Always ensure packages are installed
        use-package-always-delay  true) ;; Defer loading packages unless demanded

;; Sane defaults
(use-package emacs
    :ensure nil
    :config

    ;; Update built-in packages if newer version in ELPA
    (setopt package-install-upgrade-built-in true)

    ;; Don't ask for confirmation for everything
    (setopt use-short-answers true)
    (setopt use-dialog-box false)
    (setopt confirm-nonexistent-file-or-buffer true)
    (setopt custom-safe-themes true)

    ;; Case-insensitive menus
    (setopt read-file-name-completion-ignore-case true)
    (setopt read-buffer-completion-ignore-case true)
    (setopt completion-ignore-case true)

    ;; Opening files
    (recentf-mode 1)                 ;; Save recently opened files
    (savehist-mode 1)                ;; Save minibuffer history
    (save-place-mode 1)              ;; Save location in opened files
    (setopt make-backup-files false) ;; Don't litter backups everywhere

    ;; Indentation
    (setopt indent-tabs-mode false)      ;; Insert spaces instead of tabs
    (setopt lisp-body-indent 4)          ;; Ident lisp code with 4 spaces instead of 2
    (setopt tab-width 4)                 ;; Render tabs as 4 spaces
    (setopt tab-stop-list '(4))          ;; Tab button inserts 4 spaces
    (setopt tab-always-indent 'complete) ;; Tab button tries to indent, then tries to complete

    ;; Reload files automatically
    (global-auto-revert-mode 1)
    (setopt global-auto-revert-non-file-buffers true)

    ;; Scrolling
    ;; Scroll when there are 5 lines at the top/bottom of the page
    (setopt scroll-margin 5)
    ;; Scroll up/down by 1 line rather than recentering when point goes off the page
    (setopt scroll-step   1)
    ;; Keep point in place while scrolling
    (setopt scroll-preserve-screen-position true)
    ;; Scroll in pixel increments rather than lines
    (pixel-scroll-precision-mode 1)
    ;; Scroll with mouse even in terminal!
    (xterm-mouse-mode 1)

    ;; Line numbers in fringe
    (setopt display-line-numbers-type 'relative)
    (global-display-line-numbers-mode 1))

;; Theme
(use-package doom-themes
    :config
    (load-theme 'doom-gruvbox t))

(use-package doom-modeline
    :config
    (column-number-mode 1) ;; Column numbers in modeline
    (doom-modeline-mode 1))

;; Font
(use-package emacs
    :ensure nil
    :config
    (set-face-attribute
     'default false
     :font "Fira Code"
     :height 110))

(use-package ligature
    :config
    (ligature-set-ligatures
     true
     '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
    (global-ligature-mode 1))

;; Vim keybindings
(use-package evil
    :init
    (setopt evil-want-keybinding false) ;; Needs to be set *before* `evil' is loaded
    :config
    (setopt evil-want-integration true)
    (setopt evil-want-minibuffer true)
    (setopt evil-want-C-u-scroll true)
    (setopt evil-undo-system 'undo-redo)
    (setopt evil-kill-on-visual-paste false)
    (setopt evil-mode-beyond-eol true)
    (evil-mode 1))

(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

;; Formatting
(use-package whitespace-cleanup-mode
    :config (global-whitespace-cleanup-mode 1))

(use-package emacs
    :ensure nil
    :config
    (setopt whitespace-style '(space-mark tab-mark))
    (global-whitespace-mode 0)) ;; FIXME: only display repeated spaces, like in VSCode

;; Minibuffer
(use-package vertico
    :config
    (vertico-mode 1))

(use-package marginalia
    :config
    (marginalia-mode 1))

(use-package consult)

(use-package helpful
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h x") #'helpful-command)
    (global-set-key (kbd "C-h o") #'helpful-symbol))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helpful consult marginalia vertico whitespace-cleanup-mode evil-collection evil ligature doom-modeline doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
