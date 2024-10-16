;; Bootstrap elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install `use-package' support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Aliases that make elisp a bit less ugly in my opinion
(defconst true t)
(defconst false nil)

(setopt use-package-always-ensure true  ;; Always ensure packages are installed
        use-package-always-delay  true) ;; Defer loading packages unless demanded

;; Sane defaults
(use-package emacs
    :ensure nil
    :config
    ;; Don't ask for confirmation for everything
    (setopt use-short-answers true)
    (setopt use-dialog-box false)
    (setopt confirm-nonexistent-file-or-buffer true)
    (setopt custom-safe-themes true)

    ;; Opening files
    (recentf-mode 1)                 ;; Save recently opened files
    (savehist-mode 1)                ;; Save minibuffer history
    (save-place-mode 1)              ;; Save location in opened files
    (setopt make-backup-files false) ;; Don't litter backups everywhere

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
