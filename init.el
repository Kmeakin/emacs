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

(setq use-package-always-ensure t  ;; Always ensure packages are installed
      use-package-always-delay  t) ;; Defer loading packages unless demanded

;; Sane defaults
(use-package emacs
    :ensure nil
    :config
    ;; Don't ask for confirmation for everything
    (setq use-short-answers t)
    (setq use-dialog-box nil)
    (setq confirm-nonexistent-file-or-buffer nil)
    (setq custom-safe-themes t)

    ;; Opening files
    (recentf-mode 1)               ;; Save recently opened files
    (savehist-mode 1)              ;; Save minibuffer history
    (save-place-mode 1)            ;; Save location in opened files
    (setq make-backup-files nil)   ;; Don't litter backups everywhere

    (global-auto-revert-mode 1)    ;; Reload files automatically
    (setq global-auto-revert-non-file-buffers t))

;; Theme
(use-package doom-themes
    :config
    (load-theme 'doom-gruvbox t))

(use-package doom-modeline
    :config
    (doom-modeline-mode 1))

;; Vim keybindings
(use-package evil
    :custom
    (evil-want-integration t)
    (evil-want-minibuffer t)
    (evil-want-keybinding nil)
    (evil-want-C-u-scroll t)
    (evil-undo-system 'undo-redo)
    (evil-kill-on-visual-paste nil)
    (evil-mode-beyond-eol t)
    :config
    (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
