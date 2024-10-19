;; Bootstrap `straight.el' package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
                (url-retrieve-synchronously
                 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                 'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

;; Aliases that make elisp a bit less ugly in my opinion
(defconst true t)
(defconst false nil)

(setopt straight-use-package-by-default true)
(straight-use-package 'use-package)

;; Sane defaults
(use-package emacs
    :config

    (setq custom-file (locate-user-emacs-file "custom-vars.el"))
    (load custom-file 'noerror 'nomessage)

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

    ;; Takes up precious space
    (tool-bar-mode -1)
    (menu-bar-mode -1)

    (context-menu-mode 1) ;; Show menu on right click

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

    :bind
    ("<f10>" . menu-bar-mode) ;; Toggle menu bar

    :hook
    ((prog-mode . display-line-numbers-mode))) 


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
    :config
    (set-face-attribute
     'default false
     :font "Fira Code"
     :height 110))

(use-package ligature
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Fira Code ligatures in programming modes
    (ligature-set-ligatures
     'prog-mode
     '(
       ;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~ =:= =!=
       ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
       ;; ;; ;;;
       (";" (rx (+ ";")))
       ;; && &&&
       ("&" (rx (+ "&")))
       ;; !! !!! !. !: !!. != !== !~
       ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
       ;; ?? ??? ?:  ?=  ?.
       ("?" (rx (or ":" "=" "\." (+ "?"))))
       ;; %% %%%
       ("%" (rx (+ "%")))
       ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
       ;; |->>-||-<<-| |- |== ||=||
       ;; |==>>==<<==<=>==//==/=!==:===>
       ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]" "-" "=" ))))
       ;; \\ \\\ \/
       ("\\" (rx (or "/" (+ "\\"))))
       ;; ++ +++ ++++ +>
       ("+" (rx (or ">" (+ "+"))))
       ;; :: ::: :::: :> :< := :// ::=
       (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
       ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
       ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!" "="))))
       ;; .. ... .... .= .- .? ..= ..<
       ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
       ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
       ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
       ;; *> */ *)  ** *** ****
       ("*" (rx (or ">" "/" ")" (+ "*"))))
       ;; www wwww
       ("w" (rx (+ "w")))
       ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
       ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
       ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
       ;; << <<< <<<<
       ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!" "-"  "/" "|" "="))))
       ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
       ;; >> >>> >>>>
       (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
       ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
       ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_" (+ "#"))))
       ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
       ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
       ;; __ ___ ____ _|_ __|____|_
       ("_" (rx (+ (or "_" "|"))))
       ;; Fira code: 0xFF 0x12
       ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
       ;; Fira code:
       "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
       ;; The few not covered by the regexps.
       "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
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
    :config
    (setopt whitespace-style '(space-mark tab-mark))
    (global-whitespace-mode 0)) ;; FIXME: only display repeated spaces, like in VSCode

;; Minibuffer
(use-package vertico
    :init
    (vertico-mode 1)
    :bind
    (:map vertico-map
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous) ; FIXME: doesn't work, seems to be overwritten by evil
          ("C-h" . vertico-directory-up)
          ("C-l" . vertico-directory-enter)))

(use-package vertico-posframe
    :after vertico
    :config
    (vertico-posframe-mode 1))

(use-package marginalia
    :config
    (marginalia-mode 1))

(use-package nerd-icons-completion
    :after marginalia
    :config
    (nerd-icons-completion-mode 1)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult)

(use-package orderless
    :config
    ;; `orderless' for partial matches
    ;; `flex' for fuzzy matches
    ;; `basic' is needed for some more obscure completions (eg TRAMP)
    ;; `partial-completion' allows abbreviations like `/u/s/l' for `usr/share/local'
    (setopt completion-styles '(flex orderless basic))
    (setopt completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h x") #'helpful-command)
    (global-set-key (kbd "C-h o") #'helpful-symbol))

(use-package which-key
    :config
    (setopt which-key-popup-type 'side-window)
    (which-key-mode 1))

(use-package corfu
    :hook ((corfu-mode . corfu-popupinfo-mode)
           (prog-mode . corfu-mode)
           (shell-mode . corfu-mode)
           (eshell-mode . corfu-mode))
    :config
    (setopt corfu-cycle true) ;; Loop back around when going past the start/end
    (setopt corfu-auto true) ;; Show completions after typing, without pressing TAB
    (setopt corfu-preselect 'first) ;; Select the first item automatically
    (setopt corfu-scroll-margin 1) ;; Similar to `scroll-margin'
    (setopt corfu-auto-prefix 1) ;; Show completions after typing 1 character
    (setopt corfu-auto-delay 0.1) ;; Make the delay as short as possible
    (setopt corfu-popupinfo-delay 0.1))

(use-package nerd-icons-corfu
    :after corfu
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package treemacs
    :demand true
    :config
    (setopt treemacs-file-follow-delay 0.1)
    (treemacs-follow-mode 1) ;; Update file tree to reflect current buffer
    (treemacs-peek-mode 1))  ;; Preview files on hover
(use-package treemacs-evil :after treemacs)
(use-package treemacs-magit :after treemacs)
(use-package treemacs-nerd-icons :after treemacs
    :config
    (treemacs-load-theme "nerd-icons")
    (treemacs-start-on-boot))

(use-package eshell
    :config
    (setopt eshell-cmpl-ignore-case true)
    (setopt eshell-destroy-buffer-when-process-dies true))

(use-package eat
    :after eshell
    :hook ((eshell-load . eat-eshell-mode)
           (eshell-load . eat-eshell-visual-command-mode)))

(use-package evil-goggles
    :config
    (setopt evil-goggles-duration 0.1)
    (evil-goggles-use-diff-faces)
    (evil-goggles-mode 1))

(use-package evil-commentary
    :config
    (evil-commentary-mode 1))

(use-package centaur-tabs
    :config
    (setopt centaur-tabs-style "alternate")
    (centaur-tabs-mode 1))

(use-package winum
    :config
    (winum-mode 1)
    :bind
    (:map evil-window-map
          ("0" . #'treemacs-select-window)
          ("1" . #'winum-select-window-1)
          ("2" . #'winum-select-window-2)
          ("2" . #'winum-select-window-2)
          ("3" . #'winum-select-window-3)
          ("4" . #'winum-select-window-4)
          ("5" . #'winum-select-window-5)
          ("6" . #'winum-select-window-6)
          ("7" . #'winum-select-window-7)
          ("8" . #'winum-select-window-8)
          ("9" . #'winum-select-window-9)))
