; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;; All `require calls should be turned into use-package
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(use-package company)
(use-package hydra)
(use-package direnv)
(use-package projectile)
(use-package haskell-mode)
(use-package helm-projectile)

(use-package evil
  :init
  (progn
    (evil-mode 1)
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "SPC")))
    (use-package evil-magit
      :config
      (progn
        (evil-leader/set-key "gs" 'magit-status)))
    (use-package evil-org
      :ensure t
      :init (add-hook 'org-mode-hook 'evil-org-mode))
    (use-package evil-cleverparens
      :ensure t
      :init   (add-hook 'paredit-mode-hook 'evil-cleverparens-mode)
      :config (setq evil-cleverparens-swap-move-by-word-and-symbol t))
    (use-package evil-surround
      :ensure t
      :config
      (progn
        (global-evil-surround-mode 1)
        (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
        (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))))
  :config
  (progn
    (setq evil-cross-lines t)
    (setq evil-move-cursor-back nil)))

(defun eshell-here ()
  (interactive)
  (let* ((parent (projectile-project-root))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "cd " parent))
    (eshell-send-input)))

(evil-leader/set-key "sh" 'eshell-here)

(use-package doom-modeline
      :hook (after-init . doom-modeline-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-nord-light t)
  :config
  (progn
    (doom-themes-org-config)))

(use-package circe)
(use-package hyperbole :ensure t)

(setq circe-network-options
      '(("IRCCloud"
         :host "bnc.irccloud.com"
         :server-buffer-name "irccloud"
         :port 6697
         :tls t
         :nick "Wapr"
         :pass "***REMOVED***"
         :channels ("#nixos", "#haskell", "#hackeriet"))))

;; lsp
;; flymake litters Foo_flymake.hs files everywhere. Make it stop
(setq flymake-run-in-place nil)
  (use-package lsp-mode
    :commands lsp)

  (use-package lsp-ui
    :commands lsp-ui-mode)

  (use-package company-lsp
    :commands company-lsp)

  (use-package lsp-haskell
    :hook ((haskell-mode) .
	   (lambda ()
	     (require 'lsp-haskell)
	     (lsp)))
    :config (setq lsp-haskell-process-path-hie "hie-wrapper")
    :defer t)

;; /lsp
(use-package git-gutter :ensure t)
(use-package evil :ensure t)
(use-package purescript-mode :ensure t :defer t)
(global-git-gutter-mode +1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(use-package psc-ide
  :ensure t
  :init (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation))))

(use-package racket-mode)

 ;; (add-to-list 'display-buffer-alist
 ;;               '("^\\*Help\\*$" display-buffer-reuse-window
 ;;                 (reusable-frames . visible)))

(use-package evil-goggles :config (evil-goggles-mode) :ensure t)

(use-package prettier-js
  :ensure t
  :init (add-hook 'rjsx-mode-hook 'prettier-js-mode))


(add-hook 'javascript-mode-hook 'prettier-js-mode)

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "C-'") #'yas-expand)
  (yas-reload-all)
  (setq yas-prompt-functions '(yas-ido-prompt))
  (defun help/yas-after-exit-snippet-hook-fn ()
    (prettify-symbols-mode)
    (prettify-symbols-mode))
  (add-hook 'yas-after-exit-snippet-hook #'help/yas-after-exit-snippet-hook-fn)
  :diminish yas-minor-mode)

(use-package command-log-mode
  :ensure t)

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; make evil-lispy start in the modes you want
(add-hook 'racket-mode-hook #'evil-lispy-mode)
(use-package flycheck :ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Melpa - only because git-timemachine is broken on nixos

(use-package git-timemachine
:ensure t
:defer t)

(require 'eshell-git-prompt)
(eshell-git-prompt-use-theme 'git-radar)

(which-key-mode t)

(setq default-frame-alist '((font . "iosevka-10")))

(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(evil-mode 1)
(global-evil-surround-mode 1)
(projectile-mode +1)
;; no toolbars
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(display-time)

(setq visible-bell nil
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      default-directory "~"
      fill-column 80
      ediff-split-window-function 'split-window-horizontally)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(set-default 'indent-tabs-mode nil)
(setq display-time-24hr-format t)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make emacs behave sanely (overwrite selected text)
(delete-selection-mode 1)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Theme custom stuff
;;; add mode-line-compaction
;; better defaultsave-mode t)))

(add-hook 'prog-mode-hook 'paredit-mode )
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)

(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)
(global-evil-leader-mode)
(evil-leader/set-leader "=")

(evil-escape-mode 1)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))


(setq evil-normal-state-cursor   '("dodger blue" box)
      evil-insert-state-cursor   '("dodger blue" bar)
      evil-replace-state-cursor  '("dodger blue" hbar)
      evil-operator-state-cursor '("turquoise" box)
      evil-visual-state-cursor   '("orange" box)
      evil-motion-state-cursor   '("deep pink" box)
      evil-emacs-state-cursor    '("red2" box))

(evil-leader/set-key
  "f" 'hydra-files/body
  "r" 'helm-M-x
  "<DEL>" 'helm-M-x
  "g" 'hydra-magit/body
  "v" 'hydra-expand-region/body
  "j" 'hydra-jump/body
  "b" 'hydra-buffers/body
  "a" 'hydra-apps/body
  "p" 'hydra-projectile/body
  "n" 'hydra-narrow/body
  "s" 'hydra-search/body
  "w" 'hydra-window/body
  "e" 'hydra-errors/body
  "x" 'hydra-eval-thing/body
  "t" 'hydra-helm-resume/body
  )

(defhydra hydra-apps ()
  "apps"
  ("d" dired "dired" :exit t)
  )

(defhydra hydra-narrow ()
  "projectile"
  ("d" narrow-to-defun "narrow-to-defun")
  ("n" narrow-to-region "narrow-to-region")
  ("p" narrow-to-page "narrow-to-page")
  ("w" widen "widen")
  )

(defhydra hydra-helm-resume ()
  "helm-misc"
  ("y" helm-show-kill-ring "helm-show-kill-ring" :exit t)
  ("l" helm-resume "helm-resume" :exit t)
  )

(defhydra hydra-search ()
  "projectile"
  ("a p" helm-do-ag-project-root "ag project" :exit t)
  )

(defhydra hydra-eval-thing ()
  "eval"
  ("e" eval-last-sexp "eval-last-sexp" :exit t)
  ("r r" eval-region "eval-region" :exit t)
  ("b" eval-buffer "eval-last-sexp" :exit t)
  ("r p" eval-and-replace "eval-and-replace" :exit t)
  )

(defhydra hydra-errors ()
  "errors"
  ("p" flycheck-previous-error "previous error" )
  ("P" flycheck-next-error "nect error")
  ("n" flycheck-next-error "next error")
  ("N" flycheck-previous-error "previous error")
  )


(defhydra hydra-projectile ()
  "projectile"
  ("f" helm-projectile-find-file "find file in project" :exit t)
  ("h" helm-projectile "helm-projectile" :exit t)
  ("k" projectile-kill-buffers "kill project buffers" :exit t)
  ("p" helm-projectile-switch-project "switch project" :exit t)
  ("s" helm-do-ag-project-root "ag project" :exit t)
  ("w" helm-multi-swoop-projectile "ag project" :exit t)
  ("o" (find-file "~/todo.org") "todo" :exit t)
  )

(defhydra hydra-jump ()
  "jump"
  ("j" ace-jump-char-mode :exit t)
  )

(defhydra hydra-window ()
  "window"
  ("m" delete-other-windows "maximize window" :exit t)
  ("h" evil-window-left "window left" :exit t)
  ("j" evil-window-down "window down" :exit t)
  ("k" evil-window-up "window up" :exit t)
  ("l" evil-window-right "window right" :exit t)
  ("-" evil-window-split "horizontal split" :exit t)
  ("/" evil-window-vsplit "vertical split" :exit t)
  ("f" make-frame-command "make-frame-command" :exit t)
  )

;;; the timemachine hydras should get some color
;;; for better persistence??
(defhydra hydra-magit (:color blue)
  "git carl"
  ("s" magit-status "magit-status" :exit t)
  ("b" magit-blame-addition "magit-blame" :exit t)
  ("t" git-timemachine "git-timemachine" :color pink)
  ("n" git-timemachine-show-next-revision "git-timemachine next" :color pink)
  ("p" git-timemachine-show-previous-revision "git-timemachine prev" :color pink)
  ("q" git-timemachine-quit "git-timemachine-quit")
  )

(defhydra hydra-files ()
  "files"
  ("f" helm-find-files "find files" :exit t)
  ("s"  save-some-buffers "save some buffers" :exit t)
  )

(defhydra hydra-expand-region ()
  "expand region"
  ("v" er/expand-region "expand region")
  ("c"  hydra-contract-region "contract region")
  )

(defun hydra-contract-region ()
  (interactive)
  (let ((current-prefix-arg -1)) ;; emulate C-u
    (call-interactively 'er/expand-region) ;; invoke er/expand-region interactively
    ))

(defhydra hydra-buffers ()
  "buffers"
  ("n" next-buffer "next buffer")
  ("p" previous-buffer "previous buffer")
  ("b" helm-buffers-list "list buffers" :exit t)
  ("o" other-window "other window" :exit t)
  ("k" kill-this-buffer "kill this buffer" :exit t)
  ("d" kill-this-buffer "kill this buffer" :exit t)
  )

(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))


(add-hook 'before-save-hook 'whitespace-cleanup)

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
		     ac-source-variables
		     ac-source-features
		     ac-source-symbols
		     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))

(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(setq helm-ag-base-command "rg --vimgrep --no-heading")
(setq org-tree-slide-slide-in-effect nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(dhall-format-at-save t)
 '(dhall-format-command "dhall format")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elnode-send-file-program "cat")
 '(org-agenda-files (quote ("~/todo.org")))
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui lsp-haskell purescript-mode hyperbole handlebars-sgml-mode evil-goggles evil-googles brutalist-theme prettier-js yasnippet-snippets proof-general command-log-mode psc-ide vue-mode google-this outshine dante darcsum material-theme material git-timemachine yaml-mode which-key use-package shackle scss-mode rjsx-mode restclient rainbow-mode rainbow-delimiters powerline nix-mode multiple-cursors multi-term hydra helm-swoop helm-projectile helm-ag haskell-mode handlebars-mode git-gutter flycheck evil-surround evil-org evil-magit evil-leader evil-escape evil-ediff eshell-git-prompt dhall-mode company beacon auctex ace-jump-mode exwm)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (add-to-list
            (quote exec-path)
            (concat
             (locate-dominating-file default-directory ".dir-locals.el")
             "node_modules/.bin/"))))))
 '(scss-compile-at-save nil)
 '(sgml-validate-command "tidy"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
