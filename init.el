; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
;; All `require calls should be turned into use-package
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(use-package company)
(use-package hydra)
(use-package direnv :ensure t)
(use-package projectile)
(use-package smex :ensure t)

(use-package expand-region :ensure t
  :init (  global-set-key (kbd "C-=") 'er/expand-region))

(use-package multiple-cursors :ensure t)

(use-package matcha
  :load-path "~/.emacs.d/matcha/"
  :ensure nil
  :config
  (matcha-setup))

(use-package general
  :ensure t
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   "SPC" 'matcha-me-space))

(use-package counsel-projectile
  :after projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

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
  (load-theme 'leuven t)
  :config
  (progn
    (doom-themes-org-config)))

(use-package circe)

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(setq circe-network-options
      '(("IRCCloud"
         :host "bnc.irccloud.com"
         :server-buffer-name "irccloud"
         :port 6697
         :tls t
         :nick "Wapr"
         :pass  (get-string-from-file "bnc-pass")
         :channels ("#nixos", "#haskell", "#hackeriet"))))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-type 'cabal-repl)

(use-package git-gutter :ensure t)
(use-package evil :ensure t)
(use-package wgrep :ensure t)
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

(setq default-frame-alist '((font . "iosevka-18")))

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
  )

(defhydra hydra-apps ()
  "apps"
  ("d" dired "dired" :exit t)
  )

(defhydra hydra-narrow ()
  "narrow"
  ("d" narrow-to-defun "narrow-to-defun")
  ("n" narrow-to-region "narrow-to-region")
  ("p" narrow-to-page "narrow-to-page")
  ("w" widen "widen")
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
  ("k" projectile-kill-buffers "kill project buffers" :exit t)
  ("f" counsel-projectile "select project file" :exit t)
  ("p" counsel-projectile-switch-project "select project" :exit t)
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

(setq org-tree-slide-slide-in-effect nil)

;; LSP
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-safe-themes
   (quote
    ("4e132458143b6bab453e812f03208075189deca7ad5954a4abb27d5afce10a9a" "2f0cbe053485bccbbbb582acdba7c7c9585ad808ee8ab32f0d727c3d39b42275" "b0fd04a1b4b614840073a82a53e88fe2abc3d731462d6fde4e541807825af342" "f5568ed375abea716d1bdfae0316d1d179f69972eaccd1f331b3e9863d7e174a" "614e5089876ea69b515c50b6d7fa0a37eb7ed50fda224623ec49e1c91a0af6a1" "8047ac280914cbe8dcdc489703c398f0941339cfca77dfc09f3641f1f040267c" "e95ad48fd7cb77322e89fa7df2e66282ade015866b0c675b1d5b9e6ed88649b4" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "a16e816774b437acb78beb9916a60ea236cfcd05784227a7d829623f8468c5a2" "1a6d627434899f6d21e35b85fee62079db55ef04ecd9b70b82e5d475406d9c69" "ef4edbfc3ec509612f3cf82476beddd2aeb3da7bdc3a35726337a0cc838a4ef4" "06e4b3fdcbadc29ff95a7146dee846cd027cfefca871b2e9142b54ad5de4832f" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "155a5de9192c2f6d53efcc9c554892a0d87d87f99ad8cc14b330f4f4be204445" "868abc288f3afe212a70d24de2e156180e97c67ca2e86ba0f2bf9a18c9672f07" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(dhall-format-at-save t)
 '(dhall-format-command "dhall format")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elnode-send-file-program "cat")
 '(haskell-program-name "cabal repl")
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(org-agenda-files (quote ("~/todo.org")))
 '(package-selected-packages
   (quote
    (wgrep expand-region general smex counsel-projectile ivy-rich direnv company-lsp lsp-ui lsp-haskell purescript-mode hyperbole handlebars-sgml-mode evil-goggles evil-googles brutalist-theme prettier-js yasnippet-snippets proof-general command-log-mode psc-ide vue-mode google-this outshine dante darcsum material-theme material git-timemachine yaml-mode which-key use-package shackle scss-mode rjsx-mode restclient rainbow-mode rainbow-delimiters powerline nix-mode multiple-cursors multi-term hydra helm-swoop helm-projectile helm-ag haskell-mode handlebars-mode git-gutter flycheck evil-surround evil-org evil-magit evil-leader evil-escape evil-ediff eshell-git-prompt dhall-mode company beacon auctex ace-jump-mode exwm)))
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
