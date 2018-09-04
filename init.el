; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'company)
(require 'hydra)
(require 'evil)
(require 'projectile)
(require 'haskell-mode)
(require 'helm-projectile)
(require 'helm-config)
(require 'magit)
(require 'evil)
(require 'evil-magit)
(require 'projectile)
(require 'powerline)
(powerline-default-theme)
(require 'git-gutter)
(global-git-gutter-mode +1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(setq haskell-process-wrapper-function
	(lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))

(setq flycheck-command-wrapper-function
	(lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
      flycheck-executable-find
	(lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Melpa - only because git-timemachine is broken on nixos
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'use-package)
(use-package git-timemachine
:ensure t
:defer t)

(use-package material-theme
:ensure t
:defer t)


(require 'eshell-git-prompt)
(eshell-git-prompt-use-theme 'git-radar)

(which-key-mode t)

;; TODO replace with Pragmata Pro
(setq default-frame-alist '((font . "iosevka-16")))


(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(evil-mode 1)
(projectile-mode +1)
(projectile-global-mode t)
;; no toolbars
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'material-light t)
(display-time)

;; better defaultsave-mode t)))

;;;;;;;;;;;;;;;; Evil keys
(helm-projectile-on)

(add-hook 'after-init-hook 'global-company-mode)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

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
  "<SPC>" 'helm-M-x
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
  "r" 'hydra-helm-resume/body
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
  "projectile"
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
  ("o" (find-file "~/todo/todo.org") "todo" :exit t)
  )

(defhydra hydra-jump ()
  "jump"
  ("j" ace-jump-mode :exit t)
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

(defhydra hydra-magit ()
  "magit"
  ("s" magit-status "magit-status" :exit t)
  ("b" magit-blame "magit-blame" :exit t)
  ("t" git-timemachine "git-timemachine")
  ("n" git-timemachine-show-next-revision "git-timemachine next")
  ("p" git-timemachine-show-previous-revision "git-timemachine prev")
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

;;----------------------------------------------------------------------------
;; Reason setup
;;----------------------------------------------------------------------------

(defun shell-cmd (cmd)
  "Returns the stdout output of a shell command or nil if the command returned
   an error"
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

(defun reason-cmd-where (cmd)
  (let ((where (shell-cmd cmd)))
    (if (not (string-equal "unknown flag ----where" where))
      where)))

(let* ((refmt-bin (or (reason-cmd-where "refmt ----where")
		      (shell-cmd "which refmt")))
       (merlin-bin (or (reason-cmd-where "ocamlmerlin ----where")
		       (shell-cmd "which ocamlmerlin")))
       (merlin-base-dir (when merlin-bin
			  (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))

  ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (when merlin-bin
    (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
    (setq merlin-command merlin-bin))

  (when refmt-bin
    (setq refmt-command refmt-bin)))

(require 'reason-mode)
(require 'merlin)
(add-hook 'reason-mode-hook (lambda ()
			      (add-hook 'before-save-hook 'refmt-before-save)
			      (merlin-mode)))

(setq merlin-ac-setup t)

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

;; workaround to get ediff to work!
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (darcsum material-theme material git-timemachine yaml-mode which-key use-package shackle scss-mode rjsx-mode restclient rainbow-mode rainbow-delimiters powerline nix-mode multiple-cursors multi-term hydra helm-swoop helm-projectile helm-ag haskell-mode handlebars-mode git-gutter flycheck evil-surround evil-org evil-magit evil-leader evil-escape evil-ediff eshell-git-prompt dhall-mode company beacon auctex ace-jump-mode exwm)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (add-to-list
	    (quote exec-path)
	    (concat
	     (locate-dominating-file default-directory ".dir-locals.el")
	     "node_modules/.bin/"))))))
 '(scss-compile-at-save nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
