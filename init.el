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

(require 'eshell-git-prompt)
(eshell-git-prompt-use-theme 'git-radar)

(which-key-mode t)
;; use fira code in 18
;; TODO replace with Pragmata Pro
(setq default-frame-alist '((font . "Fira Code-12"))) 


(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(evil-mode 1) 
(projectile-mode +1) 
(projectile-global-mode t) 
;; no toolbars
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'leuven t)
(display-time)

;; better defaults
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
  ("w" helm-multi-swoop-project "ag project" :exit t)
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

;; Hydras don't work well with exwm
;; consider setting exwm-passthrough-something t 
;; (defhydra hydra-exwm (global-map "s-l")
;;   "exwm"
;;   ("h" (exwm-layout-enlarge-window 10) "enlarge buffer")
;;   ("j" (exwm-layout-enlarge-window-horizontally 10) "enlarge")
;;   ("k" (exwm-layout-shrink-window-horizontally 10) "shrink")
;;   ("l" (exwm-layout-shrink-window 10) "shrink")
;;   ("t" exwm-layout-toggle-mode-line "toogle modeline" :exit t)
;;   ("b" balance-windows "balance windows" :exit t)
;;   )

(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

;; workaround to get ediff to work!
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(set-background-color "grey90")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-timemachine yaml-mode which-key use-package shackle scss-mode rjsx-mode restclient rainbow-mode rainbow-delimiters powerline nix-mode multiple-cursors multi-term hydra helm-swoop helm-projectile helm-ag haskell-mode handlebars-mode git-gutter flycheck evil-surround evil-org evil-magit evil-leader evil-escape evil-ediff eshell-git-prompt dhall-mode company beacon auctex ace-jump-mode exwm)))
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
