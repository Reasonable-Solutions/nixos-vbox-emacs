
; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(require 'company) 
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

;; use fira code in 18
;; TODO replace with Pragmata Pro
(setq default-frame-alist '((font . "Fira Code-12"))) 


(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)

(require 'magit)
(require 'evil)
(require 'evil-magit)

(evil-mode 1)
(require 'projectile)
;; no toolbars
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'leuven t)
(display-time)

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

;; Keep your temporary files in tmp, emacs!
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))

;;;;;;;;;;;;;;;;; Evil keys
(require 'hydra)
(require 'evil)
(require 'projectile)
(require 'helm-projectile)

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
  "p" 'hydra-projectile/body
  "n" 'hydra-narrow/body
  "s" 'hydra-search/body
  "w" 'hydra-window/body
  "e" 'hydra-errors/body
  "x" 'hydra-eval-thing/body
  "r" 'hydra-helm-resume/body
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
 ;; ("t" git-timemachine "git-timemachine")
 ;; ("n" git-timemachine-show-next-revision "git-timemachine next")
 ;; ("p" git-timemachine-show-previous-revision "git-timemachine prev")
 ;; ("q" git-timemachine-quit "git-timemachine-quit")
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
  ("w" exwm-workspace-switch "workspace-switch" :exit t)
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


;; workaround to get ediff to work!
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(set-background-color "grey90")
