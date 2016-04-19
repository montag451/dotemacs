(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(let ((packages '(ace-jump-mode
                  anaconda-mode
                  cider
                  cmake-mode
                  company
                  company-anaconda
                  company-quickhelp
                  evil
                  evil-leader
                  evil-nerd-commenter
                  evil-paredit
                  fill-column-indicator
                  ggtags
                  haskell-mode
                  helm
                  helm-projectile
                  helm-swoop
                  hydra
                  magit
                  markdown-mode
                  multi-term
                  ox-gfm
                  paredit
                  projectile
                  tangotango-theme
                  restclient
                  restclient-helm
                  rust-mode
                  virtualenvwrapper))
      (need-refresh t))
  (dolist (package packages)
    (if (not (package-installed-p package))
        (progn
          (if need-refresh
	      (progn
		(package-refresh-contents)
		(setq need-refresh nil)))
          (package-install package)))))

;; load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; hydra for window manipulation
(defhydra hydra-window (global-map "C-x w" :color amaranth)
  "Window manipulation"
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  ("u" winner-undo "undo")
  ("r" winner-redo "redo")
  ("s" (let ((win (split-window-horizontally)))
         (select-window win)) "split horizontally")
  ("v" (let ((win (split-window-below)))
         (select-window win)) "split vertically")
  ("+" enlarge-window "enlarge vertically")
  ("-" shrink-window "shrink vertically")
  (">" enlarge-window-horizontally "enlarge horizontally")
  ("<" shrink-window-horizontally "shrink vertically")
  ("=" balance-windows "balance")
  ("x" delete-window "delete")
  ("X" delete-other-windows "delete")
  ("b" helm-mini "switch buffer")
  ("f" helm-find-files "find files")
  ("t" multi-term "term")
  ("n" nil))

;; hydra for multi-term
(defhydra hydra-multi-term (global-map "C-x t" :color amaranth)
  "Multiple terminal"
  ("t" multi-term "create")
  ("j" multi-term-next "next")
  ("k" multi-term-prev "prev")
  ("n" nil))

;; evil
(evil-mode t)
(define-key evil-normal-state-map (kbd "C-]") (kbd "\\ M-."))

;; evil-leader
(global-evil-leader-mode t)
(evil-leader/set-key "j" 'evil-ace-jump-word-mode)
(evil-leader/set-key "k" 'evil-ace-jump-char-mode)
(evil-leader/set-key "b" 'helm-mini)
(evil-leader/set-key "f" 'helm-find-files)

;; evil-nerd-commenter
(evilnc-default-hotkeys)
(define-key evil-visual-state-map (kbd ",ci") 'evilnc-comment-or-uncomment-lines)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(require 'helm-config)
(define-key helm-command-map (kbd "g") 'helm-do-grep)
(define-key helm-command-map (kbd "SPC") 'helm-all-mark-rings)

;; helm-projectile
(helm-projectile-on)

;; helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; virtualenvwrapper
(venv-initialize-eshell)

;; after-init hooks
(add-hook 'after-init-hook 'global-company-mode)

;; prog-mode
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'ggtags-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; python-mode
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; haskell-mode
(add-hook 'haskell-mode-hook 'subword-mode) 
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode t)
                                  (make-local-variable 'eldoc-documentation-function)
                                  (setq-local eldoc-documentation-function nil)))
