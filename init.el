;; helper functions and macros
(defmacro my/setq (&rest args)
  "A macro that behaves like `setq' except when a symbol has been defined
using `defcustom'. In this case, it uses `customize-set-variable' to set the
value of the symbol."
  (let ((def '()))
    (while args
      (let ((sym (pop args)))
        (unless (symbolp sym)
          (error "Symbol expected, found: %S" sym))
        (unless args
          (error "Missing value for: %S" sym))
        (let ((val (pop args)))
          (push `(if (custom-variable-p ',sym)
                     (customize-set-variable ',sym ,val)
                   (setq ,sym ,val))
                def))))
    `(progn ,@(reverse def))))

;; initialize package.el machinery
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; install and configure use-package
(unless (package-installed-p 'use-package)
    (progn
      (package-refresh-contents)
      (package-install 'use-package t)))
(my/setq use-package-verbose t)

;; global variables
(my/setq my/plantuml-jar-path (expand-file-name (concat user-emacs-directory "plantuml.8057.jar")))

;; no startup screen please
(my/setq inhibit-startup-screen t)

;; no backup files
(my/setq make-backup-files nil)

;; tabulations are evil
(my/setq indent-tabs-mode nil)

(my/setq frame-title-format
         '((:eval (if (buffer-file-name)
                      (abbreviate-file-name (buffer-file-name))
                    "%b"))))

;; blinking cursor is annoying
(blink-cursor-mode -1)

;; disable the bell, it drives me crazy
(my/setq ring-bell-function 'ignore)

;; save some screen estate
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; set and load custom-file
(my/setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(use-package help
  :defer t
  :config
  (my/setq help-window-select t))

(use-package mouse
  :defer t
  :config
  (my/setq mouse-yank-at-point t))

(use-package select
  :defer t
  :config
  (my/setq select-enable-primary t))

(use-package recentf
  :defer t
  :config
  (my/setq recentf-auto-cleanup 'never))

(use-package cc-vars
  :defer t
  :config
  (my/setq c-basic-offset 4))

(use-package ediff-wind
  :defer t
  :config
  (my/setq ediff-window-setup-function 'ediff-setup-windows-plain ))

(use-package ediff-init
  :defer t
  :config
  (add-hook 'ediff-quit-hook 'winner-undo t))

(use-package indents
  :config
  (my/setq tab-always-indent 'complete))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package simple
  :config
  (column-number-mode)
  (size-indication-mode))

(use-package paren
  :config
  (show-paren-mode))

(use-package desktop
  :config
  (desktop-save-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package winner
  :config
  (winner-mode))

(use-package server
  :config
  (server-mode))

(use-package dired
  :defer t
  :config
  (my/setq dired-recursive-copies 'always)
  (my/setq dired-recursive-deletes 'always))

(use-package tangotango-theme
  :ensure t
  :config
  (my/setq custom-enabled-themes '(tangotango)))

(use-package evil
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-]") (kbd "\\ M-."))
  (my/setq evil-search-wrap nil)
  (my/setq evil-symbol-word-search t)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'inferior-emacs-lisp-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'gud-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-mode))

(use-package helm
  :ensure t
  :demand
  :init
  (require 'helm-config)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-command-map
         ("g" . helm-do-grep-ag))
  :config
  (my/setq helm-command-prefix-key "C-c h")
  (my/setq helm-buffers-fuzzy-matching t)
  (my/setq helm-recentf-fuzzy-match t)
  (my/setq helm-M-x-fuzzy-match t)
  (my/setq helm-ff-file-name-history-use-recentf t)
  (my/setq helm-ff-search-library-in-sexp t)
  (my/setq helm-mode-handle-completion-in-region nil)
  (my/setq helm-move-to-line-cycle-in-source t)
  (my/setq helm-net-prefer-curl t)
  (my/setq helm-split-window-in-side-p t)
  (helm-mode))

(use-package helm-swoop
  :ensure t
  :bind (:map isearch-mode-map
              ("M-i" . helm-swoop-from-isearch))
  :config
  (my/setq helm-swoop-speed-or-color t)
  (my/setq helm-swoop-use-line-number-face t))

(use-package paredit
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode))

(use-package evil-paredit
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (my/setq company-show-numbers t))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))

(use-package company-restclient
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-anaconda
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package projectile
  :ensure t
  :config
  (my/setq projectile-completion-system 'helm)
  (my/setq projectile-mode-line nil)
  (my/setq projectile-use-git-grep t)
  (projectile-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (my/setq magit-commit-show-diff nil))

(use-package multi-term
  :ensure t
  :defer t
  :config
  (my/setq multi-term-dedicated-select-after-open-p t)
  (my/setq term-bind-key-alist '(("C-c C-c" . term-interrupt-subjob)))
  (my/setq term-unbind-key-list '("C-x" "C-c" "C-h" "C-y" "M-x" "M-:")))

(use-package hydra
  :ensure t
  :defer t
  :config
  (my/setq hydra-is-helpful nil))

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
  ("<" shrink-window-horizontally "shrink horizontally")
  ("=" balance-windows "balance")
  ("x" delete-window "delete")
  ("X" delete-other-windows "delete other windows")
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

(use-package ggtags
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'ggtags-mode))

(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (my/setq haskell-hoogle-command nil)
  (my/setq haskell-hoogle-url "http://hoogle.haskell.org/?hoogle=%s")
  (my/setq haskell-process-type 'stack-ghci))

(use-package nix-mode
  :load-path  "external/nix-mode"
  :mode (("\\.nix\\'" . nix-mode)
         ("\\.nix.in\\'" . nix-mode)))

(use-package plantuml-mode
  :ensure t
  :defer t
  :config
  (my/setq plantuml-jar-path my/plantuml-jar-path))

(use-package htmlize :ensure t :defer t)

(use-package markdown-mode :ensure t :defer t)

(use-package restclient :ensure t :defer t)

(use-package restclient-helm :ensure t :defer t)

(use-package cmake-mode :ensure t :defer t)

(use-package yaml-mode :ensure t :defer t)

(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2\\'" . jinja2-mode)))

(use-package org
  :defer t
  :config
  (my/setq org-use-speed-commands t)
  (my/setq org-babel-load-languages '((plantuml . t)
                                      (emacs-lisp . t))))

(use-package ob-plantuml
  :defer t
  :config
  (my/setq org-plantuml-jar-path my/plantuml-jar-path))

(use-package ox-gfm :ensure t :defer t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package whitespace
  :defer t
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'text-mode-hook 'whitespace-mode)
  :config
  (my/setq whitespace-line-column 80)
  (my/setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package term
  :defer t
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key
                evil-emacs-state-local-map
                (kbd "C-z")
                'term-send-raw))))

(use-package popwin
  :ensure t
  :demand
  :config
  (global-set-key (kbd "C-c s") popwin:keymap)
  (add-to-list 'popwin:special-display-config '("^\\*Man .*\\*$" :regexp t :width 80 :position right))
  (defun my/popwin-help-mode-off ()
    "Turn `popwin-mode' off for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (delq 'help-mode popwin:special-display-config)))
  (defun my/popwin-help-mode-on ()
    "Turn `popwin-mode' on for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (add-to-list 'popwin:special-display-config 'help-mode nil #'eq)))
  (add-hook 'helm-minibuffer-set-up-hook #'my/popwin-help-mode-off)
  (add-hook 'helm-cleanup-hook #'my/popwin-help-mode-on)
  (popwin-mode))
