;;; init.el --- my Emacs configuration. -*- lexical-binding: t -*-

;; helper functions and macros
(defmacro my/setq (&rest args)
  "A macro that behaves like `setq' except when a symbol has been defined
using `defcustom'. In this case, it uses `customize-set-variable' to set the
value of the symbol."
  (let (def)
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
    `(progn ,@(nreverse def))))

;; global variables
(defvar my/plantuml-jar-path
  (expand-file-name (concat user-emacs-directory "plantuml.8057.jar"))
  "Path of the plantuml jar file")

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

;; set font
(when window-system
  (let ((font (car (x-list-fonts "Ubuntu Mono-14:style=Regular:weight=normal:slant=normal"))))
    (when font
      (add-to-list 'default-frame-alist (cons 'font font)))))

;; resize all the windows of a combination when one window of the
;; combination is deleted, it helps keeping the layout of frames
(my/setq window-combination-resize t)

;; set and load custom-file
(my/setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;;; builtin packages

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
  (my/setq dired-recursive-deletes 'always)
  (my/setq dired-dwim-target t))

(use-package dired-x :after dired)

(use-package comint
  :defer t
  :config
  (defun my/comint-clean-up-on-exit (buffer)
    "Kill BUFFER and its window if it's displayed.
When the inferior process of BUFFER is not live anymore, its
window is deleted if it's displayed and BUFFER is killed."
    (let ((proc (get-buffer-process buffer)))
      (add-function :after (process-sentinel proc)
                    (lambda (proc _event)
                      (let* ((buf (process-buffer proc))
                             (win (get-buffer-window buf)))
                        (unless (process-live-p proc)
                          (ignore-errors
                            (delete-window win))
                          (kill-buffer buf)))))
      buffer))
  (advice-add 'make-comint-in-buffer :filter-return 'my/comint-clean-up-on-exit))

(use-package shell
  :defer t
  :config
  (my/setq shell-font-lock-keywords nil))

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

(use-package compile
  :defer t
  :config
  (my/setq compilation-scroll-output 'first-error))

(use-package ange-ftp
  :defer t
  :config
  (my/setq ange-ftp-try-passive-mode t))

;;; external packages

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package avy
  :ensure t
  :config
  (my/setq avy-keys (number-sequence ?a ?z))
  (my/setq avy-case-fold-search nil))

;; evil-leader must be enabled before evil, otherwise evil-leader
;; won't be enabled in initial buffer (*scratch*, *Messages*, ...)
(use-package evil-leader
  :ensure t
  :demand
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "j" 'evil-avy-goto-char
    "k" 'evil-avy-goto-word-0
    "g" 'evil-avy-goto-line)
  (global-evil-leader-mode))

(use-package evil
  :ensure t
  :config
  (defun my/fix-evil-hiding-minor-mode-map (&rest _args)
    "See `https://github.com/syl20bnr/spacemacs/issues/9391'"
    (let ((mjm-keymap (intern-soft (format "%s-map" major-mode))))
      (when mjm-keymap
        (setq evil-mode-map-alist
              (cl-loop for (c . k) in evil-mode-map-alist
                       unless (and (eq c t) (eq k (symbol-value mjm-keymap)))
                       collect (cons c k))))))
  (advice-add 'evil-normalize-keymaps :after #'my/fix-evil-hiding-minor-mode-map)
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
         ("g" . helm-do-grep-ag)
         :map isearch-mode-map
         ("M-i" . helm-occur-from-isearch))
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

(use-package helm-gtags
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (let ((map evil-normal-state-local-map))
                (define-key map (kbd "C-]") 'helm-gtags-dwim)
                (define-key map (kbd "C-t") 'helm-gtags-pop-stack))
              (helm-gtags-mode)))
  :config
  (my/setq helm-gtags-mode-name "")
  (let ((map helm-gtags-mode-map))
    (define-key map (kbd "C-c g s") 'helm-gtags-select)
    (define-key map (kbd "C-c g f") 'helm-gtags-select-path)
    (define-key map (kbd "C-c g u") 'helm-gtags-update-tags)
    (define-key map (kbd "C-c g t") 'helm-gtags-show-stack)
    (define-key map (kbd "C-c g r") 'helm-gtags-resume)
    (define-key map (kbd "C-c g j") 'helm-gtags-next-history)
    (define-key map (kbd "C-c g k") 'helm-gtags-previous-history)))

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

(use-package helm-projectile
  :ensure t
  :after helm projectile
  :config
  (helm-projectile-on))

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

(use-package popwin
  :ensure t
  :demand
  :config
  (global-set-key (kbd "C-c w") popwin:keymap)
  (my/setq popwin:reuse-window nil)
  (add-to-list 'popwin:special-display-config
               '("^\\*Man .*\\*$" :regexp t :width 80 :position right))
  (add-hook 'helm-after-initialize-hook (lambda () (popwin-mode -1)))
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
  (popwin-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

;;; handy functions

(defun my/list-buffers-with-mode (mode)
  "List all buffers with `major-mode' MODE.
MODE is a symbol."
  (save-current-buffer
    (let (bufs)
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (and (equal major-mode mode)
             (push (buffer-name buf) bufs)))
      (nreverse bufs))))

(defun my/helm-switch-to-shell-buffer ()
  (interactive)
  (switch-to-buffer
   (helm-comp-read
    "Switch to shell buffer: "
    (my/list-buffers-with-mode 'shell-mode)
    :name "Shell buffers")))

(defun my/spawn-shell (dir &optional force)
  "Spawn a shell using DIR as the default directory.
If FORCE is non-nil a new shell will be spawned even if one for
the same user and the same host already exists. When called
interactively with a prefix arg, FORCE is set to a non-nil
value."
  (interactive "DDefault directory: \nP")
  (require 'shell)
  (defvar explicit-env-args nil)
  (let* ((default-directory dir)
         (user (or (file-remote-p dir 'user) (user-login-name)))
         (host (or (file-remote-p dir 'host) system-name))
         (buffer (format "*shell %s@%s*" user host))
         (shell-file-name "/usr/bin/env")
         (explicit-env-args (cons "bash" explicit-bash-args)))
    (shell (if force
               (generate-new-buffer-name buffer)
             buffer))))

(defun my/spawn-shell-same-window (dir &optional force)
  (interactive "DDefault directory: \nP")
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (my/spawn-shell dir force)))

(global-set-key (kbd "C-c s s") 'my/helm-switch-to-shell-buffer)
(global-set-key (kbd "C-c s p") 'my/spawn-shell)
(global-set-key (kbd "C-c s x") 'my/spawn-shell-same-window)
