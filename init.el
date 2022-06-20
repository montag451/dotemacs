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

(defun my/call-key-binding (key)
  (condition-case nil
      (call-interactively (key-binding (kbd key)))
    ('quit nil)))

(defun my/comint-load-history (histfile)
  (lambda ()
    (let* ((proc (get-buffer-process (current-buffer)))
           (remote (file-remote-p default-directory))
           (histfile (expand-file-name (concat remote histfile))))
      (when proc
        (setq comint-input-ring-file-name histfile)
        (comint-read-input-ring)
        (add-function
         :before (process-sentinel proc)
         (lambda (proc _event)
           (unless (process-live-p proc)
             (comint-write-input-ring))))))))

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

(defun my/vertico-switch-to-shell-buffer ()
  (interactive)
  (let ((buffers (my/list-buffers-with-mode 'shell-mode)))
    (if buffers
        (switch-to-buffer
         (completing-read
          "Switch to shell buffer: "
          buffers))
      (message "No shell buffer found!"))))

(defun my/get-bash-path ()
  (let ((shell-file-name "/bin/sh"))
    (with-temp-buffer
      (unless (zerop (shell-command "bash -c \"type -p bash\"" t))
        (error "Cannot find bash on %s"
               (or (file-remote-p default-directory 'host)
                   (system-name))))
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun my/get-user ()
  (if (file-remote-p default-directory)
      (with-temp-buffer
        (unless (zerop (shell-command "echo $USER" t))
          (error "Cannot find remote user on %s"
                 (file-remote-p default-directory 'host)))
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position)))
    user-login-name))

(defun my/spawn-shell (dir &optional force)
  "Spawn a shell using DIR as the default directory.
If FORCE is non-nil a new shell will be spawned even if one for
the same user and the same host already exists. When called
interactively with a prefix arg, FORCE is set to a non-nil
value."
  (interactive "DDefault directory: \nP")
  (require 'shell)
  (let* ((default-directory dir)
         (remote (file-remote-p dir))
         (host (or (file-remote-p dir 'host) (system-name)))
         (buffer (format "*shell %s@%s*" (my/get-user) host))
         (histfile-env-name "HISTFILE")
         (prev-histfile (getenv histfile-env-name)))
    (unwind-protect
        (let ((histfile (expand-file-name (concat remote "~/.bash_history")))
              (tramp-histfile-override nil))
          (when remote
            (setenv histfile-env-name histfile))
          (shell (if force
                     (generate-new-buffer-name buffer)
                   buffer)))
      (setenv histfile-env-name prev-histfile))))

(defun my/spawn-shell-same-window (dir &optional force)
  (interactive "DDefault directory: \nP")
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (my/spawn-shell dir force)))

(defun my/kill-shell-buffers ()
  (interactive)
  (dolist (buffer (my/list-buffers-with-mode 'shell-mode))
    (with-current-buffer buffer
      (comint-send-eof))))

;; store customized variables into custom.el
(my/setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(global-set-key (kbd "C-c s s") #'my/vertico-switch-to-shell-buffer)
(global-set-key (kbd "C-c s p") #'my/spawn-shell)
(global-set-key (kbd "C-c s x") #'my/spawn-shell-same-window)

(package-initialize)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; always load the latest version of file
(my/setq load-prefer-newer t)

;; no startup screen please
(my/setq inhibit-startup-screen t)

;; no backup files
(my/setq make-backup-files nil)

;; tabulations are evil
(my/setq indent-tabs-mode nil)

(my/setq tab-always-indent 'complete)

(my/setq frame-title-format
         '((:eval (if (buffer-file-name)
                      (abbreviate-file-name (buffer-file-name))
                    "%b"))))

;; blinking cursor is annoying
(blink-cursor-mode -1)

;; disable the bell, it drives me crazy
(my/setq ring-bell-function #'ignore)

;; save some screen estate
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; set font
(when window-system
  (let* ((pattern "Ubuntu Mono-14:style=Regular:weight=normal:slant=normal")
         (font (car (x-list-fonts pattern))))
    (when font
      (add-to-list 'default-frame-alist (cons 'font font)))))

;; use UTF-8 with UNIX EOL if possible
(prefer-coding-system 'utf-8-unix)

;; make Emacs case sensitive when searching/replacing
(my/setq case-fold-search nil)
(my/setq case-replace nil)

;; search for the symbol at point a la VIM
(global-set-key (kbd "C-*") #'isearch-forward-symbol-at-point)

;; remove duplicates entry from history
(my/setq history-delete-duplicates t)

(connection-local-set-profile-variables
 'remote-bash
 '((explicit-shell-file-name . "/bin/bash")
   (shell-command-switch . "-c")))
(connection-local-set-profile-variables
 'remote-nixos-bash
 '((explicit-shell-file-name . "/usr/bin/env")
   (explicit-env-args . ("bash" "--noediting" "-i"))))
(connection-local-set-profiles
 '(:application tramp) 'remote-bash)
(connection-local-set-profiles
 '(:application tramp :machine "vps300614.ovh.net") 'remote-nixos-bash)

;; install use-package if necessary
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(my/setq use-package-always-pin "melpa-stable")

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
  :config
  (recentf-mode)
  (my/setq recentf-auto-cleanup 'never))

(use-package cc-vars
  :defer t
  :config
  (my/setq c-basic-offset 4))

(use-package ediff-wind
  :defer t
  :config
  (my/setq ediff-window-setup-function
           (lambda (&rest args)
             (let (window-combination-resize)
               (apply #'ediff-setup-windows-plain args)))))

(use-package ediff-init
  :defer t
  :config
  (add-hook 'ediff-prepare-buffer-hook
            (lambda ()
              (when (derived-mode-p 'outline-mode)
                (outline-show-all))))
  (add-hook 'ediff-quit-hook #'winner-undo t))

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
  (my/setq dired-dwim-target t)
  (my/setq dired-keep-marker-copy nil))

(use-package dired-x :after dired)

(use-package comint
  :defer t
  :config
  (my/setq comint-scroll-to-bottom-on-input t)
  (add-hook 'comint-exec-hook
            (lambda ()
              (let ((proc (get-buffer-process (current-buffer))))
                (when proc
                  (add-function
                   :after (process-sentinel proc)
                   (lambda (proc _event)
                     (unless (process-live-p proc)
                       (let* ((buf (process-buffer proc))
                              (win (get-buffer-window buf)))
                         (if win
                             (quit-window t win)
                           (kill-buffer buf)))))))))))

(use-package shell
  :defer t
  :config
  (my/setq shell-font-lock-keywords nil))

(use-package whitespace
  :defer t
  :diminish
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'text-mode-hook #'whitespace-mode)
  :config
  (my/setq whitespace-line-column 80)
  (my/setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package compile
  :defer t
  :config
  (my/setq compilation-scroll-output 'first-error))

(use-package ange-ftp
  :defer t
  :config
  (my/setq ange-ftp-try-passive-mode t))

(use-package eldoc
  :defer t
  :config
  (my/setq eldoc-minor-mode-string nil))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package isearch
  :defer t
  :config
  (my/setq isearch-lazy-count t))

(use-package minibuffer
  :defer t
  :config
  (my/setq
   completion-category-overrides
   '((file (styles basic partial-completion))
     (eval (styles basic partial-completion)))))

;;; external packages

(use-package diminish
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :pin melpa
  :config
  (load-theme 'zenburn t))

(use-package avy
  :ensure t
  :config
  (my/setq avy-keys (number-sequence ?a ?z))
  (my/setq avy-case-fold-search nil))

(use-package vertico
  :ensure t
  :pin gnu
  :config
  (vertico-mode)
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
  (define-key vertico-map (kbd "M-V") #'vertico-multiform-vertical)
  (define-key vertico-map (kbd "M-G") #'vertico-multiform-grid)
  (define-key vertico-map (kbd "M-F") #'vertico-multiform-flat)
  (vertico-multiform-mode)
  (my/setq vertico-multiform-categories
           '((command (vertico-sort-override-function . vertico-sort-history-length-alpha)))))

(use-package orderless
  :ensure t
  :config
  (my/setq completion-styles '(orderless basic partial-completion)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  (add-to-list 'marginalia-command-categories '(eval-expression . eval)))

(use-package consult
  :ensure t
  :config
  (my/setq consult-narrow-key (kbd "<"))
  (with-eval-after-load 'comint
    (define-key comint-mode-map (kbd "M-r") #'consult-history))
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-commands '(consult-imenu buffer))
    (add-to-list 'vertico-multiform-commands '(consult-grep buffer)))
  (define-key minibuffer-local-map (kbd "M-r") #'consult-history)
  (global-set-key (kbd "C-c h i") #'consult-imenu)
  (global-set-key (kbd "C-c h m") #'consult-man)
  (global-set-key (kbd "C-x b") #'consult-buffer))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim))
  :config
  (with-eval-after-load 'consult
    ;; TODO: improvement
    (eval
     `(defun my/grep (dir)
        ,(documentation 'consult-grep)
        (interactive "sDirectory: ")
        (consult-grep dir)))
    (define-key embark-file-map (kbd "g") #'my/grep)))

(use-package corfu
  :ensure t
  :pin gnu
  :config
  (my/setq tab-always-indent 'complete)
  (my/setq c-tab-always-indent 'complete)
  (global-corfu-mode))

(use-package cape
  :ensure t
  :pin gnu
  :init
  (add-hook 'completion-at-point-functions #'cape-line)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(use-package paredit
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook
            (lambda ()
              (let* ((key (kbd "M-r"))
                     (cmd (key-binding key)))
                (paredit-mode)
                (when-let ((orig-cmd cmd)
                           (paredit-cmd (lookup-key paredit-mode-map key))
                           (map (make-sparse-keymap)))
                  (set-keymap-parent map paredit-mode-map)
                  (define-key map key
                    (lambda ()
                      (interactive)
                      (if (string= (minibuffer-contents) "")
                          (call-interactively orig-cmd)
                        (call-interactively paredit-cmd))))
                  (push `(paredit-mode . ,map)
                        minor-mode-overriding-map-alist)))))
  :config
  (my/setq paredit-lighter ""))

(use-package projectile
  :ensure t
  :init
  (my/setq projectile-mode-line-prefix "")
  :config
  (my/setq projectile-use-git-grep t)
  (my/setq projectile-dynamic-mode-line nil)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package magit
  :ensure t
  :pin melpa
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (my/setq magit-commit-show-diff nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream))

(use-package hydra
  :ensure t
  :defer t
  :config
  (my/setq hydra-is-helpful nil))

;; hydra for window manipulation
(defhydra hydra-window (global-map "C-x w" :color red)
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
  ("b" (my/call-key-binding "C-x b") "switch buffer")
  ("f" (my/call-key-binding "C-x C-f") "find files")
  ("t" multi-term "term")
  ("n" nil))

(use-package python
  :ensure t
  :defer t
  :config
  (add-hook 'inferior-python-mode-hook
            (my/comint-load-history "~/.python_history")))

(use-package go-mode
  :ensure t
  :pin melpa
  :defer t
  :config
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (my/setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (my/setq tab-width 4)
              (setq-local whitespace-style (remq 'tabs whitespace-style))
              (whitespace-mode 0)
              (whitespace-mode 1)))
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2\\'" . jinja2-mode)))

(use-package org
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c l") #'org-store-link)
  :config
  (my/setq org-directory (expand-file-name "~/Annex/Org"))
  (my/setq org-agenda-files `(,org-directory))
  (my/setq org-default-notes-file (concat org-directory "/notes.org"))
  (my/setq org-use-speed-commands t)
  (my/setq org-src-fontify-natively t)
  (my/setq org-special-ctrl-a/e t)
  (my/setq org-catch-invisible-edits t)
  (my/setq org-M-RET-may-split-line nil)
  (my/setq org-list-demote-modify-bullet '(("+" . "*") ("-" . "+") ("*" . "-")))
  (my/setq org-list-indent-offset 2)
  (my/setq org-enforce-todo-checkbox-dependencies t)
  (my/setq org-enforce-todo-dependencies t)
  (my/setq org-babel-load-languages '((plantuml . t)
                                      (emacs-lisp . t)
                                      (gnuplot . t))))

(use-package which-key
  :ensure t
  :config
  (my/setq which-key-lighter "")
  (which-key-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :diminish
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode))

(use-package bash-completion
  :ensure t
  :after shell
  :init
  (add-to-list 'shell-dynamic-complete-functions
               #'bash-completion-dynamic-complete)
  :config
  (my/setq bash-completion-prog "/usr/bin/env")
  (my/setq bash-completion-remote-prog "/usr/bin/env")
  (my/setq bash-completion-args '("bash" "--noediting" "-l" "-i")))

(use-package eyebrowse
  :ensure t
  :pin melpa
  :config
  (my/setq eyebrowse-new-workspace t)
  (eyebrowse-mode))

(use-package whole-line-or-region
  :ensure t
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode)
  (add-hook 'calc-mode-hook
            (lambda ()
              (whole-line-or-region-local-mode -1)))
  (add-hook 'calc-trail-mode-hook
            (lambda ()
              (whole-line-or-region-local-mode -1))))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package jq-mode
  :ensure t
  :mode (("\\.jq\\'" . jq-mode)))

(use-package with-editor
  :ensure t
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

(use-package powershell
  :ensure t
  :pin melpa)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :pin melpa
  :defer t)

;; finally load customized variables
(load custom-file t)
