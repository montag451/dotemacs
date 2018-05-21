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

;; disable package.el
(require 'package)
(my/setq package-enable-at-startup nil)

;; configure borg
(add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

;; when the init file is byte-compiled, use-package is autoloaded and
;; all invocations of `use-package' are expanded and no reference to
;; functions or variables defined by use-package exists anymore in the
;; expansion, expect for the variable `personal-keybindings' which is
;; used to register all bindings made with use-package. So to prevent
;; the use of an undefined variable we need to load use-package
(require 'use-package)

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
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; set font
(when window-system
  (let* ((pattern "Ubuntu Mono-14:style=Regular:weight=normal:slant=normal")
         (font (car (x-list-fonts pattern))))
    (when font
      (add-to-list 'default-frame-alist (cons 'font font)))))

;; resize all the windows of a combination when one window of the
;; combination is deleted, it helps keeping the layout of frames
(my/setq window-combination-resize t)

;; set custom-file to the equivalent of /dev/null
(let ((devnull (cond
                ((or (eq system-type 'gnu/linux)
                     (eq system-type 'darwin))
                 "/dev/null")
                ((or (eq system-type 'ms-dos)
                     (eq system-type 'windows-nt))
                 "nul")
                (t
                 (error "Unknown system: %s" system-type)))))
  (my/setq custom-file devnull))

;; make Emacs case sensitive when searching/replacing/completing
(my/setq case-fold-search nil)
(my/setq case-replace nil)

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
  (my/setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (my/setq ediff-split-window-function
           (lambda (&rest args)
             (let (window-combination-resize)
               (apply #'split-window-vertically args)))))

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
  (define-key comint-mode-map (kbd "M-r") #'helm-comint-input-ring)
  (my/setq comint-scroll-to-bottom-on-input t)
  (defun my/comint-clean-up-on-exit (buffer)
    "Kill BUFFER and its window if it's displayed.
When the inferior process of BUFFER is not live anymore, its
window is deleted if it's displayed and BUFFER is killed."
    (let ((proc (get-buffer-process buffer)))
      (add-function :after (process-sentinel proc)
                    (lambda (proc _event)
                      (unless (process-live-p proc)
                        (let* ((buf (process-buffer proc))
                               (win (get-buffer-window buf)))
                          (if win
                              (quit-window t win)
                            (kill-buffer buf))))))
      buffer))
  (advice-add 'make-comint-in-buffer :filter-return #'my/comint-clean-up-on-exit))

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

(use-package eldoc :diminish)

(use-package edebug
  :defer t
  :config
  (defvar my/state-before-edebug nil)
  (add-hook 'edebug-mode-hook
            (lambda ()
              (when evil-local-mode
                (if edebug-mode
                    (progn
                      (setq-local my/state-before-edebug evil-state)
                      (when (not (evil-emacs-state-p))
                        (evil-emacs-state)))
                  (evil-change-state my/state-before-edebug))))))

;;; external packages

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (my/setq auto-compile-display-buffer nil)
  (my/setq auto-compile-mode-line-counter t)
  (my/setq auto-compile-source-recreate-deletes-dest t)
  (my/setq auto-compile-toggle-deletes-nonlib-dest t)
  (my/setq auto-compile-update-autoloads t)
  (add-hook 'auto-compile-inhibit-compile-hook
            #'auto-compile-inhibit-compile-detached-git-head))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package avy
  :config
  (my/setq avy-keys (number-sequence ?a ?z))
  (my/setq avy-case-fold-search nil))

;; evil-leader must be enabled before evil, otherwise evil-leader
;; won't be enabled in initial buffer (*scratch*, *Messages*, ...)
(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "j" 'evil-avy-goto-char
    "k" 'evil-avy-goto-word-0
    "g" 'evil-avy-goto-line)
  (global-evil-leader-mode))

(use-package evil
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
  (evil-set-initial-state 'erlang-shell-mode 'emacs)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  (evil-set-initial-state 'image-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'emacs)
  (evil-set-initial-state 'magit-submodule-list-mode 'emacs)
  (evil-set-initial-state 'process-menu-mode 'emacs)
  (evil-mode))

(use-package undo-tree
  :defer t
  :config
  (my/setq undo-tree-mode-lighter ""))

(use-package helm
  :diminish
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
  (my/setq helm-split-window-inside-p t)
  (helm-mode))

(use-package helm-gtags
  :defer t
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (let ((map evil-normal-state-local-map))
                (define-key map (kbd "C-]") #'helm-gtags-dwim)
                (define-key map (kbd "C-t") #'helm-gtags-pop-stack))
              (helm-gtags-mode)))
  :config
  (my/setq helm-gtags-mode-name "")
  (let ((map helm-gtags-mode-map))
    (define-key map (kbd "C-c g s") #'helm-gtags-select)
    (define-key map (kbd "C-c g f") #'helm-gtags-select-path)
    (define-key map (kbd "C-c g u") #'helm-gtags-update-tags)
    (define-key map (kbd "C-c g t") #'helm-gtags-show-stack)
    (define-key map (kbd "C-c g r") #'helm-gtags-resume)
    (define-key map (kbd "C-c g j") #'helm-gtags-next-history)
    (define-key map (kbd "C-c g k") #'helm-gtags-previous-history)))

(use-package paredit
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :config
  (my/setq paredit-lighter ""))

(use-package evil-paredit
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'evil-paredit-mode))

(use-package company
  :defer t
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (my/setq company-frontends '(company-pseudo-tooltip-frontend
                               company-preview-if-just-one-frontend
                               company-echo-metadata-frontend))
  (my/setq company-lighter "")
  (my/setq company-show-numbers t))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))

(use-package company-restclient
  :after company
  :config
  (add-to-list 'company-backends #'company-restclient))

(use-package company-anaconda
  :after company
  :config
  (add-to-list 'company-backends #'company-anaconda))

(use-package projectile
  :config
  (my/setq projectile-completion-system 'helm)
  (my/setq projectile-mode-line nil)
  (my/setq projectile-use-git-grep t)
  (projectile-mode))

(use-package helm-projectile
  :after helm projectile
  :config
  (helm-projectile-on))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (my/setq magit-commit-show-diff nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-unpulled-from-upstream))

(use-package multi-term
  :defer t
  :config
  (my/setq multi-term-dedicated-select-after-open-p t)
  (my/setq term-bind-key-alist '(("C-c C-c" . term-interrupt-subjob)))
  (my/setq term-unbind-key-list '("C-x" "C-c" "C-h" "C-y" "M-x" "M-:")))

(use-package hydra
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
  ("b" helm-mini "switch buffer")
  ("f" helm-find-files "find files")
  ("t" multi-term "term")
  ("n" nil))

;; hydra for multi-term
(defhydra hydra-multi-term (global-map "C-x t" :color red)
  "Multiple terminal"
  ("t" multi-term "create")
  ("j" multi-term-next "next")
  ("k" multi-term-prev "prev")
  ("n" nil))

(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
  :config
  (my/setq anaconda-mode-lighter ""))

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'subword-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (my/setq haskell-hoogle-command nil)
  (my/setq haskell-hoogle-url "http://hoogle.haskell.org/?hoogle=%s")
  (my/setq haskell-process-type 'stack-ghci))

(use-package plantuml-mode
  :defer t
  :config
  (my/setq plantuml-jar-path my/plantuml-jar-path))

(use-package jinja2-mode
  :mode (("\\.j2\\'" . jinja2-mode)))

(use-package org
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

(use-package ob-plantuml
  :defer t
  :config
  (my/setq org-plantuml-jar-path my/plantuml-jar-path))

(use-package which-key
  :config
  (my/setq which-key-lighter "")
  (which-key-mode))

(use-package popwin
  :config
  (global-set-key (kbd "C-c w") popwin:keymap)
  (my/setq popwin:reuse-window nil)
  (add-to-list 'popwin:special-display-config
               '("^\\*Man .*\\*$" :regexp t :width 80 :position right))
  (add-hook 'helm-after-initialize-hook (lambda () (popwin-mode -1)))
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
  (popwin-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package aggressive-indent
  :diminish
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode))

(use-package erlang
  :defer t
  :config
  (my/setq inferior-erlang-machine-options '("-sname" "emacs")))

(use-package bash-completion
  :defer t
  :init
  (add-to-list 'shell-dynamic-complete-functions #'bash-completion-dynamic-complete)
  :config
  (my/setq bash-completion-prog "/usr/bin/env")
  (my/setq bash-completion-remote-prog "/usr/bin/env")
  (my/setq bash-completion-args '("bash" "--noediting" "-l" "-i")))

(use-package pdf-tools
  :config
  (pdf-tools-install))

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

(defun my/get-bash-path ()
  (let ((shell-file-name "/bin/sh"))
    (with-temp-buffer
      (unless (zerop (shell-command "bash -c \"type -p bash\"" t))
        (error "Cannot find bash on %s"
               (or (file-remote-p default-directory 'host)
                   (system-name))))
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

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
         (user (or (file-remote-p dir 'user) (user-login-name)))
         (host (or (file-remote-p dir 'host) (system-name)))
         (buffer (format "*shell %s@%s*" user host))
         (shell-file-name (or (and remote
                                   (my/get-bash-path))
                              shell-file-name))
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

(global-set-key (kbd "C-c s s") #'my/helm-switch-to-shell-buffer)
(global-set-key (kbd "C-c s p") #'my/spawn-shell)
(global-set-key (kbd "C-c s x") #'my/spawn-shell-same-window)
