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

(defun my/override-minor-mode-key-binding (mode key &optional cmd)
  (when-let ((mode-map-name (intern-soft (format "%s-map" mode)))
             (mode-map (ignore-errors (symbol-value mode-map-name)))
             (ignore-errors (symbol-value mode)))
    (let* ((key (kbd key))
           (shadowed-cmd (cl-letf (((alist-get mode minor-mode-overriding-map-alist) nil))
                           (key-binding key)))
           (mode-cmd (lookup-key mode-map key))
           (new-cmd (or (and cmd (lambda ()
                                   (interactive)
                                   (funcall cmd shadowed-cmd mode-cmd)))
                        shadowed-cmd
                        mode-cmd))
           (map (alist-get mode minor-mode-overriding-map-alist)))

      (when (and new-cmd (not (eq new-cmd mode-cmd)))
        (unless map
          (setq map (make-sparse-keymap))
          (set-keymap-parent map mode-map)
          (push `(,mode . ,map) minor-mode-overriding-map-alist))
        (define-key map key new-cmd)))))

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

(defun my/list-buffers-with-mode (mode &optional transform)
  "List all buffers with `major-mode' MODE.
MODE is a symbol."
  (save-current-buffer
    (let (bufs)
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (and (equal major-mode mode)
             (push (if transform (funcall transform buf) buf) bufs)))
      (nreverse bufs))))

(defun my/vertico-switch-to-shell-buffer ()
  (interactive)
  (let ((buffers (my/list-buffers-with-mode 'shell-mode #'buffer-name)))
    (if buffers
        (switch-to-buffer
         (completing-read
          "Switch to shell buffer: "
          buffers))
      (message "No shell buffer found!"))))

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
  (let* ((default-directory dir)
         (host (or (file-remote-p dir 'host) (system-name)))
         (user (or (file-remote-p dir 'user)
                   (pcase (file-remote-p dir 'method)
                     ("lxc" "root")
                     ('nil (user-login-name)))
                   (my/get-user)
                   "unknown"))
         (buffer (format "*shell %s@%s*" user host))
         (remote (file-remote-p dir))
         (histfile (file-remote-p
                    (expand-file-name (concat remote "~/.bash_history"))
                    'localname)))
    (with-environment-variables (("HISTFILE" histfile))
      (shell (if force (generate-new-buffer-name buffer) buffer)))))

(defun my/spawn-shell-same-window (dir &optional force)
  (interactive "DDefault directory: \nP")
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (my/spawn-shell dir force)))

(defun my/kill-shell-buffers ()
  (interactive)
  (dolist (buffer (my/list-buffers-with-mode 'shell-mode #'buffer-name))
    (with-current-buffer buffer
      (comint-send-eof))))

(defun my/url-open-stream-fix-tls-over-socks (oldfun name buffer host service &optional gw)
  (if (and (eq gw 'tls)
           (eq url-gateway-method 'socks))
      (let ((conn (funcall oldfun name buffer host service url-gateway-method))
            (cert (network-stream-certificate host service nil) ))
        (gnutls-negotiate :process conn
                          :keylist (and cert (list cert))
                          :hostname (puny-encode-domain host))
        (nsm-verify-connection conn host service))
    (funcall oldfun name buffer host service gw)))
(advice-add 'url-open-stream :around #'my/url-open-stream-fix-tls-over-socks)

(defun my/paredit-interactive-mode ()
  (paredit-mode)
  (my/override-minor-mode-key-binding
   'paredit-mode "M-r"
   (lambda (shadowed-cmd mode-cmd)
     (let ((point (point))
           (comint-mode (derived-mode-p 'comint-mode)))
       (if (or (= point (or (and comint-mode (save-excursion (comint-bol)))
                            (minibuffer-prompt-end)))
               (= point (point-max)))
           (let ((content (or (and comint-mode
                                   (buffer-substring (comint-bol) (point-max)))
                              (minibuffer-contents))))
             (condition-case nil
                 (progn
                   (if comint-mode
                       (delete-region (comint-bol) (point-max))
                     (delete-minibuffer-contents))
                   (call-interactively shadowed-cmd))
               (t
                (insert content)
                (goto-char point))))
         (call-interactively mode-cmd)))))
  (my/override-minor-mode-key-binding 'paredit-mode "RET"))

(defun my/load-theme (theme)
  (add-hook 'after-init-hook
            (lambda ()
              (if (and desktop-save-mode (not (desktop-owner)))
                  (progn
                    (add-hook 'desktop-after-read-hook
                              (lambda ()
                                (load-theme theme t)))
                    (add-hook 'desktop-no-desktop-file-hook
                              (lambda ()
                                (load-theme theme t))))
                (load-theme theme t)))))

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

;; recompile packages if necessary
(dolist (pkg package-alist)
  (let* ((desc (cadr pkg))
         (elc (directory-files-recursively
               (package-desc-dir desc) (rx ".elc" eos))))
    (unless elc
      (package-recompile desc)
      (package--reload-previously-loaded desc))))

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

;; use UTF-8 with UNIX EOL if possible
(prefer-coding-system 'utf-8-unix)

;; make Emacs case sensitive when searching/replacing
(my/setq case-fold-search nil)
(my/setq case-replace nil)

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
  :bind ("C-*" . #'isearch-forward-symbol-at-point)
  :config
  (my/setq isearch-lazy-count t))

(use-package minibuffer
  :defer t
  :config
  (my/setq
   completion-category-overrides
   '((file (styles basic partial-completion))
     (eval (styles basic partial-completion)))))

(use-package eshell
  :defer t
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash))

(use-package comp
  :defer t
  :config
  (my/setq native-comp-async-report-warnings-errors 'silent))

;;; external packages

(use-package diminish
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :pin melpa
  :config
  (my/load-theme 'zenburn))

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
  (my/setq completion-styles '(orderless basic partial-completion))
  (setf (alist-get 'buffer completion-category-overrides) '((styles orderless))))

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
  (with-eval-after-load 'em-hist
    (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history))
  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-commands '(consult-imenu buffer))
    (add-to-list 'vertico-multiform-commands '(consult-grep buffer)))
  (add-to-list 'consult-buffer-sources
               (list
                :name "Dired Buffer"
                :category 'buffer
                :narrow ?d
                :hidden t
                :action #'dired
                :items (lambda ()
                         (my/list-buffers-with-mode
                          'dired-mode
                          (lambda (buf)
                            (with-current-buffer buf
                              default-directory))))))
  (define-key minibuffer-local-map (kbd "M-r") #'consult-history)
  (consult-customize
   consult-history :initial nil)
  (global-set-key (kbd "C-c h i") #'consult-imenu)
  (global-set-key (kbd "C-c h m") #'consult-man)
  (global-set-key (kbd "C-x b") #'consult-buffer)
  (global-set-key (kbd "C-x C-b") #'consult-buffer))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after embark)

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
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (paredit-mode)
              (my/override-minor-mode-key-binding 'paredit-mode "M-?")))
  (add-hook 'ielm-mode-hook #'my/paredit-interactive-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'my/paredit-interactive-mode)
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
              (setq tab-width 4)
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
  :pin melpa
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
  :pin melpa
  :config
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'term-exec-hook   'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

(use-package powershell
  :ensure t
  :pin melpa
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :pin melpa
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :pin melpa
  :defer t)

(use-package nix-mode
  :ensure t
  :defer t)

(use-package lxc-tramp
  :ensure t
  :defer t)

(use-package erlang
  :ensure t
  :defer t)

;; finally load customized variables
(load custom-file t)
