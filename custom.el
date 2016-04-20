(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-clang company-semantic company-xcode company-cmake company-anaconda company-capf
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-files company-dabbrev)))
 '(company-quickhelp-mode t)
 '(custom-enabled-themes (quote (tangotango)))
 '(custom-safe-themes
   (quote
    ("5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" default)))
 '(desktop-save-mode t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-commit-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-cherry-mode magit-diff-mode magit-log-mode magit-log-select-mode magit-popup-mode magit-popup-sequence-mode magit-process-mode magit-reflog-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-stashes-mode magit-status-mode magit-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode term-mode eshell-mode dired-mode cider-browse-ns-mode cider-connections-buffer-mode cider-docview-mode cider-inspector-mode cider-popup-buffer-mode cider-repl-mode cider-stacktrace-mode cider-test-report-mode org-mode gud-mode inferior-emacs-lisp-mode inferior-python-mode)))
 '(evil-insert-state-modes
   (quote
    (erc-mode geiser-repl-mode inferior-apl-mode inferior-caml-mode inferior-j-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode slime-repl-mode wdired-mode)))
 '(evil-leader/leader "<SPC>")
 '(evil-search-wrap nil)
 '(evil-symbol-word-search t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-command-prefix-key "C-c h")
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-mode t)
 '(helm-mode-handle-completion-in-region nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-net-prefer-curl t)
 '(helm-split-window-in-side-p t)
 '(help-window-select t)
 '(hydra-is-helpful nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(multi-term-dedicated-select-after-open-p t)
 '(multi-term-program "/bin/bash")
 '(org-use-speed-commands t)
 '(projectile-completion-system (quote helm))
 '(projectile-global-mode t)
 '(projectile-mode-line nil)
 '(projectile-switch-project-action (quote helm-projectile-find-file))
 '(projectile-use-git-grep t)
 '(racer-rust-src-path "~/.emacs.d/rust/src")
 '(recentf-auto-cleanup (quote never))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(server-mode t)
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob))))
 '(term-unbind-key-list (quote ("C-x" "C-c" "C-h" "C-y" "M-x" "M-:")))
 '(text-mode-hook (quote (text-mode-hook-identify auto-fill-mode)))
 '(tool-bar-mode nil)
 '(venv-location "~/Prog/venv/")
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3434" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(term-color-blue ((t (:background "cornflower blue" :foreground "cornflower blue")))))
