;;; ----- code -----

(defun open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
When called in Emacs Lisp, if @fname is given, open that.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04 2021-02-16"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (mapc
       (lambda ($fpath) (let ((process-connection-type nil))
                          (start-process "" nil "xdg-open" $fpath))) $file-list))))
(global-set-key (kbd "C-c C-o") 'open-in-external-app)


;;; ----- basics -----


;; basic face
(custom-set-variables
 '(default-frame-alist '((undecorated . t))))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-linum-mode 1)
(add-hook 'pdf-view-mode-hook
          (lambda () (add-hook 'after-change-major-mode-hook
                               (lambda () (linum-mode 0))
                               :append :local)))

;; English Font
(set-face-attribute
'default nil :font "Ubuntu Mono 16")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
(set-fontset-font (frame-parameter nil 'font)
charset
(font-spec :family "Microsoft YaHei")))

;; space indent
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; shut welcome page
(setq inhibit-splash-screen 1)


;; ----- packages -----


;; source
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (version< emacs-version "27.0") (package-initialize))

(require 'use-package) ;; use-package

(use-package nord-theme
  :ensure t
  :config (load-theme 'nord t))
;; leuven/dark, peacock, dracula, monokai, zenburn,
;; sanityinc-tomorrow-eighties, nord

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(require 'org)
(setq org-startup-indented t)
(global-set-key "\C-ca" 'org-agenda)
(require 'org-tempo)
(setq org-log-done t) ;; close timestamp
(setq org-src-fontify-natively t) ;; code block highlight
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i@/!)" "|"
                  "DONE(d!)" "PASS(p@/!)" "CANCELLED(c@/!)")))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\.md\'" . gfm-mode)
         ("\.md\'" . markdown-mode)
         ("\.markdown\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

(use-package ein
  :ensure t)

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-ignore-extensions t)
  (setq ido-auto-merge-delay-time 30)
  :config
  (ido-mode t))

(use-package editorconfig
  :ensure t)


(use-package cmake-mode
  :ensure t
  :config
  (setq load-path (cons
                   (expand-file-name "/dir/with/cmake-mode")
                   load-path)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-scope 'frame))

(use-package auto-package-update
  :ensure t)

(use-package trashed
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'trashed)
  (setq delete-by-moving-to-trash t))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-clients-deno-enable-lint nil)
  ; :hook
  ; (java-mode . lsp)
  :commands lsp)
;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company
  :ensure t
  :config (global-company-mode))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq pdf-view-image-relief 0))

(use-package typescript-mode
  :ensure t)

(provide '.emacs)
;;; .emacs ends here
