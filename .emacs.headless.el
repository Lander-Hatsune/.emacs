;;; ----- basics -----

;; C-h as backspace
(global-set-key (kbd "C-h") 'delete-backward-char)

;; basic face
(menu-bar-mode 0)
(global-linum-mode 1)

;; English Font
(set-face-attribute
'default nil :font "Ubuntu Mono 16")

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

(use-package org
  :ensure t
  :config
  (setq org-startup-indented t))

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

(provide '.emacs)
;;; .emacs ends here
