(column-number-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(require 'package)
(setq-default frames-only-mode t
              indent-tabs-mode nil
              inhibit-splash-screen t
              package-archives nil
              package-enable-at-startup nil)
(package-initialize)

(load-theme 'wombat)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)

(set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 110)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)

(use-package clojure-mode)

(use-package company
  :custom
  (company-idle-begin 0.5)
  :bind
  (:map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-<" . company-select-first)
    ("M->" . company-select-last)))

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))

(use-package direnv
  :config
  (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package emojify)

(use-package erlang)

(use-package fill-column-indicator
  :config
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode))

(use-package flycheck
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode))

(use-package helm-lsp)

(use-package hl-todo
  :demand
  :config (global-hl-todo-mode t))

(use-package htmlize)

(use-package idris-mode)

(use-package lfe-mode)

(use-package ligature
  :config
  (ligature-set-ligatures
    'prog-mode
    '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
       "=<<" "=<" "=<=" "<==" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
       "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__"
       "<~~" "</" "</>" "/>" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!="
       "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
       "(*" "*)" "/*" "*/" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--"
       "<!---"))
  (global-ligature-mode t))


(eval-and-compile
    ;;; https://www.emacswiki.org/emacs/ThreadMacroFromClojure
  (defmacro -> (&rest body)
    (let ((result (pop body)))
      (dolist (form body result)
        (setq result (append (list (car form) result) (cdr form))))))

  (defun yurrriq/lilypond-load-path ()
    (-> (executable-find "lilypond")
      (file-name-directory)
      (directory-file-name)
      (file-name-directory)
      (file-name-concat "share" "emacs" "site-lisp")
      (file-name-as-directory))))

(use-package lilypond-mode
  :load-path (lambda () (list (yurrriq/lilypond-load-path)))
  :mode ("\\.ly\\'")
  :demand)

(use-package lsp-mode
  :hook (((clojure-mode
           clojurescript-mode
           clojurec-mode
           erlang-mode
           haskell-mode)
          . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-modeline-code-actions-enable nil)
  (lsp-keymap-prefix "C-l")
  :config
  (advice-add 'lsp :before #'direnv-update-environment))

(use-package lsp-ui
  :hook ((haskell-mode . lsp-ui-mode))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable t))

(use-package lsp-origami
  :hook ((origami-mode . lsp-origami-mode)
          ((clojure-mode
            erlang-mode)
           . origami-mode)))

(use-package lsp-haskell)

(use-package magit
  :demand
  :bind
  (("C-c g" . magit-file-dispatch)
   ("C-x g" . magit-status)
   ("C-x C-g" . magit-status)))

(use-package multiple-cursors
  :demand
  :config (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package nix-mode)

(use-package nyan-mode
  :demand
  :config (nyan-mode 1))

(eval-and-compile
  (defun yurrriq/blorg-load-path ()
    (file-name-as-directory (file-name-concat default-directory "org"))))

(use-package ob-lfe
  :load-path (lambda () (list (yurrriq/blorg-load-path))))

(use-package org
  :config
  (load-file (file-name-concat (yurrriq/blorg-load-path) "blorg.el"))
  (setq time-stamp-pattern "10/^updated: %d %:B, %Y$")
  (add-hook 'before-save-hook 'time-stamp)
  :custom
  (org-babel-load-languages
    '((emacs-lisp . t)
      (shell . t))
    "Add shell to languages which can be evaluated in Org buffers")
  (org-export-allow-bind-keywords t))

(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-buffer))
  :config
  (setq ormolu-extra-args '("--ghc-opt" "-XTemplateHaskell")))

(use-package ox-tufte)

(use-package paredit
  :hook ((cider-repl-mode
          clojure-mode
          emacs-lisp-mode)
         . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smex
  :demand
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)))

(use-package which-key
  :hook (((clojure-mode
           erlang-mode)
          . which-key-mode)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t))

(use-package yaml-mode)
