;; -*- coding: utf-8 -*-
;; SPDX-License-Identifier: AGPL-3.0-only
;; SPDX-FileCopyrightText: Copyright (C) 2024 Rifa Ilyasa Achrinza
;; SPDX-FileNotice: <text>
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;; </text>

; TODO:
; https://emacs.stackexchange.com/a/80106
; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
; https://github.com/typescript-language-server/typescript-language-server/blob/master/docs/configuration.md

(require 'use-package)

; Elpaca Installer
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
; END Elpaca Intalller

; Elpaca additional config
;(setq package-install-upgrade-built-in t)
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(elpaca-wait)
; END Elpaca additional config

; Install dependencies
(use-package
  multiple-cursors
  :ensure t)

(global-set-key (kbd "C-c c e") 'mc/edit-lines)

(use-package
  org-roam
  :ensure t
  :custom
  (org-roam-directory "/home/user/Documents/git-repos/achrinza/notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(use-package
  org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

; (use-package
;   company
;   :ensure t
;   :hook ((java-mode . company-mode)
;          (js-json-mode . company-mode)
; 	 (typescript-mode . company-mode)
; 	 (yaml-mode . company-mode)
; 	 (clojure-mode . company-mode)))

; `:demand t` required to initialise built-in Eglot
(setq exec-path
      (append exec-path
              '(list "/home/user/.var/app/org.gnu.emacs/data/eclipse.jdt.ls/bin")))

; Needed as the pre-installed version in Emacs 29.1 is too old.
; Note that we fallback to `package.el` using `:ensure nil` as Elpaca cannot handle
; upgrading built-in packages.
;(use-package
;  jsonrpc
;  :ensure nil)
(defun +elpaca-unload-jsonrpc (e)
  (when (featurep 'jsonrpc)
    (unload-feature 'jsonrpc))
  (elpaca--continue-build e))
(use-package
  jsonrpc
  :ensure
  `(jsonrpc
    :build ,(append
;             (butlast elpaca-build-steps)
             (cl-set-difference
	       (butlast elpaca-build-steps)
	       '(elpaca--clone
		 elpaca--configure-remotes
		 elpaca--fetch
		 elpaca--checkout-ref
		 elpaca--queue-dependencies
		 elpaca--activate-package))
             (list '+elpaca-unload-jsonrpc 'elpaca--activate-package)))
  :config
  (setq ring-bell-function #'ignore))

;(use-package
;  eglot
;  :after jsonrpc
;  :demand t
;  :config (add-to-list 'eglot-server-programs
;	               '(clojure-mode "clojure-lsp")))

(use-package
  dape
  :ensure t
  :config (dape-breakpoint-global-mode))

;(use-package
;  clojure-mode
;  :ensure t
;  :hook (clojure-mode . eglot-ensure))

(use-package
  corfu
  :ensure t
  :config
  (global-corfu-mode)
  :custom
  (corfu-auto t))

(use-package
  cider
  :ensure t
  :hook (clojure-mode . cider-mode))

(use-package
  js-json-mode
  :hook (js-json-mode . eglot-ensure))

;(use-package
;  typescript-mode
;  :ensure t
;  :custom (setq auto-mode-alist (delete '("\\.tsx?\\'" . typescript-mode) auto-mode-alist))
;  :hook (typescript-mode . eglot-ensure))

(use-package
  yaml-mode
  :ensure t
  :hook (yaml-mode . eglot-ensure))

(use-package
  markdown-mode
  :ensure t
  :hook (markdown-mode . eglot-ensure))

(use-package
  editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package
  rainbow-delimiters
  :ensure t
  :hook (prog-mode  . rainbow-delimiters-mode))

; END install dependencies

(add-hook 'text-mode-hook
          (lambda nil
            (auto-fill-mode 1)
            (set-fill-column 78)))

(load-theme 'wombat t)

; (setq frame-background-mode "dark")

; (add-hook 'typescript-mode-hook #'eglot)
; Eglot config
; (add-to-list 'auto-mode-alist '("\\.ts[mx]?\\'" . typescript-mode))


; Treesitter config;
; Credits: https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
(use-package treesit
  :mode
  (("\\.tsx\\'" . tsx-ts-mode)
   ("\\.ts?\\'" . typescript-ts-mode))
  :preface
  (defun treesit-setup-grammars ()
    "Install Tree-sitter grammars"
    (interactive)
    (dolist (grammar
             '((tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
;  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (treesit-setup-grammars)
  :hook
  ((tsx-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)))

(dolist (mapping
  '((typescript-mode . typescript-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(add-to-list 'auto-mode-alist '("\\.dita(map)?\\'" . nxml-mode))

(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(jsonrpc multiple-cursors)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
