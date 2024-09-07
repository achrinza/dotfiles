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
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(elpaca-wait)
; END Elpaca additional config

; Install dependencies
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
  company
  :ensure t
  :hook ((js-json-mode . company-mode)
	 (typescript-mode . company-mode)
	 (yaml-mode . company-mode)
	 (clojure-mode . company-mode)))

; `:demand t` required to initialise built-in Eglot
(use-package
  eglot
  :demand t
  :config (add-to-list 'eglot-server-programs
		       '(clojure-mode "clojure-lsp")))

(use-package
  clojure-mode
  :ensure t
  :hook (clojure-mode . eglot-ensure))

;(use-package
;  cider
;  :ensure t
;  :hook (clojure-mode . cider-jack-in))

(use-package
  js-json-mode
  :hook (js-json-mode . eglot-ensure))

(use-package
  typescript-mode
  :ensure t
  :hook (typescript-mode . eglot-ensure))

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

(load-theme 'wombat t)

; (setq frame-background-mode "dark")

; (add-hook 'typescript-mode-hook #'eglot)
; Eglot config
; (add-to-list 'auto-mode-alist '("\\.ts[mx]?\\'" . typescript-mode))

(setq-default indent-tabs-mode nil)
