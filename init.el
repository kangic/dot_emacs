;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start package.el with emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; maximize
;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/Dropbox/notes/tasks/todo.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-compelete
(require 'auto-complete-config)
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-compelete for c/c++
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
)
; call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iedit-mode
(require 'iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on Semantic
(semantic-mode 1)
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on ede mode
(global-ede-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on automatic reparsing of open buffer in semantic
(global-semantic-idle-scheduler-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project
(ede-cpp-root-project "eznetpp"
                :name "eznetpp library"
                :file "~/works/github/eznetpp/CMakeLists.txt"
                :include-path '("/include"
                                "/test"
                               )
                :system-include-path '("/usr/include"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-theme
(load-theme 'wombat t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tool-bar / scroll-bar off
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-number-mode
(global-linum-mode t)

(show-paren-mode 1)
(setq show-paren-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; column-number-mode
(setq column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; column-width
(setq-default fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alias to asnwer yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't save backup files
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hangul
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<S-kana>") 'toggle-input-method)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font
(set-face-font 'default "Bitstream Vera Sans Mono-12")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                   "NanumGothic_Coding-15")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake-google-cpplint
(require 'flymake-google-cpplint)

(add-hook 'c-mode-hook 'flymake-google-cpplint-load)
(add-hook 'c++-mode-hook 'flymake-google-cpplint-load)

; configure for cpplint.py
(executable-find "cpplint.py")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" default)))
 '(ecb-options-version "2.40")
 '(flymake-google-cpplint-linelength "120")
 '(flymake-google-cpplint-verbose "3")
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-time-delay 0.9)
 '(org-agenda-files (quote ("~/datas/repository/Dropbox/notes/tasks/todo.org"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake-cursor
(require 'flymake-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change short key for ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sr-speedbar
(require 'sr-speedbar)

(setq sr-speedbar-right-side nil)
(setq sr-speedbar-auto-refresh t)

(defun my:toggle-speedbar ()
  "Toggle sr-speedbar and select"
  (interactive)
  (progn
    (sr-speedbar-toggle)
    (if (sr-speedbar-exist-p)
        (sr-speedbar-select-window))))

(global-set-key (kbd "C-x p") 'my:toggle-speedbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch to header file
(defun my:switch-to-headerfile()
  (global-set-key (kbd "C-c o") 'ff-find-other-file)
)
(add-hook 'c-mode-hook 'my:switch-to-headerfile)
(add-hook 'c++-mode-hook 'my:switch-to-headerfile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for Global(sudo apt-get install global)
(setq load-path (cons "/usr/share/emacs24/site-lisp/global/gtags.el" load-path))
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
  (lambda ()
    (gtags-mode 1)))

(defun gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
      (let ((olddir default-directory)
            (topdir (read-directory-name
                     "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (shell-command "gtags && echo 'created tagfile'")
        (cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (gtags-create-or-update)))

(defun gtags-update-single (filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

(add-hook 'after-save-hook 'gtags-update-hook)

;; short-key bindings
(add-hook 'gtags-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-.") 'gtags-find-tag)
            (local-set-key (kbd "M-,") 'gtags-find-rtag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-window by meta key
(windmove-default-keybindings 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; revive
(require 'revive)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; x-clipboard paste
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set transparency
;(set-frame-parameter nil 'alpha '(80 70))
(set-background-color "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(require 'magit)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)
(helm-autoresize-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scheme
(set-variable (quote scheme-program-name) "scheme")
