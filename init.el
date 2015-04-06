; start package.el with emacs
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-compelete
(require 'auto-complete-config)
(ac-config-default)

; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

; auto-compelete for c/c++
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-linux-gnu/4.7/include")
)
; call this function from c/c++ hooks
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

; iedit-mode
(require 'iedit)
(define-key global-map (kbd "C-c ;") 'iedit-mode)

; turn on Semantic
(semantic-mode 1)
(defun my:add-semantic-to-autocomplete()
  (add-to-list 'ac-sources 'ac-source-semantic)
)
(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)
; turn on ede mode
(global-ede-mode 1)
; turn on automatic reparsing of open buffer in semantic
(global-semantic-idle-scheduler-mode 1)

; color-theme
(load-theme 'wombat t)

; tool-bar off
(tool-bar-mode -1)

; line-number-mode
(global-linum-mode t)

; column-number-mode
(setq column-number-mode t)

; column-width
(setq-default fill-column 80)

; hangul
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(global-set-key (kbd "<S-kana>") 'toggle-input-method)

; font
(set-face-font 'default "Bitstream Vera Sans Mono-12")
(set-fontset-font "fontset-default" '(#x1100 . #xffdc)
                   "NanumGothic_Coding-15")

; flymake-google-cpplint
(require 'flymake-google-cpplint)

(add-hook 'c-mode-hook 'flymake-google-cpplint-load)
(add-hook 'c++-mode-hook 'flymake-google-cpplint-load)

; configure for cpplint.py
(executable-find "cpplint.py")

(custom-set-variables
 '(flymake-google-cpplint-verbose "3")
 '(flymake-google-cpplint-linelength "120")
)

; google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

; flymake-cursor
(require 'flymake-cursor)

(custom-set-variables
 '(help-at-pt-time-delay 0.9)
 '(help-at-pt-display-when-idle '(flymake-overlay)))

; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

; change short key for ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

; sr-speedbar
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

; switch to header file
(defun my:switch-to-headerfile()
  (global-set-key (kbd "C-c o") 'ff-find-other-file)
)
(add-hook 'c-mode-hook 'my:switch-to-headerfile)
(add-hook 'c++-mode-hook 'my:switch-to-headerfile)

; for Global(sudo apt-get install global)
(setq load-path (cons "/usr/share/emacs24/site-lisp/global/gtags.el" load-path))
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (gtags-mode 1)))

;; short-key bindings
(add-hook 'gtags-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'gtags-find-tag)
            (local-set-key (kbd "M-,") 'gtags-find-rtag)))

;; create or update to start
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

;; TAGS auto-update
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

;; move-window by meta key
(windmove-default-keybindings 'meta)

;; revive
(require 'revive)
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)
