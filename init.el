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
