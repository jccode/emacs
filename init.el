
;;---------------
;; Load plugins
;;---------------
(setq emacs-directory "~/emacs")
(add-to-list 'load-path (concat emacs-directory "/plugins"))


;;----------------
;; Function define
;;----------------
(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))



;;---------------
;; Common
;;---------------

; (set-background-color "darkblue")
(set-default-font "Consolas-10")

;; disable auto backup
(setq make-backup-files nil)

;; show line number
(global-linum-mode t)

;; highline parentheses match
(show-paren-mode 1)

;; show column-number
;; which is enabled by default

;; disable the startup page
(setq inhibitinhibit-startup-message t)

;; turn on ido-mode & icomplete-mode
(ido-mode t)
(icomplete-mode t)

;; set frame title
(setq frame-title-format "emacs@%b")

;; set text-mode as the default major mode
(setq default-major-mode 'text-mode)

;; set default coding for new file
(setq default-buffer-file-coding-system 'utf-8-unix)

;; set personal information
(setq user-full-name "jcchen")
(setq user-mail-address "junchangchen@gmail.com")


;;---------------
;; Indent setting
;;---------------

;; default tab with 4 spaces
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

;; sgml
(setq sgml-basic-offset 4)

;; xml
;(add-hook nxml-mode-hook
;  (lambda () 
;    (set (make-local-variable 'sgml-mode) t)))



;;---------------
;; Key Binding
;;---------------

(require 'move-line-region)

;; auto indent
;(global-set-key (kbd "RET") 'newline-and-indent)


;; next line (C-<return>)
;(global-set-key (kbd "C-<return>") (lambda (number)
;  (interactive "p")
;  (end-of-line)
;  (newline-and-indent)
;))


;; duplicated line
(defun duplicate-line (n) 
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(global-set-key (kbd "C-M-<down>") 'dumplicate-line)


;; global keybinding for Org-mode agenda etc
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)




;; -----------------
;; Plugins
;; -----------------

;; zen coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes


;; yasnippet
(add-to-list 'load-path "~/emacs/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(yas-minor-mode-on)


;; auto-complete
(add-to-list 'load-path "~/emacs/plugins/popup-el")
(add-to-list 'load-path "~/emacs/plugins/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs/plugins/auto-complete/dict")
(ac-config-default)


;; delete selection mode
(require 'delsel)
(delete-selection-mode 1)


;; smart-tab
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

;; json
;(require 'json)


;; multiple-cursor
(add-to-list 'load-path "~/emacs/plugins/multiple-cursors")
(require 'multiple-cursors)
;; global keybinding for multiple-cursor
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; expand-region
(add-to-list 'load-path "~/emacs/plugins/expand-region")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;; smex
(require 'smex)
;(smex-initialize)
;; key binds for smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is your old M-x.


;; js2-mode
(add-to-list 'load-path "~/emacs/plugins/js2-mode")
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode)) 


;; slime & slime.js
;; To install, use package-install. Don't do it manually.
;;
;; Installation refer to: https://github.com/Gozala/slime-js
;;
;; Install:
;;     npm istall swank-js -g
;;     M-x install-package slime-js
;;
;; Usage refer to: https://github.com/swank-js/swank-js
;; 
;; slime.js setting
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode)))
(add-hook 'css-mode-hook (lambda ()
  (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
  ;; (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)
  ))


;; perspective-el
(add-to-list 'load-path "~/emacs/plugins/perspective-el")
(require 'perspective)


;; ------------------------
;; Language special setting
;; ------------------------

;; Haskell mode





;;---------------
;; Mode Setting
;;---------------

;; electric-pair-mode auto turn on
(electric-pair-mode t)


;; electric-indent-mode when programing
(add-hook 'prog-mode-hook (lambda() 
  (interactive)
  (electric-indent-mode t)
  ))


;; Org mode
;; auto turn on the auto-complete-mode & visual-line-mode
(add-hook 'org-mode-hook (lambda ()
  (interactive)
  (auto-complete-mode t)
  (visual-line-mode)
  (auto-fill-mode -1)
  (electric-indent-mode -1)
  ))


;; sgml mode
(add-hook 'sgml-mode-hook (lambda () 
  (interactive)
  (local-set-key (kbd "C-c C-w") 'html-wrap-in-tag)
  ))


;; nxml mode
(setq nxml-child-indent 4
      nxml-outline-child-indent 4
      nxml-slash-auto-complete-flag t)


;; less css mode
(require 'less-css-mode)
(add-hook 'less-css-mode-hook 'auto-complete-mode)
;; (setq less-css-compile-at-save t)       ;compile at save



;;--------------------------------
;; package
;;--------------------------------
(require 'package)
(add-to-list 'package-archives 
  '("marmalade" . "http://marmalade-repo.org/packages/") t) (package-initialize)




;;--------------------------------
;; some special setting for user
;;--------------------------------
(setq user-custom-file (concat emacs-directory "/user-custom.el"))
(load user-custom-file)
