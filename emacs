
;;---------------
;; Load plugins
;;---------------

(add-to-list 'load-path "~/emacs/plugins")


;;---------------
;; Common
;;---------------

; (set-background-color "darkblue")
(set-default-font "Consolas-10")

;; disable auto backup
(setq make-backup-files nil)

;; show line number
(global-linum-mode t)

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

;; set personal information
(setq user-full-name "jcchen")
(setq user-mail-address "junchangchen@gmail.com")


;;---------------
;; Indent setting
;;---------------

(add-hook 'html-mode-hook
  (lambda () (set (make-local-variable 'sgml-mode-hook) 4)))



;;---------------
;; Mode Setting
;;---------------

; electric-pair-mode auto turn on
(electric-pair-mode t)


;; Org mode
;; auto turn on the auto-complete-mode & visual-line-mode
(add-hook 'org-mode-hook (lambda ()
  (interactive)
  (auto-complete-mode t)
  (visual-line-mode)))






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


;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)

;; json
;(require 'json)


;; ------------------------
;; Language special setting
;; ------------------------

;; Haskell mode





