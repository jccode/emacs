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

; turn on ido-mode & icomplete-mode
(ido-mode t)
(icomplete-mode t)


;;---------------
;; Indent setting
;;---------------

(add-hook 'html-mode-hook
  (lambda () (set (make-local-variable 'sgml-mode-hook) 4)))




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


