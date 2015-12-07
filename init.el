 

;;------------------
;; define variables
;;------------------
(defvar windows-p (string-match "windows" (symbol-name system-type)))
;; (if windows-p (message "it's windows") (message "it's linux"))
(defvar reload-on-save nil)
(defvar kill-complation-buffer nil)


;;---------------
;; Load plugins
;;---------------
(setq emacs-directory "~/emacs")
(add-to-list 'load-path (concat emacs-directory "/plugins"))
(add-to-list 'load-path (concat emacs-directory "/themes"))




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



(defun reload ()
  "reload the page"

  ;; send nc command in windows may cause emacs broken.
  ;; so making this function only works on linux. 

  ;; (unless windows-p
  ;;   (shell-command-to-string "echo \"reload\" | nc localhost 32000"))
  
  (if windows-p
      (let ((cmd (concat (file-truename emacs-directory) "/reload.exe")))
        (shell-command-to-string cmd))
    (shell-command-to-string "echo \"reload\" | nc localhost 32000"))
  
  (message "reload page"))



(defun compilation-exit-autoclose (status code msg)
  (when (and (eq status 'exit) (zerop code))
    (bury-buffer)
    (let (buffer)
      (setq buffer (get-buffer "*compilation*"))
      (if kill-complation-buffer
          (delete-window (get-buffer-window buffer))
        (switch-to-prev-buffer (get-buffer-window buffer)))
      
      ;; if less-css-mode reload page
      (when (with-current-buffer buffer
            (search-forward "lessc" nil t))
          (reload)
      )))
  (cons msg code))
(setq compilation-exit-message-function 'compilation-exit-autoclose)



(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (save-excursion
    (if (region-active-p)
        (duplicate-region arg)
      (duplicate-current-line arg))))

(defun duplicate-region (num &optional start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (let* ((start (or start (region-beginning)))
         (end (or end (region-end)))
         (region (buffer-substring start end)))
    (goto-char start)
    (dotimes (i num)
      (insert region))))

(defun duplicate-current-line (num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (when (eq (point-at-eol) (point-max))
    (goto-char (point-max))
    (newline)
    (forward-char -1))
  (duplicate-region num (point-at-bol) (1+ (point-at-eol))))



(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))



(defun region-as-string ()
  (buffer-substring (region-beginning)
                    (region-end)))

(defun isearch-forward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-forward))

(defun isearch-backward-use-region ()
  (interactive)
  (when (region-active-p)
    (add-to-history 'search-ring (region-as-string))
    (deactivate-mark))
  (call-interactively 'isearch-backward))

(defun insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun insert-datetime ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M" (current-time))))

(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


;;---------------
;; Common
;;---------------

; (set-background-color "darkblue")
(set-default-font "Ubuntu Mono-10")        ;consolas-10
(set-fontset-font t 'han (font-spec :family "Microsoft Yahei" :size 12))
;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))

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

;; set default fill-colume width 88
(setq-default fill-column 88)

;; set frame title
(setq frame-title-format "%b - Emacs")

;; set text-mode as the default major mode
(setq default-major-mode 'text-mode)

;; set default coding for new file
(setq default-buffer-file-coding-system 'utf-8-unix)

;; set personal information
(setq user-full-name "jcchen")
(setq user-mail-address "junchangchen@gmail.com")

;; hide toolbar
(tool-bar-mode -1)

;; quiet, please! No dinging!
(setq ring-bell-function 'ignore)


;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))


;; turn on auto revert mode
(global-auto-revert-mode t)

;; coding
(define-coding-system-alias 'UTF-8 'utf-8)

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


;; configure those mode derived in CC Mode at once.
;; such as: Groovy, java, C and related
(defun my-c-mode-hook ()
   (setq indent-tabs-mode nil
         c-basic-offset 4))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)



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
;; (defun duplicate-line (n) 
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (kill-line)
;;   (yank)
;;   (open-line 1)
;;   (next-line 1)
;;   (yank))

;; (global-set-key (kbd "C-M-<down>") 'dumplicate-line)


;; global keybinding for Org-mode agenda etc
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; global keybinding for select next window, the same as "C-x o"
(global-set-key (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "C-x p f") 'ffip)


(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)


;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

(global-set-key (kbd "C-c j") 'delete-indentation)
;; (global-set-key (kbd "C-c o") 'switch-to-minibuffer)
;; (global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'


;; -----------------
;; Plugins
;; -----------------


;; zen coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(add-hook 'nxhtml-mode-hook 'zencoding-mode)
;; reset key bind
(define-key zencoding-mode-keymap (kbd "<C-return>") nil)
(define-key zencoding-mode-keymap (kbd "<C-M-return>") 'zencoding-expand-line)


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




;; perspective-el
(add-to-list 'load-path "~/emacs/plugins/perspective-el")
(require 'perspective)
(persp-mode t)
;; define custom-persp macro
(defmacro custom-persp (name &rest body)
  `(let ((initialize (not (gethash ,name perspectives-hash)))
         (current-perspective persp-curr))
     (persp-switch ,name)
     (when initialize ,@body)
     (setq persp-last current-perspective)))





;; ntcmd mode
(require 'ntcmd)
(add-to-list 'auto-mode-alist '("\\.bat$" . ntcmd-mode))
(add-to-list 'auto-mode-alist '("\\.cmd$" . ntcmd-mode))


;; simple-httpd
;; (add-to-list 'load-path "~/emacs/plugins/emacs-http-server")
;; (require 'simple-httpd)
;; (setq httpd-root (if windows-p "D:/temp/" "/srv/www"))
;; ;; change the httpd-port to 8008
;; (setq httpd-port 8008)


;; skewer-mode
;; (add-to-list 'load-path "~/emacs/plugins/skewer-mode")
;; (require 'skewer-mode)
;; (require 'skewer-repl)
;; (require 'skewer-html)
;; (require 'skewer-css)


;; web-mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; (defun web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 4)
;;   (setq web-mode-css-indent-offset 4)
;;   (setq web-mode-code-indent-offset 4)
;;   (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "peru"))
;; (add-hook 'web-mode-hook 'web-mode-hook)


;; nxhtml
(add-to-list 'load-path "~/emacs/plugins/nxhtml/elisp")
(load "~/emacs/plugins/nxhtml/elisp/autostart.el")
(setq mumamo-background-colors nil)




;; find-file-in-project
(require 'find-file-in-project)

;; Helper method to create local settings
;; https://github.com/magnars/.emacs.d/blob/master/setup-ffip.el
(defun ffip-set-root (path)
  "By default, ffip consider the folder as a root that cotains .git file.
  If our project not a git project. You can use this function to define the project root."
  (set (make-local-variable 'ffip-project-root) path))

(defun ffip--create-exclude-find-options (names)
  (mapconcat (lambda (name)
               (concat "-not -regex \".*" name ".*\"")) names " "))

(defun ffip-local-excludes (&rest names)
  "Given a set of names, will exclude results with those names in the path.

  Example:
  (ffip-local-excludes \"target\" \"overlays\")"
  (set (make-local-variable 'ffip-find-options)
       (ffip--create-exclude-find-options names)))

(defun ffip-local-patterns (&rest patterns)
  "An exhaustive list of file name patterns to look for.

  Example:
  (ffip-local-patterns \"*.js\" \"*.jsp\" \"*.css\")"
  (set (make-local-variable 'ffip-patterns) patterns))

;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; (setq ffip-find-options
;;       (ffip--create-exclude-find-options
;;        '("node_modules"
;;          "bower_components"
;;          "target"
;;          )))

(let ((exclude-option (ffip--create-exclude-find-options
       '("node_modules"
         "bower_components"
         "target"
         "platforms"
         )))
      (other-option '" -not -path '*/\.*'"))
  ;; (message (concat exclude-option other-option))
  (setq ffip-find-options (concat exclude-option other-option)))


(setq ffip-project-file '(".project" ".git"))
;; (setq ffip-project-file ".git")
; TODO: define local patterns, local excludes bind to perspective
(setq ffip-patterns '("*.html" "*.htm" "*.org" "*.txt" "*.el" "*.py" "*.js" 
   "*.java" "*.jsp" "*.php" "*.css" "*.less" "*.xml" "*.properties" "*.hs" "*.coffee"))

(if windows-p (setq ffip-find-executable "C:/PROGRA~2/Git/bin/find.exe"))



;; slime & slime.js
;; To install, use package-install. Don't do it manually.
;;
;; Installation refer to: https://github.com/Gozala/slime-js
;;
;; Install:
;;     npm istall swank-js -g
;;     M-x package-install slime-js
;;
;; Usage refer to: https://github.com/swank-js/swank-js
;; 
;; slime.js setting

;; (global-set-key [f5] 'slime-js-reload)
;; (add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))
;; (add-hook 'css-mode-hook (lambda ()
;;   (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
;;   ;; (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)
;;   ))



;; smooth-scrolling
(require 'smooth-scrolling)


;; ace jump mode
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


;; info-look
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))




;; django mode
;; (require 'python)
;; (require 'python-django)


;; Pymacs
;; (require 'pymacs)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")


;; emacs-for-python
;; (load-file "~/emacs/plugins/emacs-for-python/epy-init.el")
;; (add-to-list 'load-path "~/emacs/plugins/emacs-for-python")
;; (require 'epy-setup)      ;; It will setup other loads, it is required!
;; (require 'epy-python)     ;; If you want the python facilities [optional]
;; (require 'epy-completion) ;; If you want the autocompletion settings [optional]
;; (require 'epy-editing)    ;; For configurations related to editing [optional]
;; (require 'epy-bindings)   ;; For my suggested keybindings [optional]
;; (require 'epy-nose)       ;; For nose integration

;; (epy-django-snippets)             ;; django snippets
;; (epy-setup-ipython)

;; (global-hl-line-mode t)               ;; To enable
;; (epy-setup-checker "pyflakes %f") ;; flymake checker
;; (set-face-background 'hl-line "black") ;;


;; dash
(add-to-list 'load-path "~/emacs/plugins/dash")
(require 'dash)

;; wrap-region
(add-to-list 'load-path "~/emacs/plugins/wrap-region")
(require 'wrap-region)
;; (wrap-region-mode t)
(wrap-region-global-mode t)




;; jedi
(add-to-list 'load-path "~/emacs/plugins/emacs-deferred")
(require 'concurrent)
(require 'deferred)

(add-to-list 'load-path "~/emacs/plugins/ctable")
(require 'ctable)

(add-to-list 'load-path "~/emacs/plugins/emacs-epc")
(require 'epc)

(add-to-list 'load-path "~/emacs/plugins/emacs-python-environment")
(require 'python-environment)

(add-to-list 'load-path "~/emacs/plugins/emacs-jedi")
(require 'jedi)
(autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; (setq jedi:server-command '("~/emacs/plugins/emacs-jedi/jediepcserver.py"))
(setq jedi:server-command (list "python" jedi:server-script))
(add-hook 'python-mode-hook 'jedi:ac-setup)


;; js2-mode
(add-to-list 'load-path "~/emacs/plugins/js2-mode")
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode)) 


;; groovy
(add-to-list 'load-path "~/emacs/plugins/emacs-groovy-mode")

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))

;;; turn on syntax highlighting
;; (global-font-lock-mode 1)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\.yml$" . yaml-mode))



;; org-impress-js
(add-to-list 'load-path "~/emacs/plugins/org-impress-js")
(require 'org-impress-js)


;; cal-china-x
(add-to-list 'load-path "~/emacs/plugins/cal-china-x.el")
(require 'cal-china-x)

;; dirtree
(require 'tree-mode)
(require 'windata)
(require 'dirtree)

;; open dirtree in current project
(defun dirtree-prj ()
  (interactive)
  (dirtree (ffip-project-root) t))

(defun dirtree-prj-in-buffer ()
  (interactive)
  (dirtree-in-buffer (ffip-project-root) t))




;; ------------------------
;; Themes
;; ------------------------
;; (require 'color-theme)
;; (require 'sublime-text-2)
;; (require 'color-theme-monokai)

;; The theme auto turn on
(require 'molokai-theme)
;; (sublime-text-2)



;; ------------------------
;; Language special setting
;; ------------------------

;; Haskell mode
(add-to-list 'load-path "~/emacs/plugins/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/emacs/plugins/haskell-mode/")

;; (custom-set-variables                   
;;  '(haskell-mode-hook 'turn-on-haskell-indentation)
;; )

;; set indentation to 4.
;; Extra indentation after an indentation to the left (e.g. after do)
(setq haskell-indentation-left-offset 4)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; coffee-script
(add-to-list 'load-path "~/emacs/plugins/coffee-mode/")
(require 'coffee-mode)

(defun my/coffee-compile-file ()
  (interactive)
  (coffee-compile-file)
  (when reload-on-save
    (reload)))

;; defined custom key-bindings
(eval-after-load "coffee-mode"
  '(progn
     (setq coffee-indent-tabs-mode t)
     (define-key coffee-mode-map (kbd "C-c C-c") 'my/coffee-compile-file))) ;coffee-compile-file




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
  ;; (visual-line-mode)
  (auto-fill-mode t)
  (electric-indent-mode 0)
  ))


;; sgml mode
(add-hook 'nxhtml-mode-hook (lambda () 
  (interactive)
  (local-set-key (kbd "C-c C-w") 'html-wrap-in-tag)
  ))


;; nxml mode
(setq nxml-child-indent 4
      nxml-outline-child-indent 4
      nxml-slash-auto-complete-flag t)


;; less css mode
(require 'less-css-mode)
(add-hook 'less-css-mode-hook (lambda ()
  (interactive)
  (auto-complete-mde t)
  ))
;; (setq less-css-compile-at-save t)       ;compile at save


;; scss
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)


;; plantuml
;; active org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "~/emacs/res/plantuml.jar"))

(require 'plantuml-mode)

;; when load plantuml-mode at startup.
;; emacs will split window to vertical. the below window shows *Message* buffer.
;; The following code to fixed this behavour.
(add-hook 'emacs-startup-hook
          (lambda () (delete-other-windows)) t)

(require 'plantuml_helpers)


;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; angular js snippets
(add-to-list 'load-path "~/emacs/plugins/angular-snippets.el/")
(require 'angular-snippets)
(eval-after-load "sgml-mode"
  '(define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point))

;; 
;; tern
;; http://ternjs.net/doc/manual.html
;; 
(add-to-list 'load-path "~/emacs/plugins/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;; this line only needed when `tern` cannot start correctly.
;; e.g. "~/emacs" dir install in a windows partition.
;; in this case, you need to install `tern` command manually via `npm install -g tern`
(setq tern-command (list "tern"))


;; common lisp SLIME
;; To use slime-helper, install quicklisp, sbcl in system
(let ((f (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p f)
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (setq inferior-lisp-program "/usr/bin/sbcl")))


;; company-mode. required by scala-mode
(add-to-list 'load-path "~/emacs/plugins/company-mode/")
(require 'company)

;; 
;; scala
;;
(add-to-list 'load-path "~/emacs/plugins/scala-mode2/")
(require 'scala-mode2)

(add-to-list 'load-path "~/emacs/plugins/sbt-mode/")
(require 'sbt-mode)

(add-to-list 'load-path "~/emacs/plugins/ensime-emacs/")

;; If necessary, add JDK_HOME or JAVA_HOME to the environment
;; (setenv "JDK_HOME" "/path/to/jdk")
;; If necessary, make sure "sbt" and "scala" are in the PATH environment
;; (setenv "PATH" (concat "/path/to/sbt/bin:" (getenv "PATH")))
;; (setenv "PATH" (concat "/path/to/scala/bin:" (getenv "PATH")))
;; You can also customize `ensime-inf-get-project-root' and `ensime-inf-get-repl-cmd-line'
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)




;;--------------------------------
;; package
;;--------------------------------
(require 'package)
(add-to-list 'package-archives 
  '("marmalade" . "http://marmalade-repo.org/packages/") t) (package-initialize)



;;------------------------------
;; My hook
;;------------------------------

;; reload page after save.
(add-hook 'after-save-hook (lambda ()
                             (interactive)
                             (when reload-on-save
                               (let (filename)
                                 (setq filename (buffer-name))
                                 (when (or (string-match-p "\\.html$" filename)
                                           (string-match-p "\\.htm$" filename)
                                           (string-match-p "\\.css$" filename)
                                           (string-match-p "\\.js$" filename))
                                   (reload))))
                             ))


;;--------------------------------
;; org-mode for jekyll
;;--------------------------------
(load (concat emacs-directory "/org-jekyll.el"))
(require 'org-jekyll)


;;--------------------------------
;; Others
;;--------------------------------
(require 'workaround-mumamo-buffer-file-name-warnings)



;;--------------------------------
;; SOME special setting for user
;;--------------------------------
(setq user-custom-file (concat emacs-directory "/user-custom.el"))
(load user-custom-file)
(load (concat emacs-directory "/pwd.el"))



