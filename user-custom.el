

;; variable
(setq dropbox-d (if windows-p "D:/Dropbox" "~/Dropbox"))
(setq note-d (concat dropbox-d "/note/"))
(setq todo-d (concat dropbox-d "/task/"))
(setq task-file (concat dropbox-d "/task/task.org"))


;; function definition
;; (defun jc-edit-task () (interactive) (find-file task-file))



;; set custom prefix-key : C-x j
(define-prefix-command 'ctrl-x-j-map)
(global-set-key (kbd "C-x j") 'ctrl-x-j-map)

;; define custom key binding
;; (define-key ctrl-x-j-map (kbd "t") 'jc-edit-task)
(define-key ctrl-x-j-map (kbd "i d") 'insert-date)
(define-key ctrl-x-j-map (kbd "i t") 'insert-datetime)
(define-key ctrl-x-j-map (kbd "p e") 'encrypt-password)
(define-key ctrl-x-j-map (kbd "p d") 'decrypt-password)


;; custom perspective
(defun custom-persp/note ()
  (interactive)
  (custom-persp "note"
                (let (root)
                  (setq root (concat dropbox-d "/note/"))
                  (find-file root))
                ))

(defun custom-persp/emacs ()
  (interactive)
  (custom-persp "emacs"
                (find-file "~/emacs/init.el")))

(defun custom-persp/task ()
  (interactive)
  (custom-persp "todo"
                (let ((root (concat dropbox-d "/task")))
                   (find-file root))))

(define-key persp-mode-map (kbd "C-x p n") 'custom-persp/note)
(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)
(define-key persp-mode-map (kbd "C-x p t") 'custom-persp/task)

;; org-agenda-files
(setq org-agenda-files (list todo-d))

;; capture
(setq org-default-notes-file (concat todo-d "refile.org"))
(setq org-capture-templates 
      '(("t" "todo" entry (file org-default-notes-file) 
         ;; "* TODO %?\n %U\n %i\n %a" :clock-in t :clock-resume t)
         "* TODO %?\n %i\n" :clock-in t :clock-resume t)
        ("m" "meeting" entry (file org-default-notes-file)
         "* Meeting with %? :METTING:\n %U" :clock-in t :clock-resume t)
        ("p" "phone call" entry (file org-default-notes-file)
         "* PHONE %? :PHONE:\n %U" :clock-in t :clock-resume t)
        ("r" "respond" entry (file org-default-notes-file)
         "* TODO Respond to %:from on %:subject\n SCHEDULED: %t\n %U\n %a\n" :clock-in t :clock-resume t :immediate-finish t)
        ))




;; chinese-calendar. only show china holiday
(load (concat emacs-directory "/birthday.el"))
(setq chinese-holidays (append '(
                                 (holiday-fixed 2 14 "情人节")
                                 (holiday-fixed 3 8 "妇女节")
                                 (holiday-fixed 5 4 "青年节")
                                 (holiday-fixed 6 1 "儿童节")
                                 (holiday-fixed 9 10 "教师节")
                                 (holiday-float 5 0 2 "母亲节")
                                 (holiday-float 6 0 3 "父亲节")
                                 (holiday-lunar 1 15 "元宵节" 0)
                                 (holiday-lunar 7 7 "七夕" 0)
                                 (holiday-fixed 12 25 "圣诞节")
                                 ) cal-china-x-chinese-holidays))

(setq calendar-holidays (append chinese-holidays birthday))


;; load local.el if exist
(let ((locale-config-f (concat emacs-directory "/locale.el")))
  (if (file-exists-p locale-config-f)
      (load locale-config-f)
      ))

