

;; variable
(setq dropbox-d (if windows-p "F:/Dropbox" "/windows/F/Dropbox"))
(setq note-d (concat dropbox-d "/note/"))
(setq task-file (concat dropbox-d "/task/task.org"))


;; function definition
(defun jc-edit-task () (interactive) (find-file task-file))



;; set custom prefix-key : C-x j
(define-prefix-command 'ctrl-x-j-map)
(global-set-key (kbd "C-x j") 'ctrl-x-j-map)


;; define custom key binding
(define-key ctrl-x-j-map (kbd "t") 'jc-edit-task)
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

(define-key persp-mode-map (kbd "C-x p n") 'custom-persp/note)
(define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)



;; only show china holiday
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

