
;; variable 
(setq dropbox-d "/windows/F/Dropbox")
(setq task-file (concat dropbox-d "/task/task.org"))
(setq note-linux-file (concat dropbox-d "/note/linux/linux.org"))
(setq note-emacs-file (concat dropbox-d "/note/emacs/emacs.org"))
(setq note-english-file (concat dropbox-d "/note/english/english-note.org"))
(setq note-mysql-file (concat dropbox-d "/note/mysql.org"))


;; function definition
(defun jc-edit-task () (interactive) (find-file task-file))
(defun jc-edit-note-emacs () (interactive) (find-file note-emacs-file))
(defun jc-edit-note-linux () (interactive) (find-file note-linux-file))
(defun jc-edit-note-mysql () (interactive) (find-file note-mysql-file))
(defun jc-edit-note-english () (interactive) (find-file note-english-file))
(defun jc-edit-emacs-init () (interactive) (find-file (concat emacs-directory "/init.el")))
(defun jc-edit-emacs-user-custom () (interactive) (find-file (concat emacs-directory "/user-custom.el")))


;; set custom prefix-key : C-x j
(define-prefix-command 'ctrl-x-j-map)
(global-set-key (kbd "C-x j") 'ctrl-x-j-map)


;; define custom key binding
(define-key ctrl-x-j-map (kbd "t") 'jc-edit-task)
(define-key ctrl-x-j-map (kbd "n e") 'jc-edit-note-emacs)
(define-key ctrl-x-j-map (kbd "n g") 'jc-edit-note-english)
(define-key ctrl-x-j-map (kbd "n l") 'jc-edit-note-linux)
(define-key ctrl-x-j-map (kbd "n m") 'jc-edit-note-mysql)
(define-key ctrl-x-j-map (kbd "e i") 'jc-edit-emacs-init)
(define-key ctrl-x-j-map (kbd "e u") 'jc-edit-emacs-user-custom)

