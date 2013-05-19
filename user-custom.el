
;; variable 
(setq dropbox-d "/windows/F/Dropbox")
(setq task-file (concat dropbox-d "/task/task.org"))
(setq note-linux-file (concat dropbox-d "/note/linux/linux.org"))
(setq note-emacs-file (concat dropbox-d "/note/emacs/emacs.org"))
(setq note-english-file (concat dropbox-d "/note/english/english-note.org"))


;; function definition
(defun jc-task () (interactive) (find-file task-file))
(defun jc-note-emacs () (interactive) (find-file note-emacs-file))
(defun jc-note-linux () (interactive) (find-file note-linux-file))
(defun jc-note-english () (interactive) (find-file note-english-file))


;; set custom prefix-key : C-x j
(define-prefix-command 'ctrl-x-j-map)
(global-set-key (kbd "C-x j") 'ctrl-x-j-map)


;; define custom key binding
(define-key ctrl-x-j-map (kbd "t") 'jc-task)
(define-key ctrl-x-j-map (kbd "n e") 'jc-note-emacs)
(define-key ctrl-x-j-map (kbd "n g") 'jc-note-english)
(define-key ctrl-x-j-map (kbd "n l") 'jc-note-linux)

