

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

