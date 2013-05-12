
;;---------------------
;; move line
;;---------------------

(defun move-line-up (n)
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down (n)
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


;;---------------------
;; move region
;;---------------------

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))


;;---------------------
;; move line & region
;;---------------------

(defun move-line-region-up (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (&optional start end n)
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down n)))

;(global-set-key (kbd "M-p") 'move-line-region-up)
;(global-set-key (kbd "M-n") 'move-line-region-down)

(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)


(provide 'move-line-region)
