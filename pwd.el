
;;;; password

(defun s-index-at(string idx)
  "get char from string by index"
  (char-to-string (elt string idx)))

(defun s-index-at-as-number(string idx)
  "get char from string as number by index"
  (string-to-number (s-index-at string idx)))

(defun remove-nth (lst n)
  (append (subseq lst 0 n) (subseq lst (1+ n) (length lst))))

(defun remove-idxs (lst idxs)
  "remove all idxs in list"
  (let ((i 0)
        (len (length lst))
        ret
        el)
    (while (< i len)
      (setq el (elt lst i))
      ;; (message "%s %s" el (not (member i idxs)))
      (if (not (member i idxs))
          (setq ret (cons el ret)))
      ;; (message "ret %s" ret)
      (setq i (1+ i)))
    (reverse ret)))

(defun letter-operate (letter num fn)
  "do operate on the letter by fn(add/subtract a number)."
  (cond ((p-number letter) (op-number letter num fn))
        ((p-lower-letter letter) (op-lower-letter letter num fn))
        ((p-upper-letter letter) (op-upper-letter letter num fn))))

(defun p-number (num)
  "detect a ascii-num is a number"
  (and (>= num 48) (<= num 57)))

(defun p-lower-letter(num)
  "detect a ascii-num is a lowwer letter"
  (and (>= num 97) (<= num 122)))

(defun p-upper-letter(num)
  "detect a ascii-num is a upper letter"
  (and (>= num 65) (<= num 90)))

(defun op-number (num operand fn)
  "apply fn on num with operand"
  (op-char-with-range num operand fn 48 57))

(defun op-lower-letter (num operand fn)
  "apply fn on lower letter with operand"
  (op-char-with-range num operand fn 97 122))

(defun op-upper-letter (num operand fn) 
  "apply fn on upper letter with operand"
  (op-char-with-range num operand fn 65 90))

(defun op-char-with-range (char operand fn min max) 
  "apply fn on char. char's range is from min to max"
    (let ((ret (funcall fn char operand))
        (range (1+ (- max min))))
    (cond ((< ret min) (setq ret (+ ret range)))
          ((> ret max) (setq ret (- ret range))))
    ret))

(defun number-to-char (num)
  "number to ascii-num"
  (string-to-char (number-to-string num)))

(defun insert-at (lst idx el)
  "insert el into list at idx"
  (let ((len (length lst))
        (i 0)
        ret)
    (setq idx (if (< idx 0) (+ len idx) idx))
    (cond ((<= idx 0) (cons el lst))
          ((>= idx len) (append lst (cons el nil)))
          (t (while (< i idx)
               (setq ret (cons (car lst) ret))
               (setq lst (cdr lst))
               (setq i (1+ i)))
             (append (reverse ret) (cons el lst))
             ))))


(defun encrypt-password (begin end)
  "encrypt my password"
  (interactive "r")
  (let* ((str (buffer-substring begin end))
         (len (length str))
         idx base-num op fn ret)
    
    (setq idx (1+ (random (1- len))))
    (setq base-num (1+ (random 8)))
    (setq op (random 9))
    (setq fn (if (= (% op 2) 1) #'+ #'-))
    (setq ret (concat (mapcar (lambda (letter) (letter-operate letter base-num fn)) str)))
    (setq ret (let ((rst (string-to-list ret)))
                (setq rst (insert-at
                           (append (cons (number-to-char idx) rst) (cons (number-to-char op) nil))
                           idx (number-to-char base-num)))
                (concat rst)
                ))
    (kill-new ret)
    (message "The encrypt password is: %s" ret)
    ret
    ))


(defun decrypt-password (begin end)
  "decrypt my password"
  (interactive "r")
  (let* ((str (buffer-substring begin end))
         (len (length str))
         (idx-base-num (s-index-at-as-number str 0))
         (base-num (s-index-at-as-number str idx-base-num))
         (op (% (s-index-at-as-number str (1- len)) 2))
         (txt (remove-idxs str (list 0 idx-base-num (1- len))))
         (fn nil)
         (ret nil))
    (setq fn (if (= op 0) #'+ #'-))
    (setq ret (concat (mapcar (lambda (letter) (letter-operate letter base-num fn)) txt)))
    (message "password is: %s" ret)
    ))

