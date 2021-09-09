(defun dired-dwim-target-directory ()
  (let* ((get-buffer-dir (lambda (buffer)
                           (with-current-buffer buffer
                             (and (eq major-mode 'dired-mode)
                                  (dired-current-directory)))))
         (this-dir (funcall get-buffer-dir (current-buffer)))
         (other-dir (seq-some (lambda (buffer)
                                (let ((buffer-dir (funcall get-buffer-dir buffer)))
                                  (and (not (equal buffer-dir this-dir))
                                       buffer-dir)))
                              (buffer-list))))
    (if dired-dwim-target
        (or other-dir this-dir)
      this-dir)))

(defun define-keys (keymap bindings)
  (dolist (binding bindings)
    (define-key keymap (kbd (nth 0 binding)) (nth 1 binding))))

(defun open-newline-below ()
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-newline-above ()
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun insert-after-movement (movement)
  `(lambda ()
     (interactive)
     (funcall #',movement)
     (keys-minor-mode -1)))

(defun kill-inside-movement (backward forward)
  `(lambda (arg)
     (interactive "p")
     (funcall #',backward 1)
     (set-mark (point))
     (funcall #',forward arg)
     (kill-region (mark) (point))))

(defun kill-movement (movement)
  `(lambda (arg)
     (interactive "p")
     (set-mark (point))
     (funcall #',movement arg)
     (kill-region (mark) (point))))

(defun select-whole-line (arg)
  (beginning-of-line)
  (set-mark (point))
  (next-line (1- arg))
  (end-of-line)
  (forward-char))

(defun change-inside-movement (backward forward)
  `(lambda (arg)
     (interactive "p")
     (funcall (kill-inside-movement ',backward ',forward) arg)
     (indent-for-tab-command)
     (keys-minor-mode -1)))

(defun change-movement (movement)
  `(lambda (arg)
     (interactive "p")
     (funcall (kill-movement ',movement) arg)
     (indent-for-tab-command)
     (keys-minor-mode -1)))

(defun select-whole-line-until-line-break (arg)
  (beginning-of-line)
  (set-mark (point))
  (next-line (1- arg))
  (end-of-line))

(defun yank-inside-movement (backward forward)
  `(lambda (arg)
     (interactive "p")
     (funcall #',backward 1)
     (set-mark (point))
     (funcall #',forward arg)
     (kill-ring-save (mark) (point))))

(defun yank-movement (movement)
  `(lambda (arg)
     (interactive "p")
     (set-mark (point))
     (funcall #',movement arg)
     (kill-ring-save (mark) (point))))

(defun local-leader ()
  nil)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
