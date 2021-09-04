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
