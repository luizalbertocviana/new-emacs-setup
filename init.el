;; some keymaps

(defvar old-global-map
  (copy-keymap global-map))

(defvar new-global-map
  (copy-keymap global-map))

;; helper functions

(defun define-keys (keymap bindings)
  (dolist (binding bindings)
    (define-key keymap (kbd (nth 0 binding)) (nth 1 binding))))

;; keybindings

;; esc turns on the new layer of keybindings
(define-key old-global-map (kbd "<escape>")
  (lambda () (interactive) (use-global-map new-global-map)))

;; i turns off the new layer of keybindings
(define-key new-global-map (kbd "i")
  (lambda () (interactive) (use-global-map old-global-map)))

;; basic movement keys

(define-keys new-global-map
  '(("h" backward-char)
    ("j" next-line)
    ("k" previous-line)
    ("l" forward-char)
    ("L" move-end-of-line)
    ("H" move-beginning-of-line)
    
    ("C-h" backward-sentence)
    ("C-j" scroll-up-command)
    ("C-k" scroll-down-command)
    ("C-l" forward-sentence)

    ("w" forward-word)
    ("b" backward-word)))

;; g keymap

(defvar g-keymap
  (make-sparse-keymap))

(define-keys g-keymap
  '(("g" beginning-of-buffer)
    ("G" end-of-buffer)))

(define-key new-global-map (kbd "g") g-keymap)

;; d keymap

(defvar d-keymap
  (make-sparse-keymap))

(define-keys d-keymap
  '(("w" kill-word)
    ("b" backward-kill-word)
    ("L" kill-line)
    ("v" kill-region)

    ("C-l" kill-sentence)))

(define-key new-global-map (kbd "d") d-keymap)

;; editing keys

(define-keys new-global-map
  '(("x" delete-char)
    ("u" undo)
    ("p" yank)
    ("P" yank-pop)))

;; file keymap

(defvar file-map
  (make-sparse-keymap))

(define-keys file-map
  '(("f" find-file)
    ("s" save-buffer)))

;; text keymap

(defvar text-map
  (make-sparse-keymap))

(define-keys text-map
  '(("f" fill-paragraph)))

;; buffer keymap

(defvar buffer-map
  (make-sparse-keymap))

(define-keys buffer-map
  '(("l" list-buffers)
    ("b" switch-to-buffer)
    ("s" save-some-buffers)))

;; windows keymap

(defvar windows-map
  (make-sparse-keymap))

(define-keys windows-map
  '(("m" delete-other-windows)
    ("a" apropos-command)
    ("w" other-window)
    ("s" split-window-below)))

;; help keymap

(defvar help-map
  (make-sparse-keymap))

(define-keys help-map
  '(("m" describe-mode)
    ("k" describe-key)
    ("v" describe-variable)
    ("f" describe-function)))

;; leader keymap

(defvar leader
  "SPC")

(defvar leader-map
  (make-sparse-keymap))

(define-keys leader-map
  `(("w" ,windows-map)
    ("f" ,file-map)
    ("b" ,buffer-map)
    ("h" ,help-map)
    ("t" ,text-map)
    ("q" save-buffers-kill-terminal)))

(define-key new-global-map (kbd leader) leader-map)

;; visual keybindings

(define-key new-global-map (kbd "v") 'set-mark-command)

;; aborting keybinding

(define-key new-global-map (kbd "<escape>") 'keyboard-quit)

;; we start with the new global map

(use-global-map new-global-map)
