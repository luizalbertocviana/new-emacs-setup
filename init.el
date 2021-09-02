;; keys minor mode

(defvar keys-minor-mode-map
  (make-sparse-keymap))

(define-minor-mode keys-minor-mode
  "A minor mode to ensure some keybindings always take precedence"
  :init-value t
  :lighter " keys")

(add-hook 'minibuffer-setup-hook
	  (lambda ()
	    (keys-minor-mode 0)))

(add-hook 'after-load-functions
	  (lambda (_file)
	    (unless (eq (caar minor-mode-map-alist)
			'keys-minor-mode)
	      (let ((keys (assq 'keys-minor-mode minor-mode-map-alist)))
		(assq-delete-all 'keys-minor-mode minor-mode-map-alist)
		(add-to-list 'minor-mode-map-alist keys)))))

;; helper functions

(defun define-keys (keymap bindings)
  (dolist (binding bindings)
    (define-key keymap (kbd (nth 0 binding)) (nth 1 binding))))

;; keybindings

;; esc turns on the new layer of keybindings
(global-set-key (kbd "<escape>") 'keys-minor-mode)

;; i turns off the new layer of keybindings
(define-key keys-minor-mode-map (kbd "i") 'keys-minor-mode)

;; basic movement keys

(define-keys keys-minor-mode-map
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

(define-key keys-minor-mode-map (kbd "g") g-keymap)

;; d keymap

(defvar d-keymap
  (make-sparse-keymap))

(define-keys d-keymap
  '(("w" kill-word)
    ("b" backward-kill-word)
    ("L" kill-line)
    ("v" kill-region)

    ("C-l" kill-sentence)))

(define-key keys-minor-mode-map (kbd "d") d-keymap)

;; editing keys

(define-keys keys-minor-mode-map
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
    ("e" info-emacs-manual)
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

(define-key keys-minor-mode-map (kbd leader) leader-map)

;; visual keybindings

(define-key keys-minor-mode-map (kbd "v") 'set-mark-command)

;; aborting keybinding

(define-key keys-minor-mode-map (kbd "<escape>") 'keyboard-quit)

;; we start with keys minor mode enabled

(keys-minor-mode 1)
