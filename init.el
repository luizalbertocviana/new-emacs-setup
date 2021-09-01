;; some keymaps

(defvar old-global-map
  (copy-keymap global-map))

(defvar new-global-map
  (copy-keymap global-map))

;; esc turns on the new layer of keybindings
(define-key old-global-map (kbd "<escape>")
  (lambda () (interactive) (use-global-map new-global-map)))

;; basic movement keys

(define-key new-global-map (kbd "h") 'backward-char)
(define-key new-global-map (kbd "j") 'next-line)
(define-key new-global-map (kbd "k") 'previous-line)
(define-key new-global-map (kbd "l") 'forward-char)
(define-key new-global-map (kbd "L") 'move-end-of-line)
(define-key new-global-map (kbd "H") 'move-beginning-of-line)

;; i turns off the new layer of keybindings
(define-key new-global-map (kbd "i")
  (lambda () (interactive) (use-global-map old-global-map)))

(define-key new-global-map (kbd "C-h") 'backward-sentence)
(define-key new-global-map (kbd "C-j") 'scroll-up-command)
(define-key new-global-map (kbd "C-k") 'scroll-down-command)
(define-key new-global-map (kbd "C-l") 'forward-sentence)

(define-key new-global-map (kbd "w") 'forward-word)
(define-key new-global-map (kbd "b") 'backward-word)

;; g keymap

(defvar g-keymap
  (make-sparse-keymap))

(define-key g-keymap (kbd "g") 'beginning-of-buffer)
(define-key g-keymap (kbd "G") 'end-of-buffer)

(define-key new-global-map (kbd "g") g-keymap)

;; d keymap

(defvar d-keymap
  (make-sparse-keymap))

(define-key d-keymap (kbd "w") 'kill-word)
(define-key d-keymap (kbd "b") 'backward-kill-word)
(define-key d-keymap (kbd "L") 'kill-line)
(define-key d-keymap (kbd "C-l") 'kill-sentence)
(define-key d-keymap (kbd "v") 'kill-region)

(define-key new-global-map (kbd "d") d-keymap)

;; editing keys

(define-key new-global-map (kbd "x") 'delete-char)
(define-key new-global-map (kbd "u") 'undo)
(define-key new-global-map (kbd "p") 'yank)
(define-key new-global-map (kbd "P") 'yank-pop)

;; visual keybindings

(define-key new-global-map (kbd "v") 'set-mark-command)

(use-global-map new-global-map)
