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

;; insert entering keybindings

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

(define-keys keys-minor-mode-map
  `(("i" keys-minor-mode)
    ("a" ,(insert-after-movement 'forward-char))
    ("A" ,(insert-after-movement 'end-of-line))
    ("o" ,(insert-after-movement 'open-newline-below))
    ("O" ,(insert-after-movement 'open-newline-above))
    ("I" ,(insert-after-movement 'back-to-indentation))))

;; basic movement keys

(define-keys keys-minor-mode-map
  `(("h" backward-char)
    ("j" next-line)
    ("k" previous-line)
    ("J" end-of-defun)
    ("K" beginning-of-defun)
    ("l" forward-char)
    ("L" move-end-of-line)
    ("$" move-end-of-line)
    ("H" back-to-indentation)
    ("0" move-beginning-of-line)
    ("G" end-of-buffer)

    ("f" isearch-forward-regexp)
    ("F" isearch-backward-regexp)
    
    ("C-h" backward-sentence)
    ("C-j" scroll-up-command)
    ("C-k" scroll-down-command)
    ("C-l" forward-sentence)

    ("w" forward-to-word)
    ("W" forward-whitespace)
    ("e" forward-word)
    ("b" backward-word)
    ("B" ,(lambda (arg) (interactive "p") (forward-whitespace (* -1 arg))))))

;; g keymap

(defvar g-keymap
  (make-sparse-keymap))

(define-keys g-keymap
  '(("g" beginning-of-buffer)
    ("l" goto-line)
    ("j" join-line)
    ("G" end-of-buffer)
    ("d" xref-find-definitions)))

(define-key keys-minor-mode-map (kbd "g") g-keymap)

;; d keymap

(defvar d-keymap
  (make-sparse-keymap))

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

(define-keys d-keymap
  `(("w" ,(kill-movement 'forward-to-word))
    ("W" ,(kill-movement 'forward-whitespace))
    ("b" ,(kill-movement 'backward-word))
    ("d" ,(kill-movement 'select-whole-line))
    ("L" ,(kill-movement 'end-of-line))
    ("S" ,(kill-movement 'forward-sexp))
    ("o" delete-blank-lines)
    ("v" kill-region)
    ("s" just-one-space)

    ("C-l" ,(kill-movement 'forward-sentence))
    ("C-h" ,(kill-movement 'backward-sentence))))

(define-key keys-minor-mode-map (kbd "d") d-keymap)

;; c keymap

(defvar c-keymap
  (make-sparse-keymap))

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

(define-keys c-keymap
  `(("w" ,(change-movement 'forward-to-word))
    ("W" ,(change-movement 'forward-whitespace))
    ("b" ,(change-movement 'backward-word))
    ("c" ,(change-movement 'select-whole-line-until-line-break))
    ("L" ,(change-movement 'end-of-line))
    ("S" ,(change-movement 'forward-sexp))

    ("C-l" ,(change-movement 'forward-sentence))
    ("C-h" ,(change-movement 'backward-sentence))))

(define-key keys-minor-mode-map (kbd "c") c-keymap)

;; v keymap

(defvar v-keymap
  (make-sparse-keymap))

(define-keys v-keymap
  '(("v" set-mark-command)
    ("b" mark-whole-buffer)
    ("w" mark-word)
    ("s" mark-sexp)
    ("p" mark-paragraph)
    ("P" mark-page)
    ("e" er/expand-region)
    ("f" mark-defun)
    ("r" rectangle-mark-mode)
    ("x" exchange-point-and-mark)))

(define-key keys-minor-mode-map (kbd "v") v-keymap)

;; r keymap

(defvar r-keymap
  (make-sparse-keymap))

(define-keys r-keymap
  '(("m" point-to-register)
    ("g" jump-to-register)
    ("a" append-to-register)
    ("w" window-configuration-to-register)
    ("f" frameset-to-register)
    ("y" copy-to-register)
    ("v" view-register)
    ("p" insert-register)))

(define-key keys-minor-mode-map (kbd "r") r-keymap)

;; t keymap

(defvar t-keymap
  (make-sparse-keymap))

(define-keys t-keymap
  '(("t" transpose-lines)
    ("c" transpose-chars)
    ("w" transpose-words)
    ("s" transpose-sexps)))

(define-key keys-minor-mode-map (kbd "t") t-keymap)

;; m keymap

(defvar m-keymap
  (make-sparse-keymap))

(define-keys m-keymap
  '(("m" kmacro-end-or-call-macro)
    ("s" kmacro-start-macro-or-insert-counter)
    ("v" apply-macro-to-region-lines)
    ("q" kbd-macro-query)
    ("n" kmacro-name-last-macro)
    ("b" kmacro-bind-to-key)
    ("i" insert-keyboard-macro)
    ("e" kmacro-edit-macro)))

(define-key keys-minor-mode-map (kbd "m") m-keymap)

;; z keymap

(defvar z-keymap
  (make-sparse-keymap))

(define-keys z-keymap
  '(("z" recenter-top-bottom)))

(define-key keys-minor-mode-map (kbd "z") z-keymap)

;; editing keys

(define-keys keys-minor-mode-map
  `(("x" delete-forward-char)
    ("u" undo)
    ("y" kill-ring-save)
    ("p" yank)
    ("P" yank-pop)
    ("." repeat)
    ("s" isearch-forward-symbol-at-point)
    (">" repeat-complex-command)

    ("C-s" isearch-forward-regexp)
    ("C-r" isearch-backward-regexp)

    ("D" ,(kill-movement 'end-of-line))
    ("C" ,(change-movement 'end-of-line))

    ("M-l" completion-at-point)))

;; bookmark keymap

(defvar bookmark-map
  (make-sparse-keymap))

(define-keys bookmark-map
  '(("s" bookmark-set)
    ("g" bookmark-jump)
    ("l" list-bookmarks)
    ("w" bookmark-save)))

;; file keymap

(defvar file-map
  (make-sparse-keymap))

(define-keys file-map
  `(("f" find-file)
    ("d" dired)
    ("D" ediff)
    ("r" recentf-open-files)
    ("s" save-buffer)
    ("w" write-file)
    ("b" ,bookmark-map)))

;; rectangle keymap

(defvar rectangle-map
  (make-sparse-keymap))

(define-keys rectangle-map
  '(("d" kill-rectangle)
    ("c" clear-rectangle)
    ("s" open-rectangle)
    ("p" yank-rectangle)
    ("i" string-insert-rectangle)
    ("n" rectangle-number-lines)
    ("r" string-rectangle)
    ("y" copy-rectangle-as-kill)))

;; table column keymap

(defvar table-column-map
  (make-sparse-keymap))

(define-keys table-column-map
  '(("d" table-delete-column)
    ("i" table-insert-column)))

;; table row keymap

(defvar table-row-map
  (make-sparse-keymap))

(define-keys table-row-map
  '(("d" table-delete-row)
    ("i" table-insert-row)))

;; table keymap

(defvar table-map
  (make-sparse-keymap))

(define-keys table-map
  `(("C" table-capture)
    ("R" table-recognize)
    ("S" table-generate-source)
    ("a" table-justify)
    ("i" table-insert)
    ("m" table-span-cell)
    ("r" ,table-row-map)
    ("c" ,table-column-map)
    ("s" table-split-cell)))

;; text keymap

(defvar text-map
  (make-sparse-keymap))

(define-keys text-map
  `(("f" fill-paragraph)
    ("a" align-regexp)
    ("j" delete-indentation)
    ("v" visual-line-mode)
    ("u" upcase-dwim)
    ("S" ispell-word)
    ("s" sort-lines)
    ("o" occur)
    ("R" query-replace-regexp)
    ("r" ,rectangle-map)
    ("l" downcase-dwim)
    ("c" count-words)))

;; buffer keymap

(defvar buffer-map
  (make-sparse-keymap))

(define-keys buffer-map
  '(("l" list-buffers)
    ("b" switch-to-buffer)
    ("c" clone-indirect-buffer)
    ("d" kill-this-buffer)
    ("q" kill-buffer-and-window)
    ("m" ibuffer)
    ("r" revert-buffer)
    ("s" save-some-buffers)))

;; windows keymap

(defvar windows-map
  (make-sparse-keymap))

(define-keys windows-map
  '(("m" delete-other-windows)
    ("d" delete-window)
    ("a" apropos-command)
    ("w" other-window)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("v" split-window-right)
    ("s" split-window-below)))

;; help keymap

(defvar help-map
  (make-sparse-keymap))

(define-keys help-map
  '(("m" describe-mode)
    ("w" woman)
    ("a" apropos)
    ("s" info-lookup-symbol)
    ("b" describe-bindings)
    ("k" describe-key)
    ("e" info-emacs-manual)
    ("v" describe-variable)
    ("f" describe-function)))

;; program find keymap

(defvar program-find-map
  (make-sparse-keymap))

(define-keys program-find-map
  '(("r" xref-find-references)
    ("d" xref-find-definitions)))

;; lsp keymap

(defvar lsp-map
  (make-sparse-keymap))

(define-keys lsp-map
  '(("R" lsp-workspace-restart)
    ("f" lsp-format-buffer)
    ("i" lsp-organize-imports)
    ("q" lsp-workspace-shutdown)
    ("r" lsp-rename)))

;; program keymap

(defvar program-map
  (make-sparse-keymap))

(define-keys program-map
  `(("c" comment-line)
    ("f" ,program-find-map)
    ("l" ,lsp-map)
    ("e" next-error)
    ("p" check-parens)
    ("B" compile)
    ("b" recompile)))

;; conflict keep keymap

(defvar keep-keymap
  (make-sparse-keymap))

(define-keys keep-keymap
  '(("a" smerge-keep-all)
    ("b" smerge-keep-base)
    ("c" smerge-keep-current)
    ("l" smerge-keep-lower)
    ("u" smerge-keep-upper)))

;; conflict keymap

(defvar conflict-map
  (make-sparse-keymap))

(define-keys conflict-map
  `(("N" smerge-prev)
    ("R" smerge-resolve-all)
    ("n" smerge-next)
    ("r" smerge-resolve)
    ("s" smerge-swap)
    ("k" ,keep-keymap)))

;; version keymap

(defvar version-map
  (make-sparse-keymap))

(define-keys version-map
  '(("g" magit-status)
    ("v" magit-file-dispatch)
    ("V" magit-dispatch)))

;; leader keymap

(defvar leader
  "SPC")

(defvar leader-map
  (make-sparse-keymap))

(defun local-leader ()
  nil)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(define-keys leader-map
  `(("w" ,windows-map)
    ("f" ,file-map)
    ("b" ,buffer-map)
    ("c" calendar)
    ("h" ,help-map)
    ("t" ,text-map)
    ("T" eshell)
    ("p" ,program-map)
    ("v" ,version-map)
    ("m" local-leader)
    ("i" imenu)
    ("P" list-processes)
    ("r" async-shell-command)
    ("TAB" switch-to-last-buffer)
    ("q" save-buffers-kill-terminal)))

(define-key leader-map (kbd "SPC") (kbd "C-c C-c"))

(define-key keys-minor-mode-map (kbd leader) leader-map)

;; aborting keybinding

(define-key keys-minor-mode-map (kbd "<escape>") 'keyboard-quit)

;; we start with keys minor mode enabled

(keys-minor-mode 1)
