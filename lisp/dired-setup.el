(use-package dired-aux :straight nil)

;; dired file manager
(use-package dired-x :straight nil
  :hook
    (dired-mode . dired-omit-mode)
  :custom
    ;; do what i mean, makes operations intuitive when there are two
    ;; open dired buffers
    (dired-dwim-target t)  
    ;; dired does not change last modified timestamp when copying
    ;; files
    (dired-copy-preserve-time t)
    ;; human readable sizes
    (dired-listing-switches "-alh")
  :config
    ;; this command is disabled by default, but I like this better
    ;; than 'dired-find-file
    (put 'dired-find-alternate-file 'disabled nil)
    ;; this prevents dot files from being listed (this cannot be put
    ;; into a :custom section (dont know why))
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    ;; this makes dired list directories first
    (use-package ls-lisp :straight nil
      :custom
        (ls-lisp-dirs-first t)
        (ls-lisp-use-insert-directory-program nil)))

;; colorful dired
(use-package diredfl
  :config
    (diredfl-global-mode))

;; facilities to use subtrees in dired
(use-package dired-subtree)

(define-key dired-mode-map [remap end-of-defun]       'dired-next-marked-file)
(define-key dired-mode-map [remap beginning-of-defun] 'dired-prev-marked-file)
(define-key dired-mode-map [remap undo]               'dired-unmark)
(define-key dired-mode-map [remap open-line]          'dired-subtree-toggle)
(define-key dired-mode-map [remap backward-char]      (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map [remap forward-char]       'dired-find-alternate-file)
(define-key dired-mode-map [remap set-mark-command]   'dired-mark)

(define-key dired-mode-map [remap local-leader] 'dired-hydra/body)

(defhydra dired-search-hydra (:columns 4 :exit t)
  "dired search"
  ("g" find-grep-dired               "grep")
  ("r" dired-do-query-replace-regexp "query replace regexp")
  ("s" dired-do-isearch-regexp       "search regexp")
)

(defhydra dired-hydra (:columns 4 :exit t)
  "dired"
  ("D" dired-diff                                   "diff")
  ("S" dired-do-symlink                             "symlink")
  ("T" dired-toggle-marks                           "toggle marks")
  ("U" dired-upcase                                 "upcase")
  ("Z" dired-do-compress                            "compress")
  ("c" wdired-change-to-wdired-mode                 "edit directory")
  ("d" dired-do-delete                              "delete")
  ("g" dired-do-chgrp                               "change group")
  ("h" dired-omit-mode                              "toggle hide")
  ("i" (lambda () (interactive) (image-dired "./")) "image dired")
  ("l" dired-downcase                               "downcase")
  ("m" dired-do-chmod                               "change mode")
  ("n" dired-create-directory                       "new directory")
  ("o" dired-do-chown                               "change owner")
  ("r" dired-do-rename                              "rename")
  ("s" dired-search-hydra/body                      "search")
  ("t" dired-do-async-shell-command                 "run command")
  ("y" dired-do-copy                                "copy")
  ("z" dired-do-compress-to                         "compress to")
)
