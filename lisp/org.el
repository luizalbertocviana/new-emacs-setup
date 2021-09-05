(use-package org
  :hook
  (org-mode . (lambda () (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)))
  (org-agenda-mode . (lambda ()
                       (local-set-key [remap kill-whole-line]    'org-agenda-kill)              
                       (local-set-key [remap undo]               'org-agenda-undo)              
                       (local-set-key [remap next-line]          'org-agenda-next-item)         
                       (local-set-key [remap previous-line]      'org-agenda-previous-item)     
                       (local-set-key [remap backward-char]      'org-agenda-exit)              
                       (local-set-key [remap forward-char]       'org-agenda-show-and-scroll-up)
                       (local-set-key [remap beginning-of-defun] 'org-agenda-earlier)           
                       (local-set-key [remap end-of-defun]       'org-agenda-later)))
  (org-agenda-mode . (lambda ()
                       (local-set-key (kbd "c") 'org-agenda-date-prompt)
                       (local-set-key (kbd "t") 'org-agenda-todo)       
                       (local-set-key (kbd "m") 'org-agenda-bulk-toggle)
                       (local-set-key (kbd "M") 'org-agenda-bulk-action)))
  :custom
  (org-agenda-new-buffers nil)
  (org-confirm-babel-evaluate nil)
  (org-log-done t)
  (org-default-notes-file "~/Dropbox/org/notes.org")
  (org-agenda-files (list "~/Dropbox/org/notes.org"))
  (org-agenda-span 'month)
  (org-agenda-tags-column 80)
  (org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                           "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (org-babel-lisp-eval-fn 'sly-eval)
  (org-list-allow-alphabetical t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
    
    (push 'beamer org-export-backends)
    (setq org-capture-templates
          '(("c" "Capture" entry (file+datetree  "~/Dropbox/org/notes.org")
              "* TODO %? %^g\n  SCHEDULED: %^{Scheduled}t DEADLINE: %^{Deadline}t"))))

(setq org-agenda-restore-windows-after-quit t)

(defvar org-map
  (make-sparse-keymap))

(define-keys org-map
  '(("l" org-store-link)
    ("a" org-agenda-list)
    ("c" org-capture)))

(define-key leader-map (kbd "o") org-map)

(defhydra org-heading-hydra (:columns 4 :exit t)
  "org headings"
  ("h" org-promote-subtree   "promote subtree")
  ("l" org-demote-subtree    "demote subtree")
  ("k" org-move-subtree-up   "move subtree up")
  ("j" org-move-subtree-down "move subtree down")
  ("d" org-cut-subtree       "cut subtree")
  ("y" org-copy-subtree      "copy subtree")
  ("p" org-paste-subtree     "paste subtree")
)

(defhydra org-link-hydra (:columns 4 :exit t)
  "org links"
  ("o" org-open-at-point "open")
  ("i" org-insert-link   "insert")
  )

(defhydra org-spreadsheet-column-hydra (:columns 4 :exit t)
  "org spreadsheet column"
  ("h" org-table-move-column-left   "move left")
  ("l" org-table-move-column-right  "move right")
  ("d" org-table-move-delete-column "delete")
  ("i" org-table-move-insert-column "insert")
  )

(defhydra org-spreadsheet-row-hydra (:columns 4 :exit t)
  "org spreadsheet row"
  ("k" org-table-move-row-up     "move up")
  ("j" org-table-move-row-down   "move down")
  ("d" org-table-move-kill-row   "delete")
  ("i" org-table-move-insert-row "insert")
  ("h" org-table-hline-and-move  "insert hline")
  )

(defhydra org-spreadsheet-hydra (:columns 4 :exit t)
  "org spreadsheet"
  ("d" org-table-blank-field                "blank field")
  ("s" org-table-sort-lines                 "sort lines")
  ("u" org-table-iterate-buffer-tables      "update buffer tables")
  ("v" org-table-toggle-coordinate-overlays "toggle row/column labels")
  ("h" org-table-move-cell-left             "move cell left")
  ("j" org-table-move-cell-down             "move cell down")
  ("k" org-table-move-cell-up               "move cell up")
  ("l" org-table-move-cell-right            "move cell right")
  ("c" org-spreadsheet-column-hydra/body    "columns")
  ("r" org-spreadsheet-row-hydra/body       "rows")
  )

(defhydra org-todo-hydra (:columns 4 :exit t)
  "org todo"
  ("c" org-todo                                "change")
  ("d" org-deadline                            "deadline")
  ("a" org-toggle-archive-tag                  "toggle archive")
  ("o" org-insert-todo-heading-respect-content "insert")
  ("r" org-clone-subtree-with-time-shift       "clone with time shift")
  ("s" org-schedule                            "schedule")
  ("t" org-show-todo-tree                      "show todo tree")
  )

(defhydra org-hydra (:columns 4 :exit t)
  "org"
  ("D" org-time-stamp               "active timestamp")
  ("d" org-time-stamp-inactive      "inactive timestamp")
  ("S" org-sort                     "sort")
  ("c" org-edit-special             "change")
  ("e" org-export-dispatch          "export")
  ("h" org-heading-hydra/body       "headings")
  ("l" org-link-hydra/body          "links")
  ("r" org-babel-execute-src-block  "run source block")
  ("s" org-spreadsheet-hydra/body   "spreadsheets")
  ("t" org-todo-hydra/body          "todo")
)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key [remap local-leader] 'org-hydra/body)))

(define-key org-read-date-minibuffer-local-map (kbd "h") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "l") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
(define-key org-read-date-minibuffer-local-map (kbd "j") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "k") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
(define-key org-read-date-minibuffer-local-map (kbd "H") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "L") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month 1))))
(define-key org-read-date-minibuffer-local-map (kbd "J") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1))))
(define-key org-read-date-minibuffer-local-map (kbd "K") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
















