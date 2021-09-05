(use-package helm)

(use-package helm-ls-git)

(use-package helm-lsp)

(define-key file-map (kbd "o") 'helm-multi-files)

(define-key (current-global-map) [remap imenu]                    'helm-imenu)
(define-key (current-global-map) [remap apropos]                  'helm-apropos)
(define-key (current-global-map) [remap woman]                    'helm-man-woman)
(define-key (current-global-map) [remap list-processes]           'helm-list-emacs-process)
(define-key (current-global-map) [remap recentf-open-files]       'helm-recentf)
(define-key (current-global-map) [remap async-shell-command]      'helm-run-external-command)
(define-key (current-global-map) [remap switch-to-buffer]         'helm-buffers-list)
(define-key (current-global-map) [remap find-file]                'helm-find-files)
(define-key (current-global-map) [remap occur]                    'helm-occur)
(define-key (current-global-map) [remap execute-extended-command] 'helm-M-x)
