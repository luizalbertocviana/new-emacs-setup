(use-package cider)

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode t nil)

;; adds lsp support for clojure
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)

(defun cider-lsp-integration ()
  (setq-local lsp-enable-indentation nil)
  (setq-local lsp-enable-completion-at-point nil))

;; cider integration with lsp
(add-hook 'cider-mode-hook 'cider-lsp-integration)

(defhydra clojure-mode-run-hydra (:columns 4 :exit t)
  "clojure run"
  ("c" cider-jack-in "clojure")
  ("j" cider-jack-in-cljs "clojurescript")
  ("b" cider-jack-in-clj&cljs "both")
)

(defhydra clojure-mode-eval-hydra (:collumns 4 :exit t)
  "clojure eval"
  ("d" cider-debug-defun-at-point "debug")
  ("e" cider-eval-defun-at-point "defun")
  ("i" cider-interrupt "interrupt")
  ("p" cider-pprint-eval-defun-at-point "pretty print")
)

(defhydra clojure-mode-doc-hydra (:collumns 4 :exit t)
  "clojure doc"
  ("c" cider-clojuredocs "clojuredocs")
  ("d" cider-doc "clojure")
  ("j" cider-javadoc "javadoc")
  ("s" cider-apropos-documentation "search")
)

(defhydra clojure-mode-test-hydra (:collumns 4 :exit t)
  "clojure test"
  ("f" cider-test-rerun-failed-tests "run failed tests")
  ("l" cider-test-rerun-test "run last test")
  ("n" cider-test-run-ns-tests "run namespace tests")
  ("p" cider-test-run-project-tests "run project tests")
  ("s" cider-test-show-report "show report")
  ("t" cider-test-run-test "run test")
)

(defhydra clojure-mode-find-hydra (:columns 4 :exit t)
  "clojure find"
  ("d" cider-xref-fn-deps "dependencies")
  ("f" cider-find-var "definition")
  ("n" cider-find-ns "namespace")
  ("r" cider-xref-fn-refs "references")
)

(defhydra clojure-mode-browse-hydra (:columns 4 :exit t)
  "clojure browse"
  ("n" cider-browse-ns-all "namespaces")
  ("s" cider-browse-spec-all "specs")
)

(defhydra clojure-mode-hydra (:columns 4 :exit t)
  "clojure"
  ("B" clojure-mode-browse-hydra/body "browse")
  ("T" cider-toggle-trace-var "toggle tracing")
  ("b" cider-load-buffer "load buffer")
  ("d" clojure-mode-doc-hydra/body "documentation")
  ("e" clojure-mode-eval-hydra/body "eval")
  ("f" clojure-mode-find-hydra/body "find")
  ("i" cider-inspect "inspect")
  ("l" cider-load-file "load file")
  ("m" cider-macroexpand-1 "macroexpand")
  ("n" cider-repl-set-ns "change repl namespace")
  ("o" cider-scratch "open scratch")
  ("r" clojure-mode-run-hydra/body "run")
  ("s" cider-apropos "search")
  ("t" clojure-mode-test-hydra/body "test")
  ("u" cider-undef "undef")
  ("v" cider-enlighten-mode "view locals")
)

(define-key clojure-mode-map [remap local-leader] 'clojure-mode-hydra/body)

(defhydra cider-repl-mode-hydra (:columns 4 :exit t)
  "cider repl"
  ("n" cider-repl-set-ns "namespace"))

(define-key cider-repl-mode-map [remap local-leader] 'cider-repl-mode-hydra/body)

(define-key cider-stacktrace-mode-map [remap end-of-defun] 'cider-stacktrace-next-cause)
(define-key cider-stacktrace-mode-map [remap beginning-of-defun] 'cider-stacktrace-previous-cause)
(define-key cider-stacktrace-mode-map [remap newline] 'cider-stacktrace-jump)
(define-key cider-stacktrace-mode-map [remap indent-for-tab-command] 'cider-stacktrace-cycle-cause)
