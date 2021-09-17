(add-hook 'c++-mode-hook 'lsp t nil)

(defhydra c++-mode-hydra (:columns 4 :exit t)
  "c++"
  ("b" compile "build")
  ("d" gdb "debug")
  ("e" next-error "next error")
  ("r" recompile "rebuild"))

(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map [remap local-leader] 'c++-mode-hydra/body)))

(defhydra gud-mode-hydra (:columns 4 :exit t)
  "gud"
  ("b" gdb-frame-breakpoints-mark   "set breakpoint")
  ("d" gdb-frame-disassembly-buffer "disassembly")
  ("i" gdb-frame-io-buffer "io buffer")
  ("l" gdb-frame-locals-buffer "locals")
  ("m" gdb-frame-memory-buffer "memory")
  ("s" gdb-frame-stack-buffer "stack"))

(add-hook 'gud-mode-hook
          (lambda ()
            (define-key gud-mode-map [remap local-leader] 'gud-mode-hydra/body)))
