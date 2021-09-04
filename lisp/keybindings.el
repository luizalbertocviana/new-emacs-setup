;; key chords
(use-package key-chord
  :custom
    (key-chord-two-keys-delay 0.3)
    (key-chord-one-key-delay 0.3)
  :config
    (key-chord-mode 1)
    ;; chords to avoid reaching for shift key
    (key-chord-define global-map "qq" "/")
    (key-chord-define global-map "ww" "?")
    (key-chord-define global-map "''" "\"")
    (key-chord-define global-map "11" "!")
    (key-chord-define global-map "22" "@")
    (key-chord-define global-map "33" "#")
    (key-chord-define global-map "44" "$")
    (key-chord-define global-map "55" "%")
    (key-chord-define global-map "77" "&")
    (key-chord-define global-map "88" "*")
    (key-chord-define global-map "99" "(")
    (key-chord-define global-map "00" ")")
    (key-chord-define global-map "--" "_")
    (key-chord-define global-map "==" "+")
    (key-chord-define global-map ",," "<")
    (key-chord-define global-map ".." ">")
    (key-chord-define global-map ";;" ":")
    (key-chord-define global-map "\\\\" "|")
    (key-chord-define global-map "[[" "{"))
