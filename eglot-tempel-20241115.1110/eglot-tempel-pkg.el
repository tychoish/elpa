;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "eglot-tempel" "20241115.1110"
  "Use tempel to expand snippets from eglot."
  '((eglot  "1.9")
    (tempel "0.5")
    (emacs  "29.1")
    (peg    "1.0.1"))
  :url "https://github.com/fejfighter/eglot-tempel"
  :commit "c6c9a18eba61f6bae7167fa62bab9b637592d20d"
  :revdesc "c6c9a18eba61"
  :keywords '("convenience" "languages" "tools")
  :authors '(("Jeff Walsh" . "fejfighter@gmail.com"))
  :maintainers '(("Jeff Walsh" . "fejfighter@gmail.com")))
