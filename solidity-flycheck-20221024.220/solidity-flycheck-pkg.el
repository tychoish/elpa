;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "solidity-flycheck" "20221024.220"
  "Flycheck integration for solidity emacs mode."
  '((flycheck      "32-cvs")
    (solidity-mode "0.1.9")
    (dash          "2.17.0"))
  :url "https://github.com/ethereum/emacs-solidity"
  :commit "8cb8ac6d1311f5bc893cd72ee96e3e335ee8b2a1"
  :revdesc "8cb8ac6d1311"
  :keywords '("languages" "solidity" "flycheck")
  :authors '(("Lefteris Karapetsas" . "lefteris@refu.co"))
  :maintainers '(("Lefteris Karapetsas" . "lefteris@refu.co")))
