;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "company-solidity" "20200113.1721"
  "Company-mode back-end for solidity-mode."
  '((company       "0.9.0")
    (cl-lib        "0.5.0")
    (solidity-mode "0.1.9"))
  :url "https://github.com/ethereum/emacs-solidity"
  :commit "93412f211fad7dfc3b02aa226856fc52b6a15c22"
  :revdesc "93412f211fad"
  :keywords '("solidity" "completion" "company")
  :authors '(("Samuel Smolkin" . "sam@future-precedent.org"))
  :maintainers '(("Samuel Smolkin" . "sam@future-precedent.org")))
