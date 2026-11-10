;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20261101.1112"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "31.1")
    (cond-let "1.1")
    (emacsql  "4.4")
    (llama    "1.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "d87a2718c2fe45ff98258a2c7f3821b3f7b24f29"
  :revdesc "d87a2718c2fe"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
