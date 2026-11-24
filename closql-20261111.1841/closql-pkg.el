;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20261111.1841"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "31.1")
    (cond-let "1.1")
    (emacsql  "4.4")
    (llama    "1.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "5e34a2300e149cefcba65cd683ca4294b57cdae6"
  :revdesc "5e34a2300e14"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
