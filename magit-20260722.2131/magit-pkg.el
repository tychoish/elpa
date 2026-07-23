;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20260722.2131"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "31.0")
    (cond-let      "1.1")
    (llama         "1.0")
    (magit-section "4.6")
    (seq           "2.24")
    (transient     "0.13")
    (with-editor   "3.5"))
  :url "https://github.com/magit/magit"
  :commit "639ea6c61598f196c363c9299d21a7301b7d9fa0"
  :revdesc "639ea6c61598"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
