(defsystem #:package-doc-dump
  :description "Simple API doc generator - dumps docstrings, names, lambda lists of all external symbols in specified packages to an HTML file. In the order the symbols are specified in the defpackage forms."
  :license "MIT"
  :author "Anton Vodonosov"
  :depends-on ("docparser" "3bmd" "3bmd-ext-code-blocks")
  :serial t
  :components ((:file "package-doc-dump")))
