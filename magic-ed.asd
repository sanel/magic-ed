(defsystem :magic-ed
  :serial t
  :name "magic-ed"
  :version "0.2"
  :description "Edit your code from REPL."
  :depends-on (#+sbcl :sb-introspect)
  :components
  ((:file "src/magic-ed")))
