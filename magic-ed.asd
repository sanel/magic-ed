(defsystem magic-ed
  :serial t
  :name "magic-ed"
  :version "0.2"
  :author "Sanel Zukan. https://github.com/sanel/magic-ed/"
  :licence "MIT"
  :description "Edit your code from REPL."
  :depends-on (#+sbcl :sb-introspect)
  :components ((:file "src/magic-ed")))
