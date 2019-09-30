(defsystem progressor
  :class :package-inferred-system
  :version "0.1.0"
  :description "A client/server to store long running jobs progress state."
  :homepage "https://github.com/40ants/progressor"
  :pathname "src"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "progressor"
  :entry-point "progressor/cli/main:main"
  :depends-on ("progressor/server"
               "progressor/cli/main"))
