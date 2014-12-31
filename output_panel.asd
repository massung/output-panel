(defpackage :output-panel-asd
  (:use :cl :asdf))

(in-package :output-panel-asd)

(defsystem :output_panel
  :name "output_panel"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CAPI Output Panel Pane for LispWorks."
  :serial t
  :components ((:file "output_panel")))
