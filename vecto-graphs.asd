(defsystem vecto-graphs
  :depends-on (vecto)
  :serial t
  :components
  ((:file "package")
   (:file "share")
   (:file "bar-graph")
   (:file "pie-chart")))
