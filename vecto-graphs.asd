(defsystem vecto-graphs
  :depends-on (vecto)
  :serial t
  :components
  ((:file "package")
   (:file "share")
   (:file "drawing")
   (:file "axes")
   (:file "bar-graph")
   (:file "pie-chart")
   (:file "line-chart")))
