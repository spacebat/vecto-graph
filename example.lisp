(defun test-bar-graph (&optional (file "/tmp/bar-graph.png"))
  (vecto-graphs:bar-graph file
                          '(("African" 40)
                            ("European" 36))
                          "Airspeed velocity of an unladen swallow"
                          "km/h"))

(defun test-line-chart (&optional (file "/tmp/line-chart.png"))
  (vecto-graphs:line-chart file
                          '(("Debt"  (2006 8.4) (2007 8.9) (2008 10) (2009 11.8) (2010 12.3))
                            ("GDP" (2006 13.3) (2007  13.9) (2008 14.3) (2009 14) (2010 14.5)))
                          :x-label "US debt vs GDP"
                          :y-label "Trillion $"))
