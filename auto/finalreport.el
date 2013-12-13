(TeX-add-style-hook "finalreport"
 (lambda ()
    (TeX-add-symbols
     '("SUMJ" 3)
     '("SUMI" 3)
     '("SUMX" 3)
     '("SUMK" 3)
     '("SUMN" 3))
    (TeX-run-style-hooks
     "graphicx"
     "amssymb"
     "amsmath"
     "enumitem"
     "fullpage"
     ""
     "latex2e"
     "art12"
     "article"
     "12pt")))

