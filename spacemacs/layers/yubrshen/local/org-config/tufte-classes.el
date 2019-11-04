(add-to-list 'org-latex-classes
             '("tufte-handout"
               "\\documentclass[twoside,nobib]{tufte-handout}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

(add-to-list 'org-latex-classes
             '("tufte-book"
               "\\documentclass[twoside,nobib]{tufte-book}
                                  [NO-DEFAULT-PACKAGES]"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")))
