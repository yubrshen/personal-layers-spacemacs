;; The following if commented out would not make plantuml not working:
;; It seems that org must be started when plantuml is started.

(require 'org)
;; (require 'org-contacts) ; to avoid the problem of (void-function org-projectile:per-repo)
(require 'org-bullets)
(require 'ox-bibtex)
(require 'ox-extra)
(require 'ox-latex)
(require 'bibtex)

;;;; Setup reveal

;;(load "~/programming/write-slides-with-emacs-org-reveal/org-reveal/ox-reveal.el")
;;(load "~/elisp/org-reveal/ox-reveal.el")
(load "../../../../../org-reveal/ox-reveal.el")

;;;; Setup for exportting to freemind (ox-freemind)
;; After using org-freemind-export-to-freemind a few times by calling the funciton, then option shows up in the org-export-dispatch menu
;; No more configuration than the following is needed.
(setq org-freemind-section-format 'node)

;;; Bindings

(spacemacs/set-leader-keys "aof" 'org-open-at-point-global)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "r" 'org-refile)

(defun org-sort-entries-priorities () (interactive) (org-sort-entries nil ?p))
(spacemacs/set-leader-keys-for-major-mode
  'org-mode "s p" 'org-sort-entries-priorities)

(evil-define-key '(normal visual motion) org-mode-map
  "gh" 'outline-up-heading
  "gj" 'outline-forward-same-level
  "gk" 'outline-backward-same-level
  "gl" 'outline-next-visible-heading
  "gu" 'outline-previous-visible-heading)

;;; Hooks

(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'flyspell-mode)

;;; Theming

(setq org-priority-faces '((65 :inherit org-priority :foreground "red")
                           (66 :inherit org-priority :foreground "brown")
                           (67 :inherit org-priority :foreground "blue")))
(setq org-ellipsis "")
(setq org-bullets-bullet-list '("" "" "" ""))

;;; Templates

;; New and simpler org-structure-template-alist after org-mode 9.2:
(setq org-structure-template-alist
  '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src ? :tangle :noweb no-export")
     ("uml" . "src plantuml :file ?.png\n@startuml\n\n\@enduml")
     ("v" . "verse")))
;; For more sophisticated code boilers, use yas-snippet
;; I'm defining the same keys as those for easy-template
;; Note, the command to start the new org-structure-template in spacemacs is , ib
;; By (require 'org-tempo) would enable <s TAB substitution, but its substitution is partial not as as complete as , ib

;; The following of "easy-template" no longerr works for org-mode 9.2
;; (setq org-structure-template-alist
;;       '(;; Standard Blocks

;;         ("n" "#+NAME: ?")
;;         ("q" "#+BEGIN_QUOTE\n\n#+END_QUOTE")

;;         ;; Language Blocks
;;         ("src" "#+BEGIN_SRC ?\n\n#+END_SRC")
;;         ("c"  "#+BEGIN_SRC clojure\n\n#+END_SRC")
;;         ("d"  "#+BEGIN_SRC dot\n\n#+END_SRC")
;;         ("e"  "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC")
;;         ("h"  "#+BEGIN_SRC haskell\n\n#+END_SRC")
;;         ("la" "#+BEGIN_SRC latex\n\n#+END_SRC")
;;         ("l"  "#+BEGIN_SRC lisp\n\n#+END_SRC")
;;         ("p"  "#+BEGIN_SRC python\n\n#+END_SRC")

;;         ;; html-export org-html-themese collapse properties slug
;;         ("clps" ":PROPERTIES:\n :HTML_CONTAINER_CLASS: hsCollapsed\n :END:\n")

;;         ;; Hugo title slug template
;;         ("b" "#+TITLE: \n#+SLUG: \n#+DATE: 2018-mm-dd
;; #+CATEGORIES: \n#+SUMMARY: \n#+DRAFT: false")
;;         ;; Yu Shen's own definitions:
;;         ("E" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
;;         ("S" "#+BEGIN_SRC sh\n?\n#+END_SRC")
;;         ("uml" "#+BEGIN_SRC plantuml :file uml.png\n@startuml\n?\n@enduml\n#+END_SRC\n#results:")
;;         ("ditaa" "#+NAME:?\n#+BEGIN_SRC ditaa \n\n#+END_SRC\n"
;;          "\n<src lang=\"ditaa\">\n?\n</src>")
;;         ("sql" "#+NAME:?\n#+BEGIN_SRC sql :noweb no-export :tangle \n\n#+END_SRC\n"
;;          "<src lang=\"sql\">\n?\n</src>")
;;         ;;  :engine mssql :cmdline \"-S localhost -U SA -P <my password>\" \n\n#+END_SRC\n"
;;         ;; is the setting to execute SQL statements with Microsoft SQL server with my local set up
;;         ;; The setting is best set as global properties with org-file

;;         ("ps" "#+BEGIN_SRC python \n?\n#+END_SRC\n" "<src lang=\"python\">\n?\n</src>")
;;         ("p" "#+NAME:?\n#+BEGIN_SRC python :noweb no-export :tangle  \n\n#+END_SRC\n"
;;          "<src lang=\"python\">\n?\n</src>")
;;         ("pe" "#+END_SRC\n\n?\n#+BEGIN_SRC python \n" "</src>\n<src lang=\"python\">")
;;         ("cc" "#+NAME:?\n#+BEGIN_SRC C++ :noweb no-export :tangle :main no \n\n#+END_SRC\n"
;;          "<src lang=\"C++\">\n?\n</src>")
;;         ("clj" "#+NAME:?\n#+BEGIN_SRC clojure \n\n#+END_SRC\n"
;;          "\n<src lang=\"clojure\">\n?\n</src>")
;;         ("cs" "#+END_SRC\n\n\n#+NAME: ?\n#+BEGIN_SRC clojure \n"
;;          "</src>\n<src lang=\"clojure\">")
;;         ("r" "#+NAME:?\n#+BEGIN_SRC R \n\n#+END_SRC\n"
;;          "\n<src lang=\"R\">\n?\n</src>")
;;         ("rs" "#+END_SRC\n\n\n#+NAME: ?\n#+BEGIN_SRC R \n" "</src>\n<src lang=\"R\">")
;;         ("j" "#+NAME:?\n#+BEGIN_SRC javascript \n\n#+END_SRC\n"
;;          "\n<src lang=\"javascript\">\n?\n</src>")
;;         ("js" "#+END_SRC\n\n\n#+NAME: ?\n#+BEGIN_SRC javascript \n"
;;          "</src>\n<src lang=\"javascript\">")
;;         ("elsp" "#+NAME:?\n#+BEGIN_SRC emacs-lisp \n\n#+END_SRC\n"
;;          "\n<src lang=\"emacs-lisp\">\n?\n</src>")
;;         ("elsps" "#+END_SRC\n\n\n#+NAME: ?\n#+BEGIN_SRC emacs-lisp \n"
;;          "</src>\n<src lang=\"emacs-lisp\">")
;;         ("shell" "#+NAME:?\n#+BEGIN_SRC shell \n\n#+END_SRC\n"
;;          "\n<src lang=\"shell\">\n?\n</src>")
;;         ("l" "#+NAME:?\n#+BEGIN_SRC latex \n\n#+END_SRC\n"
;;          "\n<src lang=\"latex\">\n?\n</src>"))
;;       )

;;; Org Blocks

;; Hide all org-blocks, including src, quote, etc. blocks, on buffer load
(defvar org-blocks-hidden nil)
(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))

;;; Export
(ox-extras-activate '(ignore-headlines))
;;;; Using xelatex, not used
;; Use xelatex instead of pdflatex to support Chinese text in unicode
;; (setq
;;  org-latex-pdf-process
;;  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;;; Use xelatex or pdflatex based on buffer string #+LATEX_CMD: xelatex

;; The following is based on
;; based on https://orgmode.org/worg/org-faq.html#using-xelatex-for-pdf-export
;; (require 'ox-latex)
;; (setq org-latex-listings t) ; using listing package, becoming obsoleted as 8/8/2019

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
;; combinde with the suggestion by
;; https://stackoverflow.com/questions/47623041/getting-emacs-spacemacs-to-accept-function-that-changes-latex-compiler-based-u
;; fixing the the content of texcmd for xelatex

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-latex-packages-alist
      '(("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)))

(defun my-auto-tex-cmd (file)
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd))
    (if (string-match "LATEX_CMD: xelatex" (buffer-string))
        (progn                            ; else
          ;; xelatex -> .pdf
          (setq texcmd "latexmk -shell-escape -f -pdflatex=xelatex -8bit -pdf %f") ; removing -quiet
          ;; Packages to include when xelatex is used
          (setq org-latex-default-packages-alist
                (append org-latex-default-packages-alist
                        '(("" "fontspec" t)
                          ("" "xunicode" t)
                          ("" "url" t)
                          ("" "rotating" t)
                          ("american" "babel" t)
                          ("babel" "csquotes" t)
                          ("" "soul" t)
                          ("xetex" "hyperref" nil)
                          )))
          (setq org-latex-classes
                (append org-latex-classes
                        (cons '("article"
                                "\\documentclass[11pt,article,oneside]{memoir}"
                                ("\\section{%s}" . "\\section*{%s}")
                                ("\\subsection{%s}" . "\\subsection*{%s}")
                                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                              org-latex-classes)))
          )
      (progn                            ; else
        ;; pdflatex -> .pdf
        ;; (setq texcmd "latexmk -pdf -quiet %f")
        (setq texcmd "latexmk -shell-escape -f -pdf %f") ; remove quiet, add -f
        ;; default packages for ordinary latex or pdflatex export
        (setq org-latex-default-packages-alist
              (append org-latex-default-packages-alist
                      '(("AUTO" "inputenc" t)
                        ("T1"   "fontenc"   t)
                        (""     "fixltx2e"  nil)
                        ("normalem" "ulem" t)
                        (""     "wrapfig"   nil)
                        (""     "soul"      t)
                        (""     "textcomp"  t)
                        (""     "marvosym"  t)
                        (""     "wasysym"   t)
                        (""     "latexsym"  t)
                        (""     "amssymb"   t)
                        (""     "hyperref"  nil))))))
    ;; LaTeX compilation command
    (setq org-latex-pdf-process (list texcmd))))
;; (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)
;; org-export-latex-after-initial-vars-hook does not exit.
(add-hook 'org-export-before-parsing-hook 'my-auto-tex-cmd) ; must add hook to org-export-before-parsing-hook

;;;; Exporting Chinese with xelatex, not being used

;; The following solution also work after fixing the variable org-latex-pdf-process
;; remove -to- in it.
;; But it's less generic.
;; (setq
;;   org-latex-pdf-process
;;   '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;      "xelatex -shell-escape  -interaction nonstopmode -output-directory %o %f"
;;      "xelatex -shell-escape  -interaction nonstopmode -output-directory %o %f"))
;; To export Chinese text, the org file must have the following options set:
;; #+LATEX_HEADER: \usepackage{xltxtra}
;; #+LATEX_HEADER: \setmainfont{WenQuanYi Micro Hei}
;; The above setting does not work, as of Aug. 8, 2019.

;; There are a lot more configuration to learn from
;; https://github.com/kaushalmodi/.emacs.d/blob/42831e8997f7a3c90bf4bd37ae9f03c48277781d/setup-files/setup-org.el#L413-L584

;;;; Use listing to export source code

;; The following will conflict with the setting to use xelatex!
;; With pdflatex
;; (setq org-latex-pdf-process '("latexmk -shell-escape -f -pdf %f")) ; remove -quiet, add -f (force) to be able to debug.
;; (setq org-latex-listings 'minted)
;; using minted, minted must be used instead of the package of listings in order to support Dart source code.
(add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

;; (setq org-latex-minted-options '(("frame" "lines")
;;                                  ("fontsize" "\\scriptsize")
;;                                  ("xleftmargin" "\\parindent")
;;                                  ("linenos" "")))
;; having trouble with minted packages

;;;; Customize the margine
;; Globablly change the marge for org export to PDF
(add-to-list 'org-latex-packages-alist '("margin=1cm" "geometry"))

;;;; Setup for exporting to Freemind

(require 'ox-freemind)
;; (require 'org-freeplane) ; not working
;;; Babel

(setq org-confirm-babel-evaluate   nil)
(setq org-src-fontify-natively     t)
(setq org-src-tab-acts-natively    t)
(setq org-src-preserve-indentation t)
(setq org-src-window-setup         'current-window)
(setq org-babel-default-header-args:python
      (cons '(:results . "output file replace")
            (assq-delete-all :results org-babel-default-header-args)))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((latex .   t)
                               (python .  t)
                               (haskell . t)
                               (clojure . t)
                               (dot .     t)
                               (emacs-lisp . t)
                               (C . t)
                               (ditaa . t)
                               (js . t)
                               (latex . t)
                               (shell . t) ; sh does not work, shell works
                               (plantuml . t)
                               (sql . t)
                               )
                             )

;;; Files

(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . "/usr/bin/firefox %s")
                      ("\\.pdf\\'" . default)))

;;; Yu Shen's babel related customization

;; (load "~/programming/emacs-lisp/literate-tools.el")
;;(load "~/elisp/spacemacs/layers/yubrshen/local/literate-tools/literate-tools.el")
(load "../literate-tools/literate-tools.el")
(setq Org-Reveal-root "~/yshen/Dropbox/reveal.js")
;; (setq Org-Reveal-root "file:///home/yubrshen/programming/write-slides-with-emacs-org-reveal/reveal.js")
(setq Org-Reveal-title-slide nil)

;;; My keybinding

;; The following might be in keybinding section.

(evil-escape-mode)
(setq-default evil-escape-key-sequence ",,")
(setq-default evil-escape-delay 0.3)
;; the suggested is 0.2, but to Yu Shen it's still not enough delay,
;; 0.3 seems better.
;; The delay between the two key presses can be customized with the variable
;; evil-escape-delay. The default value is 0.1.
;; If your key sequence is composed with the two same characters it is
;; recommended to set the delay to 0.2.

;;; Org-capture and Org-agenda customization

(setq org-directory "~/zoom-out")
; set proper value of org-capture file; have a centralized notes.org
(setq org-default-notes-file (concat org-directory "/" "notes.org"))

(setq org-plantuml-jar-path "~/bin/plantuml.jar")

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "HOLD(h)" "|" "PNEDING(p)" "|"  "CANCELED(c)")))
;; (setq org-tag-alist '((:startgroup . nil)
;;                       ("heavy" . ?h) ("light" . ?l)
;;                       (:endgroup . nil)
;;                       ))

(setq org-tag-alist '((:startgroup)
                      ("heavy" . ?h) ("light" . ?l)
                      (:endgroup)
                      ))
;; (setq org-tag-alist (quote ((:startgroup)
;;                             ("work" . ?w)
;;                             ("home" . ?h)
;;                             (:endgroup)
;;                             ("oss" . ?o)
;;                             ("xpack" . ?x)
;;                             ("book" . ?b)
;;                             ("support" . ?s)
;;                             ("docs" . ?d)
;;                             ("emacs" . ?e)
;;                             ("tech" . ?t)
;;                             ("noexport" . ?n)
;;                             ("recurring" . ?r)
;;                             ("WAITING" . ?W) ("HOLD" . ?H)
;;                             ("NOTE" . ?n) ("CANCELLED" . ?c))))
;;; Provide
(provide 'org-config)
