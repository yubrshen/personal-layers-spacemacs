;;; -*- lexical-binding: t -*-

;; Required for plantuml-mode to work
;; (setq org-plantuml-jar-path "~/bin")

(setq config-packages
  '(;; Unowned Packages
     ;; aggressive-indent
     ;; avy
     ;; eshell
     ;; evil
     ;; ivy
     ;; magit
     ;; ob
     org
     ;; org-bullets
     ;; ranger

     ;; Owned Packages
     ;; auto-dim-other-buffers
     ;; dash-functional
     ;; faceup
     ;; outshine  ; also configures `outline-mode'
     ;; s

     ;; ;; Local Packages
     ;; (redo-spacemacs :location local)
     )
  )

;;; unowned
;;;; org
;; The following is overshadowed or overide by the same function in the layer config of Eric
;; I don't know why.
(defun yubrshen/pre-init-org ()
  (setq org-structure-template-alist
    (append org-structure-template-alist
      '(
         ;; ("a" . "export ascii")
         ;; ("c" . "center")
         ;; ("C" . "comment")
         ;; ("e" . "example")
         ;; ("E" . "export")
         ;; ("h" . "export html")
         ;; ("l" . "export latex")
         ;; ("q" . "quote")
         ;; ("s" . "src ? :tangle :noweb no-export")
         ("uml" . "src plantuml :file ?.png\n@startuml\n\n\@enduml")
         ;; ("v" . "verse")
         ("z" . "src python :tangle ~/Dropbox/only-focus-besides-earning/odin-money/pipeline.py :noweb no-export") ; my current often used block header
         ))))

(setq-default abbrev-mode t)
