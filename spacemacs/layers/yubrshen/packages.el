;;; yubrshen Layer -*- lexical-binding: t; -*-

;; The personalization layer for Yu Shen (yubrshen)
(message "Entering yubrshen/packages.el")

;;; Declare packages to be customized
(setq yubrshen-packages
      '(latex-preview-pane
        (yushen-personalization :location local)
        (literate-tools :location local)
        (org-config :location local)       ; use org-config of my old config for org as a package as is.
        ;; It actually breaks when I don't require org.

        ;; plantuml-mode
        ;; Unowned

        )
      )
;;;; latex-preview-pane
(defun yubrshen/init-latex-preview-pane ()
  (use-package latex-preview-pane))

;;;; yushen-personaization

(defun yubrshen/init-yushen-personalization ()
  (message "calling use-package yushen-personalization")
  (use-package yushen-personalization))

;;;; literate-tools
;; Tools for litreate programming

(defun yubrshen/init-literate-tools ()
  (message "calling use-package litreate-tools")
  (use-package literate-tools
    ))

;;;; org-config

(defun yubrshen/init-org-config ()
  (message "calling use-package org-config")
  (use-package org-config
    ;; :init
    ;; (setq org-plantuml-jar-path "~/bin/plantuml.jar")
    ))


;;; Unowned

;;;; plantuml-mode

;; (defun yubrshen/init-plantuml-mode ()
;;   (use-package plantuml-mode
;;     :init
;;     (setq org-plantuml-jar-path "~/bin/plantuml.jar")
;;     :config
;;     (setq org-plantuml-jar-path "~/bin/plantuml.jar")
;;     )
;;   ;; (spacemacs|use-package-add-hook plantuml-mode
;;   ;;   :pre-init
;;   ;;   (setq org-plantuml-jar-path "~/bin/plantuml.jar")
;;   ;;   )
;;   )
