;;; -*- lexical-binding: t; -*-

;; (message "Entering yubrshen/layers.el")

(configuration-layer/declare-layers
 '(;; (plantuml
   ;;   )
   ;; ipython-notebook
   ;; There is a problem of conflict between org-capture, org-agenda with ipython-notebook.
   ;; Here is the following trace:
   ;;
   ;; Debugger entered--Lisp error: (file-missing "Setting current directory" "No such file or directory" "/home/yshen/org/")
   ;; call-process("/bin/bash" nil t nil "-c" "jupyter kernelspec list --json")
   ;; apply(call-process "/bin/bash" nil t nil ("-c" "jupyter kernelspec list --json"))
   ;; process-file("/bin/bash" nil t nil "-c" "jupyter kernelspec list --json")
   ;; shell-command-to-string("jupyter kernelspec list --json")
   ;; ob-ipython--get-kernels()
   ;; ob-ipython-auto-configure-kernels()
   ;; run-hooks(change-major-mode-after-body-hook text-mode-hook outline-mode-hook org-mode-hook)
   ;; apply(run-hooks (change-major-mode-after-body-hook text-mode-hook outline-mode-hook org-mode-hook))
   ;; run-mode-hooks(org-mode-hook)
   ;; org-mode()
   ;; set-auto-mode-0(org-mode nil)
   ;; set-auto-mode()
   ;; normal-mode(t)
   ;; after-find-file(t t)
   ;; find-file-noselect-1(#<buffer notes.org> "~/org/notes.org" nil nil "~/org/notes.org" nil)
   ;; find-file-noselect("/home/yshen/org/notes.org")
   ;; org-capture-target-buffer("")
   ;; org-capture-set-target-location()
   ;; org-capture(nil)
   ;; funcall-interactively(org-capture nil)
   ;; call-interactively(org-capture nil nil)
   ;; command-execute(org-capture)

   ;; It seems that the problem is that it's looking for a file of /home/yshen/org/notes.org
   ;; which does not exist.
   ;; Why does org-capture need to open that file?

   ))
