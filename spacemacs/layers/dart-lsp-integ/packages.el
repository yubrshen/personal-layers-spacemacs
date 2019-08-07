;;; Dart Layer -*- lexical-binding: t; -*-
(message "Entering dart-lsp-integ/packages.el")
(setq dart-lsp-integ-packages
      '(;; Owned packages
        dart-mode
        ob-dart
        ;; Unowned packages
        org
        ))

;;; Owned Packages
;;;; dart-mode
(defun dart-lsp-integ/init-dart-mode ()
  (use-package dart-mode
    :defer t
    :config
    (progn
      (setq dart-format-on-save t)       ; to enable auto format at save time
      (defun project-try-dart (dir)         ; The following snippet assists project.el
        ;; in finding the project root for your dart file. It's required by both eglot and lsp
        (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                           (locate-dominating-file dir "BUILD"))))
          (if project
              (cons 'dart project)
            (cons 'transient dir))))
      (add-hook 'project-find-functions #'project-try-dart)
      (cl-defmethod project-roots ((project (head dart)))
        (list (cdr project)))

      ;; use lsp backend for dart-mode:
      (add-hook 'dart-mode-hook 'lsp)

      ;; Help lsp-mode find your project root automatically.
      (with-eval-after-load "projectile"
        (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
        (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

      (setq lsp-auto-guess-root t)
      ))
  )

;;;; ob-dart

(defun dart-lsp-integ/init-ob-dart ()
  (use-package ob-dart
    :ensure t
    :config
    ;; (add-to-list 'org-babel-load-languages  '(dart . t))
    (spacemacs|use-package-add-hook org
      :post-config (add-to-list 'org-babel-load-languages '(dart . t)))
    ;; to avoid the error:
    ;; An error occurred while post-configuring org in layer dart (error: (void-variable org-babel-load-languages))
    ;; must use spacemacs|use-package-add-hook as above

    ))

;;; Unowned packages

;;;; org
(defun dart-lsp-integ/post-init-org ()
  (setq org-structure-template-alist
        (append
         '(("sd" "#+BEGIN_SRC dart :noweb no-export\n\n#+END_SRC"))
         ;; add :noweb no-export to support multiple levels of enclosing of code blocks
         org-structure-template-alist))
  ;; (add-to-list 'org-babel-load-languages  '(dart . t))
  ;; The above does not work to avoid the following error:
  ;; An error occurred while post-configuring org in layer dart (error: (void-variable org-babel-load-languages))
  )
