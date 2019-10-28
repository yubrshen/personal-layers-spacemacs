;;; my/org-skip-inode-and-root
(defun my/org-skip-inode-and-root ()
  "
Retrun the position of the next child heading, if
a. there is any child
b. the first child's heading containts keyword
otherwise, return nil
"
  (when                                 ; when first child found and go to that
    (save-excursion
      (org-goto-first-child))
    (let ((eos (save-excursion          ; eos: end of the subtree or the end of the buffer
                 (or (org-end-of-subtree t)
                   (point-max))))
           (nh (save-excursion          ; nh: the position of the next heading or the end the buffer
                 (or (outline-next-heading)
                   (point-max))))
           (ks org-todo-keywords-1)     ; ks: all TODO and DONE keywords in the buffer
           mat)                         ; mat intialized to nil
      (save-excursion
        (org-goto-first-child)
        (while (and ks (not mat))       ; while there is still keywords, and mat is nil; that is to search one of the keywords
          (setq mat
            (re-search-forward (concat "\\*\\W+"
                                 (car ks)
                                 "\\W*")
              eos t))
          (setq ks (cdr ks))))
      (when mat                          ; when a keyword is found, return the position of the next heading
        nh))))

;;; my/org-skip-leaves
(defun my/org-skip-leaves ()
  "Returns the end of the subtree, if
a. there is no child, or
b. the first child has no keyword;
otherwise, return nil"
  (let ((eos (save-excursion            ; eos: end of the subtree or the end of the buffer
               (or (org-end-of-subtree t)
                 (point-max)))))
    (if (not (save-excursion
               (org-goto-first-child)))
      eos                               ; if there is no child (leave), returns the end of the current subtree
      (let ((ks org-todo-keywords-1)
             mat)                       ; mat initialized to nil
        (save-excursion
          (org-goto-first-child)
          (while (and ks (not mat))     ; while there is still keywords to search and there is none found
            (setq mat
              (re-search-forward (concat "\\*\\W+"
                                   (car ks)
                                   "\\W*")
                eos t))
            (setq ks (cdr ks))))
        (when (not mat)                 ; if no keyword found at the first child, returns the end of the subtree
          eos)))))                      ; otherwise returns nil

;;; my/org-skip-non-root-task-subtree
(defun my/org-skip-non-root-task-subtree ()
  "Returns the end of the current subtree if it's contained in a TODO task"
  (let ((eos (save-excursion
               (or (org-end-of-subtree t)
                 (point-max))))
         nonroot)                       ; nonroot initialized to nil
    (save-excursion
      (org-save-outline-visibility nil
        (org-reveal)
        (while (and (not nonroot) (org-up-heading-safe)) ; go to the parennt until a todo taks is found
          (setq nonroot (org-entry-get (point) "TODO")))))
    (when nonroot                       ; return the end of the current subtree if it's contained in a TODO task
      eos)))

;;; my/disallow-todo-state-for-projects
(defun my/disallow-todo-state-for-projects ()
  "Reset the heading to be TODO, if it is not one of TODO, DONE or CANCELLED"
  (when (my/org-skip-inode-and-root)
    (let ((ts (org-get-todo-state)))    ; ts: the TODO keyword of the current subtree
      (when (not (or (equal ts "TODO")
                   (equal ts "DONE")
                   (equal ts "CANCELLED")))
        (org-set-property "TODO" "TODO")))))

(add-hook 'org-after-todo-state-change-hook 'my/disallow-todo-state-for-projects)

;;; my/repeated-task-template
(defun my/repeated-task-template ()
  "Capture template for repeated tasks."
  (concat "* NEXT %?\n"
          "  SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d>>\")\n"
          "  :PROPERTIES:\n"
          "  :REPEAT_TO_STATE: NEXT\n"
          "  :RESET_CHECK_BOXES: t\n  :END:\n  %U\n  %a"))

;;; setq
(setq
;;;; org files
  org-directory "~/Dropbox/org"
  org-default-notes-file "~/Dropbox/org/inbox.org"
  my/inbox "~/Dropbox/org/inbox.org"
  my/project "~/Dropbox/org/projects-and-tasks.org"
  my/someday "~/Dropbox/org/someday.org"
  my/birthdays "~/Dropbox/org/birthdays.org"
  org-agenda-files (list my/project my/inbox)
;;;; org-todo-keywords
  org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
     (sequence "HOLD(h)" "|" "PNEDING(p)" "|"  "CANCELED(c)"))
;;;; other org configuration variables
  org-agenda-show-future-repeats nil
  org-agenda-dim-blocked-tasks nil
  org-catch-invisible-edits 'smart
  org-enforce-todo-dependencies t
  org-log-into-drawer t
  org-modules '(org-info org-checklist)
  org-refile-allow-creating-parent-nodes 'confirm
  org-refile-use-outline-path t
;;;; org-capture-templates
  org-capture-templates
  '(("r" "repeated task" entry
      (file my/inbox)
      #'my/repeated-task-template)
     ("t" "todo" entry
       (file my/inbox)
       "* TODO %?\n  %U\n  %a")
     ("p" "plain" entry
       (file my/inbox)
       "* %?\n  %U\n  %a")
     ("w" "waiting" entry
       (file my/inbox)
       "* WAITING %?\n  %U\n  %a"))
;;;; org-refile related settings
  org-refile-targets '((nil . (:maxlevel . 9))
                        (my/project :maxlevel . 9)
                        (my/someday :maxlevel . 9))
  ;; org-refile-use-outline-path 'file'
  org-outline-path-complete-in-steps nil
  ;; to use Helm complete, not in steps with the default mechanism
 ;;;; org-agenda-prefix-format
  ;; org-agenda-prefix-format '((todo . "")
  ;;                             (search . "")
  ;;                             (tags . "")
  ;;                             (agenda . ""))
  ;; Show the bread-crumb with at least 30 charaters
  org-agenda-prefix-format '((todo . "%-30b") (tags . "%-30b") (agenda . "%-30b"))

;;;; org-agenda-custom-commands
  org-agenda-custom-commands
  '(
;;;;; Repeated or scheduled
     ("r" "Repeated or scheduled tasks"
       ((tags
          "SCHEDULED={<[^<>.+]+[.+]?[+][^<>.+]+>}+TODO={NEXT\\|TODO}+daily"
          ((org-agenda-overriding-header "Daily Tasks")
            (org-agenda-sorting-strategy '(priority-down))))
         (tags
           "SCHEDULED={<[^<>.+]+[.+]?[+][^<>.+]+>}+TODO={NEXT\\|TODO}-daily"
           ((org-agenda-overriding-header "Repeated Tasks")))
         (tags-todo "SCHEDULED={<[^<>.+]+>}+TODO={NEXT\\|TODO}"
           ((org-agenda-overriding-header "Scheduled Tasks"))))
       ((org-agenda-sorting-strategy '(scheduled-up priority-down))))
;;;;; Tasks needing attention
     ("a" "Tasks that need attention"
       ((tags "TODO=\"HOLD\""
          ((org-agenda-overriding-header "HOLD")))
         (tags "TODO={DONE\\|CANCELLED}"
           ((org-agenda-overriding-header "DONE or CANCELLED at top level")
             (org-agenda-skip-function #'my/org-skip-non-root-task-subtree)))))
;;;;; General Agenda
     ("g" "My General Agenda"
       (
         (agenda ""
           ((org-agenda-files (list my/inbox my/project my/birthdays))
             (org-agenda-span 'day)))
         (tags "@heavy-@home+TODO=\"NEXT\""
           ((org-agenda-overriding-header "NEXT @heavy")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "-@heavy-@home+TODO=\"NEXT\""
           ((org-agenda-overriding-header "NEXT non-heavy")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "@heavy-@home+TODO=\"TODO\""
           ((org-agenda-overriding-header "@heavy")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "-@heavy-@home+TODO=\"TODO\""
           ((org-agenda-overriding-header "non-heavy")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "@home+@heavy+TODO=\"NEXT\""
           ((org-agenda-overriding-header "NEXT @heavy@home")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "@home-@heavy+TODO=\"NEXT\""
           ((org-agenda-overriding-header "NEXT @home")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "@home+@heavy+TODO=\"TODO\""
           ((org-agenda-overriding-header "@heavy@home")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "@home-@heavy+TODO=\"TODO\""
           ((org-agenda-overriding-header "@home")
             (org-agenda-sorting-strategy '(priority-down))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))

         (tags "TODO={.*}"
           ((org-agenda-files (list my/inbox))
             (org-agenda-overriding-header "Inbox")
             (org-tags-match-list-sublevels nil)
             (org-agenda-sorting-strategy '(priority-down))))
         (todo "WAITING"
           ((org-agenda-overriding-header "Waiting")
             (org-agenda-sorting-strategy '(priority-down))))
         (tags "-{^@.*}+TODO={NEXT\\|TODO}"
           (
             (org-agenda-overriding-header "Tasks Without Context")
             (org-agenda-skip-function #'my/org-skip-inode-and-root)
             (org-agenda-sorting-strategy
               '(todo-state-down priority-down))))
         (tags "TODO=\"TODO\"+@office"
           ((org-agenda-overriding-header "Active Work Projects")
             (org-agenda-sorting-strategy '(priority-down))
             (org-tags-match-list-sublevels nil)
             (org-agenda-skip-function
               '(or
                  (my/org-skip-leaves)
                  (org-agenda-skip-subtree-if 'nottodo '("NEXT"))))))
         (tags "TODO=\"TODO\"+@office"
           ((org-agenda-overriding-header "Stuck Work Projects")
             (org-agenda-sorting-strategy '(priority-down))
             (org-tags-match-list-sublevels nil)
             (org-agenda-skip-function
               '(or
                  (my/org-skip-leaves)
                  (org-agenda-skip-subtree-if 'todo '("NEXT"))))))
         (tags "TODO=\"TODO\"-@office"
           ((org-agenda-overriding-header "Active Projects")
             (org-agenda-sorting-strategy '(priority-down))
             (org-tags-match-list-sublevels nil)
             (org-agenda-skip-function
               '(or
                  (my/org-skip-leaves)
                  (org-agenda-skip-subtree-if 'nottodo '("NEXT"))))))
         (tags "TODO=\"TODO\"-@office"
           ((org-agenda-overriding-header "Stuck Projects")
             (org-agenda-sorting-strategy '(priority-down))
             (org-tags-match-list-sublevels nil)
             (org-agenda-skip-function
               '(or
                  (my/org-skip-leaves)
                  (org-agenda-skip-subtree-if 'todo '("NEXT"))))))
         (tags "@read_watch_listen+TODO=\"NEXT\""
           ((org-agenda-overriding-header "NEXT @read/watch/listen")
             (org-agenda-sorting-strategy '(priority-down effort-up))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         (tags "@read_watch_listen+TODO=\"TODO\""
           ((org-agenda-overriding-header "@read/watch/listen")
             (org-agenda-sorting-strategy '(priority-down effort-up))
             (org-agenda-skip-function
               '(or
                  (my/org-skip-inode-and-root)
                  (org-agenda-skip-entry-if 'scheduled)))))
         ))
;;;;; Weekly review
     ("W" "Weekly review"
       ;; https://emacs.stackexchange.com/questions/52994/org-mode-agenda-show-list-of-tasks-done-in-the-past-and-not-those-clocked
       ;; show an agenda view with all items marked as "DONE" in the last weeks
       agenda ""
       ((org-agenda-start-day "-7d")    ; started 7 days ago
         (org-agenda-span 7)            ; for 7 days
         ;; (org-agenda-start-on-weekday 1) ; starting on Monday
         ;; (org-agenda-start-with-log-mode '(closed)) ; include only closed
         ;; (org-agenda-archives-mode t)   ; include archived items
         (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "^\\*+ DONE ")) ; At least one level tasks
         ))
;;;;; DONE Sparse Tree
     ("d" occur-tree "DONE")
     )
  )

;; The following is not needed for my own as I'm using Spacemacs
;; (define-key global-map "\C-cc" 'org-capture)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cl" 'org-store-link)
