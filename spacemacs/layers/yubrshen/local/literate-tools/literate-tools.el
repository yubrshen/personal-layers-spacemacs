  ;; to understand better of the code, seek the corresponding literate programming file:
  ;; ./source-decomposition-for-literate-programming.org
;;; literate-tools.el --- tools for literate programming

(provide 'literate-tools)

(defun language-in-the-buffer ()
  "The language in the current buffer.")

(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
                      (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun language-of-the-mode ()
  "Returns the string of the name of the language."
  (let ((buf-name-str (symbol-name (buffer-mode)))
        (lang-regx "\\(^.*\\)-mode"))
    (if (string-match lang-regx buf-name-str)
        (match-string 1 buf-name-str)
      buf-name-str)))

;; The function-name should support multiple languagus,
;; So far, python, emacs-lisp, and dart has been tested partially.

(defun function-name (str)
  "Extract the function name from str."
  ;; see the test cases below for illustration
  (let ((function-name-regex "[ \]*\\([[:word:]]+[ ]+\\)?\\([[:alnum:]_]+\\)"))
    (if (string-match function-name-regex str)
        (match-string 2 str)
      "to-be-named")))

;; unit test
(string= (function-name "def image123_cb(") "image123_cb")
(string= (function-name "  def process_traffic_lights(self): ") "process_traffic_lights")
(string= (function-name "  defun process_traffic_lights(self): ") "process_traffic_lights")
(string= (function-name "   process_traffic_lights(self): ") "process_traffic_lights")
(string= (function-name " bool isNoble(int atomicNumber)") "isNoble")

(defun truncate_first_line_front_space (code)
  "Reformat the code, to work out. It may be complicated with where the cursor is.
  It may impact the regular expression for the function name.
  "
  code)
(defun block-literate (src label language-type)
  "construct literate block by wrapping the src with label as the name of block."
  (let ((src-reformated (truncate_first_line_front_space src)))
    (concat
     "** " label "\n\n"
     "#+NAME:" label "\n"
     "#+BEGIN_SRC " language-type " :noweb no-export :tangle \n"
     src-reformated "\n"
     "#+END_SRC \n\n")))

;; unit test
(string= (block-literate "Hello world" "function" "python")
         "#+NAME:function\n#+BEGIN_SRC python :noweb tangle :tangle \nHello world\n#+END_SRC \n")

(defun literate-extraction (beg end provide-name)
  "Extract the selected region and replace the region by literate code label named by the
  function name of the region, also generate a literate code block in the clipboard.
  With universal argument, prompt user to provide the block name.
  Assume the cursor is at the beginning of a line."
  (interactive "r\nP")

  (let* ((source (buffer-substring-no-properties beg end))
         (block-name (if provide-name (read-string "Block name: ")
                       (function-name source)))
         (label (concat "<<" block-name ">>")) ; note this line would have problem when tangling
         (block (block-literate source block-name (language-of-the-mode))))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert label)
      (insert "\n"))
    (kill-new block)
    block))

  ;; Shift the selected region right if distance is postive, left if
  ;; negative

  (defun shift-region (distance)
    (let ((mark (mark)))
      (save-excursion
        (indent-rigidly (region-beginning) (region-end) distance)
        (push-mark mark t t)
        ;; Tell the command loop not to deactivate the mark
        ;; for transient mark mode
        (setq deactivate-mark nil))))

  (defun shift-right ()
    (interactive)
    (shift-region 1))

  (defun shift-left ()
    (interactive)
    (shift-region -1))

  ;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
  ;; the following so that Ctrl-Shift-Right Arrow moves selected text one
  ;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
  ;; column to the left:

  (global-set-key [C-S-right] 'shift-right)
  (global-set-key [C-S-left] 'shift-left)
