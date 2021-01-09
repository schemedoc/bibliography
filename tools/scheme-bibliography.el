(defvar scheme-bibliography-pdf-todo-directory nil)
(defvar scheme-bibliography-pdf-directory nil)

(defun scheme-bibliography-analyze-markdown-entry ()
  (interactive)
  (cl-assert scheme-bibliography-pdf-todo-directory)
  (cl-assert scheme-bibliography-pdf-directory)
  (unless (looking-at "^  \* ")
    (error "Point is not at the beginning of a bibliography entry"))
  ;; Take the PDF and PS files we already have.
  ;; Copy them into the destination directory.
  ;; When a file is gzipped, decompress the copy.
  ;; Take the SHA1 checksums of the decompressed files.
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (with-temp-buffer
      (insert line)
      (goto-char (point-min))
      (search-forward "Available online:")
      (let ((basenames '()) (non-compressed-basenames '()))
        (while (re-search-forward "readscheme.org[^)]*/\\([^/]+\\))" nil t)
          (push (match-string 1) basenames))
        (dolist (basename basenames)
          (let ((old-file
                 (car (directory-files-recursively
                       scheme-bibliography-pdf-todo-directory
                       (concat "^" (regexp-quote basename) "$")))))
            (when old-file
              (message "Copying %s" old-file)
              (let ((new-file (concat (file-name-as-directory
                                       scheme-bibliography-pdf-directory)
                                      basename)))
                (copy-file old-file new-file t t t t)
                (cond ((let ((case-fold-search t))
                         (string-match "\\(.*\\)\\.gz$" basename))
                       (let ((default-directory
                               scheme-bibliography-pdf-directory))
                         (message "Running gunzip")
                         (shell-command
                          (concat "gunzip" " "
                                  (shell-quote-argument basename)))
                         (push (match-string 1 basename)
                               non-compressed-basenames)))
                      (t
                       (push basename non-compressed-basenames)))))))
        (when non-compressed-basenames
          (let ((default-directory scheme-bibliography-pdf-directory))
            (message "%s"
                     (shell-command
                      (string-join (cons "shasum"
                                         (mapcar
                                          #'shell-quote-argument
                                          non-compressed-basenames))
                                   " "))))))))
  ;; Highlight the title of the paper for copy-pasting into a search engine.
  (goto-char (point-at-bol))
  (re-search-forward "\. \"\\(.*?\\)\".")
  (set-mark (match-beginning 1))
  (goto-char (match-end 1)))
