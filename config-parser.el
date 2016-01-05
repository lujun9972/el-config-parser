(require 'cl-lib)

(defun config-parser--parse-section (line)
  "Parse \"[section]\" to '(\"section\")"
  (when (string-match "^\\[\\([^]]+\\)\\]$" line)
    (list (match-string 1 line))))

(defun config-parser--parse-option (line sep)
  "Parse \"key=val\" to '(\"key\". \"val\"). Here assume SEP is ="
  (when (string-match (format "^\\([^%s[:space:]]+\\)[[:space:]]*%s[[:space:]]*\\(.+\\)$" sep sep) line)
    (cons (match-string 1 line)
          (match-string 2 line))))

(defun config-parser-read (file &optional sep)
  "Parse the cfg FILE, the result is an alist which car element is the section and cdr element is options in the section

options is also an alist like '(key . value) "
  (let* ((sep (or sep ":"))
         (file-content (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
         (file-lines (mapcar #'string-trim (split-string file-content "[\r\n]+")))
         (valid-file-lines (remove-if (lambda (line)
                                        (or (string-equal line "")
                                            (string-prefix-p "#" line)
                                            (string-prefix-p ";" line)))
                                      file-lines))
         (section '(""))
         result)
    (dolist (line valid-file-lines result)
      (cond ((config-parser--parse-section line)
             (unless (equal section '("")) ;empty section
               (push (reverse section) result))
             (setq section (config-parser--parse-section line)))
            ((config-parser--parse-option line sep)
             (push (config-parser--parse-option line sep) section))
            (t (error "invalid line:%s" line))))
    (push (reverse section) result)
    (reverse result)))

(defun config-parser--insert-section (section)
  "Insert the SECTION data"
  (let ((section-name (cond ((stringp section)
                             section)
                            ((listp section)
                             (car section)))))
    (unless (string-equal section-name "")
      (insert (format "[%s]" section-name))
      (newline))))

(defun config-parser--insert-option (option sep)
  "Insert the OPTION data with SEP as the delimiter"
  (let ((key (car option))
        (value (cdr option)))
    (insert (format "%s%s%s" key sep value))
    (newline)))

(defun config-parser-write (file config-data &optional sep)
  "Insert CONFIG-DATA into FILE with SEP as the delimiter"
  (let* ((sep (or sep ":")))
    (with-temp-file file
      (dolist (section config-data)
        (let ((section-name (car section))
              (options (cdr section)))
          (config-parser--insert-section section-name)
          (dolist (option options)
            (config-parser--insert-option option sep)))))))

;; (config-parser-write "retest.cfg" (config-parser-read "test.cfg" "="))
