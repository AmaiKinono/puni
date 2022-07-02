;;; common.el --- Common code to run before each test -*- lexical-binding: t; -*-

;;;; Libraries

(require 'rx)
(require 'ert)

;;;; Internals

(defvar chapter-name nil)
(defvar chapter-cursor nil)
(defvar temporary-state nil)

(defun state-handler ()
  "Read the following state and compare it to `temporary-state'."
  (let (eof-str state-str beg end cursor-pos)
    ;; Parse opening line.
    (if (looking-at (rx "<<" (group (+ (not (any blank "\n")))) (* blank) "\n"))
        (setq eof-str (match-string 1))
      (error (format "Invalid opening line for heredoc: %s"
                     (buffer-substring (point) (line-end-position)))))
    ;; Record string start position
    (forward-line)
    (setq beg (point))
    ;; Parse ending identifier and move to the end of heredoc
    (if (search-forward eof-str nil t)
        (setq end (- (point) (length eof-str)))
      (error (format "Heredoc ending identifier %s not found" eof-str)))
    ;; Parse string & cursor position
    (setq state-str (buffer-substring beg end))
    (with-temp-buffer
      (insert state-str)
      (goto-char (point-min))
      (if (search-forward chapter-cursor nil t)
          (progn (delete-char (- (length chapter-cursor)))
                 (setq cursor-pos (point)))
        (error (format "Cursor string %s not found" chapter-cursor)))
      (when (search-forward chapter-cursor nil t)
        (error (format "Multiple occurence of cursor string %s"
                       chapter-cursor)))
      (setq state-str (buffer-substring (point-min) (point-max))))
    ;; Compare with `temporary-state'.
    (if (or (null temporary-state)
            (equal (cons state-str cursor-pos)
                   temporary-state))
        (setq temporary-state (cons state-str cursor-pos))
      (error (format "Wrong state encountered.
[Target, cursor pos: %s]
%s
[Result, cursor pos: %s]
%s" cursor-pos state-str (cdr temporary-state) (car temporary-state))))))

(defun elisp-form-handler ()
  (let* ((beg (point))
         (end (progn (forward-sexp) (point)))
         (form (read (buffer-substring beg end))))
    ;; Special treatment for `chapter' calls.
    (if (eq (car form) 'chapter)
        (eval form)
      (with-temp-buffer
        (insert (car temporary-state))
        (goto-char (cdr temporary-state))
        (eval form)
        (setf (car temporary-state) (buffer-substring (point-min) (point-max))
              (cdr temporary-state) (point))))))

(defun comment-handler ()
  (forward-line))

(defun next-form ()
  "Go to the start of next top-level form in a test file.
It returns the handler of the form, which processes the form and
also skips over it.  If there's no following form, return nil.
If the following form is invalid, throw an error."
  (when (looking-at (rx (+ (or space "\n"))))
    (goto-char (match-end 0)))
  (cond
   ((looking-at "<<") #'state-handler)
   ((looking-at "(") #'elisp-form-handler)
   ((looking-at ";;") #'comment handler)
   ((eobp) nil)
   (t (error (format "Invalid top level form: %s"
                     (buffer-substring (point) (line-end-position)))))))

;;;; APIs

(cl-defun chapter (name &key setup cursor)
  "Begin a new test chapter.
A chapter defines several state, and the forms to call in between
them.  If any form throw an error, or calling any of the forms
doesn't result in the following state, the test fails.

Each state is a string, defined by a heredoc in the test files.
Each state includes a cursor position, which is marked by a
string CURSOR (\"|\" by default).

NAME is the name of the chapter.  SETUP is a form to call before
running the chapter, typically it enables some major mode."
  (setq temporary-state nil)
  (setq chapter-name nil)
  (setq chapter-cursor (or cursor "|"))
  (eval setup))

(defun run-test (file)
  "Run test defined by FILE.
A test file can contain the following top-level forms:

- Comments: Single line comments begin in \";;\"
- Forms: Elisp forms to call
- States: Strings in heredoc

See `chapter' for details."
  (let (handler)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (setq handler (next-form))
        (funcall handler)))))
