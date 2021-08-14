;; checkdoc-hack.el --- Suppress unwanted checkdoc warnings -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao WANG
;; License: GPL v3, or (at your option) any later version

(defun puni-checkdoc-create-error (text start end &optional unfixable)
  "Create a checkdoc error conditionally.
When it's an error about the argument of backward moving
functions, don't create the error."
  (let ((file (buffer-file-name)))
    (if (string-match "Argument .* should appear (as .*) in the doc string"
                      text)
        (save-excursion
          (goto-char start)
          (beginning-of-line)
          (unless (looking-at (rx (* space) "\"Backward version of"))
            (checkdoc--create-error-for-checkdoc
             text start end unfixable)))
      (checkdoc--create-error-for-checkdoc
       text start end unfixable))))

(setq checkdoc-create-error-function #'puni-checkdoc-create-error)
