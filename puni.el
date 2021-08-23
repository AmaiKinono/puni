;;; puni.el --- Parentheses Universalistic -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang

;; Author: Hao Wang <amaikinono@gmail.com>
;; Maintainer: Hao Wang <amaikinono@gmail.com>
;; Created: 08 Aug 2021
;; Keywords: convenience, lisp, tools
;; Homepage: https://github.com/AmaiKinono/puni
;; Version: 0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Puni is a package for soft deletion, which means deleting while keeping
;; expressions balanced.  Here are the main features:

;; - A set of customizable soft deletion commands, enabled by `puni-mode'.
;; - A simple API `puni-soft-delete-by-move', for defining your own soft
;;   deletion commands.
;; - Completely based on Emacs built-in mechanisms, doesn't contain any
;;   language-specific logic, yet work on many major modes.

;; It's recommended to use Puni with `electric-pair-mode'.  Read README.md to
;; know more about Puni.  If you haven't received such a file, please visit
;; https://github.com/AmaiKinono/puni.

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'rx)
(require 'subr-x)

(defgroup puni nil
  "Customizable soft deletion."
  :group 'convenience
  :group 'lisp
  :group 'tools
  :prefix "puni-"
  :link '(url-link "https://github.com/AmaiKinono/puni"))

;;;; Internals

(defvar puni--debug nil
  "Turn on debug mode when non-nil.")

;;;;; Probes

(defun puni--line-empty-p ()
  "Return t if current line is empty or contains only spaces."
  (save-excursion
    (beginning-of-line)
    (looking-at (rx line-start (* space) line-end))))

(defun puni--in-string-p ()
  "Return t if point is in a string.
Notice this returns nil if point is before the opening quote, or
after the end quote."
  (eq (syntax-ppss-context (syntax-ppss)) 'string))

(defun puni--in-comment-p ()
  "Return t if point is in a comment.
Notice this returns nil if point is before/in the opening
delimiter, or after/in the end delimiter."
  (eq (syntax-ppss-context (syntax-ppss)) 'comment))

;;;;; Errors

(defun puni--error-if-before-point (bound)
  "Error if BOUND is non-nil and is before point."
  (when (and bound (< bound (point)))
    (error "BOUND is before point")))

(defun puni--error-if-after-point (bound)
  "Error if BOUND is non-nil and is after point."
  (when (and bound (> bound (point)))
    (error "BOUND is after point")))

;;;;; Syntax

;; Ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=37452

(defun puni--syntax-class-to-char (syntax-class)
  "Return the designator char of SYNTAX-CLASS."
  (aref " .w_()'\"$\\/<>@!|" syntax-class))

(defun puni--syntax-char-after (&optional point)
  "Return the syntax code after POINT, described by a char.
When POINT is nil, return the syntax code after the current
point.  When POINT doesn't exist, or there's no char after POINT,
return nil.

For the meaning of the returned char, see `modify-syntax-entry'."
  (let ((point (or point (point))))
    (unless (or (< point (point-min))
                (>= point (point-max)))
      (puni--syntax-class-to-char (syntax-class (syntax-after point))))))

;;;;; Basic move: blank

;; NOTE: ALl "basic moves" (take those move forward as example), except those
;; explicitely deals with blanks, assumes they starts in a position where no
;; spaces are after point, and will go to a position where no spaces are before
;; point.

(defun puni--forward-blanks (&optional bound)
  "Jump forward whitespaces and newlines.
Return t if success.  When BOUND is non-nil, don't go further
than BOUND."
  (puni--error-if-before-point bound)
  (when (looking-at (rx (+ (or space "\n"))))
    (goto-char (match-end 0))
    (when (and bound (> (point) bound))
      (goto-char bound))
    t))

(defun puni--backward-blanks (&optional bound)
  "Backward version of `puni--forward-blanks'."
  (puni--error-if-after-point bound)
  (unless (bobp)
    (let ((from (point)))
      (while (and (or (null bound) (> (point) bound))
                  (or (puni--backward-syntax " " bound)
                      (when (bolp) (forward-char -1) t))))
      (let ((to (point)))
        (unless (eq from to) to)))))

;;;;; Basic move: char

(defun puni--skip-chars-forward (string &optional bound)
  "Go forawrd accross chars in STRING.
When BOUND is non-nil, stop before BOUND.

This is the same as `skip-chars-forward', except that:

- It signals an error is BOUND is before point at the first place.
- If it fails, return nil.
- If sucess, return the point after move.

See `skip-chars-forward' for the real meaning of STRING."
  (puni--error-if-before-point bound)
  (pcase (skip-chars-forward string bound)
    (0 nil)
    (_ (point))))

(defun puni--skip-chars-backward (string &optional bound)
  "Backward version of `puni--skip-chars-forward'."
  (puni--error-if-after-point bound)
  (pcase (skip-chars-backward string bound)
    (0 nil)
    (_ (point))))

(defun puni--forward-same-char (&optional bound)
  "Go forward consecutive same characters.
It returns the point after move.  If BOUND is non-nil, stop
before BOUND.  If it fails, return nil."
  (unless (eobp)
    ;; See `skip-chars-forward'.  I've tested that `regexp-quote' works for the
    ;; situations mentioned there.
    (puni--skip-chars-forward (regexp-quote (char-to-string (char-after)))
                              bound)))

(defun puni--backward-same-char (&optional bound)
  "Backward version of `puni--forward-same-char'."
  (unless (bobp)
    (puni--skip-chars-backward (regexp-quote (char-to-string (char-before)))
                               bound)))

;;;;; Basic move: syntax

(defun puni--forward-syntax (syntax &optional bound)
  "Go forward across chars in specified syntax classes.
SYNTAX is a string of syntax code chars.  When BOUND is non-nil,
stop before BOUND.

This is the same as `skip-syntax-forward', except that:

- It signals an error is BOUND is before point at the first place.
- If it fails, return nil.
- If sucess, return the point after move."
  (puni--error-if-before-point bound)
  (pcase (skip-syntax-forward syntax bound)
    (0 nil)
    (_ (point))))

(defun puni--backward-syntax (syntax &optional bound)
  "Backward version of `puni--forward-syntax'."
  (puni--error-if-after-point bound)
  (pcase (skip-syntax-backward syntax bound)
    (0 nil)
    (_ (point))))

(defun puni--forward-same-syntax (&optional bound)
  "Move point past all characters with the same syntax class.
It returns the point after move.  If BOUND is non-nil, stop
before BOUND.

This is more robust than `forward-same-syntax' because it takes
`syntax-table' text properties into account.  See the docstring
of `char-syntax'."
  (when-let ((syntax (puni--syntax-char-after)))
    (puni--forward-syntax (char-to-string syntax) bound)))

(defun puni--backward-same-syntax (&optional bound)
  "Backward version of `puni--forward-same-syntax'."
  (when-let (syntax (puni--syntax-char-after (1- (point))))
    (puni--backward-syntax (char-to-string syntax) bound)))

;;;;; Basic move: symbol

(defun puni--symbol-syntax-p (&optional point)
  "Check if char at POINT has symbol or word syntax.
This includes the situation where it's a char with \"escape\"
syntax (like backslash), but followed by a char with symbol or
word syntax."
  (let ((point (or point (point))))
    (or (memq (puni--syntax-char-after point) '(?_ ?w))
        (and (eq (puni--syntax-char-after point) ?\\)
             (memq (puni--syntax-char-after (1+ point)) '(?_ ?w))))))

(defun puni--symbol-prefix-p (&optional point)
  "Check if char at POINT is a symbol prefix.
This means it's a char with \"'\" syntax, and the point after it
satisfies `puni--symbol-syntax-p'."
  (let ((point (or point (point))))
    (save-excursion
      (goto-char point)
      ;; In case ther are multiple prefixes... Could that be a case?
      (and (puni--forward-syntax "'")
           (puni--symbol-syntax-p)))))

(defun puni--forward-symbol (&optional bound)
  "Move forward an symbol if there's one in front.
A symbol is a symbol in Emacs convention, allowing escaped chars
with symbol syntax in it, plus an expression prefix before it (if
exist).

If BOUND is non-nil, stop before BOUND."
  ;; It may be a good idea to treat a series of punctuations as a symbol (think
  ;; of operators).  Unfortunately there are major modes where "<>" should be
  ;; delimiters, but are given the punctuation syntax.
  (puni--error-if-before-point bound)
  (let ((from (point)))
    (when (puni--symbol-prefix-p)
      (puni--forward-same-syntax bound))
    (while (and (puni--symbol-syntax-p)
                (puni--forward-same-syntax bound)))
    (let ((to (point)))
      (unless (eq from to)
        to))))

(defun puni--backward-symbol (&optional bound)
  "Backward version of `puni--forward-symbol'."
  (puni--error-if-after-point bound)
  (let ((from (point)))
    (while (and (puni--symbol-syntax-p (1- (point)))
                (puni--backward-same-syntax bound)))
    (let ((to (point)))
      (unless (eq from to)
        (puni--backward-syntax "'" bound)
        (point)))))

;;;;; Basic move: string

(defun puni--forward-string ()
  "Move forward a string.
Return the point if success, otherwise return nil."
  (let ((from (point))
        to)
    (save-excursion
      (when (and (progn (puni--forward-syntax "\\")
                        (puni--forward-syntax "\""))
                 (puni--in-string-p))
        ;; The default `forward-sexp' could jump over a string.
        ;; `forward-sexp-function' from the major-mode sometimes doesn't, when
        ;; they jump to the end of a further delimiter.
        (let ((forward-sexp-function nil))
          (goto-char from)
          (forward-sexp))
        (setq to (point))))
    (when (and to (not (eq from to)))
      (goto-char to))))

(defun puni--backward-string ()
  "Backward version of `puni--forward-string'."
  (let ((from (point))
        to)
    (save-excursion
      (when (and (puni--backward-syntax "\"")
                 (progn (puni--backward-syntax "\\") t)
                 (puni--in-string-p))
        (let ((forward-sexp-function nil))
          (goto-char from)
          (forward-sexp -1))
        (setq to (point))))
    (when (and to (not (eq from to)))
      (goto-char to))))

;;;;; Basic move: comment

(defun puni--forward-comment-block ()
  "Jump forward a whole comment block.
Return the point if success.  When the closing delimiter of the
comment is newline, this goes to the point before the newline."
  (let ((from (point))
        to)
    (save-excursion
      ;; `forward-comment' could do its work but return nil, e.g., when we are
      ;; before a single line comment at the end of the file, and there's no
      ;; trailing newline.
      (when (progn (forward-comment 1)
                   (not (eq (point) from)))
        (setq to (point))))
    (when to (goto-char to))))

(defun puni--backward-comment-block ()
  "Jump backward a whole comment block.
Return the point if success."
  (when (forward-comment -1)
    (puni--forward-blanks)
    (point)))

;;;;; Basic move: single line comment

;; This section has nothing to do with core functionality, but only the
;; interactive `puni-forward/backward-sexp' commands.

(defun puni--begin-of-single-line-comment-p ()
  "Return t if point is at the opening delimiter of a single line comment.
Doesn't work on a single-line comment at the end of buffer, and
there's no trailing newline."
  (save-excursion
    (and (not (puni--forward-blanks))
         ;; We don't use `puni--forward-comment-block' here, see its docstring.
         (let ((from (point)))
           (forward-comment 1)
           (not (eq from (point))))
         (eq (char-before) ?\n))))

(defun puni--end-of-single-line-comment-p ()
  "Return t if point is after the end delimiter of a single line comment.
The end delimiter of such a comment is a newline, so this means
the point is at the beginning of next line (or next n lines, if
next n-1 lines are empty)."
  (and (eq (puni--syntax-char-after (1- (point))) ?>)
       (eq (char-before) ?\n)
       (save-excursion (forward-comment -1))))

(defun puni--forward-consecutive-single-line-comments ()
  "Jump forward a series of single-line comments.
Return the point if success."
  (let ((from (point))
        to)
    (save-excursion
      (while (and (puni--begin-of-single-line-comment-p)
                  (progn (forward-line)
                         (beginning-of-line)
                         (puni--forward-syntax " ")
                         t)))
      (unless (eq from (point))
        (puni--backward-blanks from)
        (setq to (point))))
    (when to (goto-char to))))

(defun puni--backward-consecutive-single-line-comments ()
  "Jump backward a series of single-line comments.
Return the point if success."
  (let ((from (point))
        to)
    (save-excursion
      (while (and (puni--end-of-single-line-comment-p)
                  (forward-comment -1)
                  (progn (puni--backward-syntax " ")
                         (puni--end-of-single-line-comment-p))
                  (not (bobp))
                  (save-excursion (forward-line -1)
                                  (puni--begin-of-single-line-comment-p))))
      (unless (>= (point) from)
        (puni--forward-blanks from)
        (setq to (point))))
    (when to (goto-char to))))

;;;;; Basic move: sexp

;; In this section, we build necessary components for the final
;; `puni--strict-forward/backward-sexp' functions.

(defun puni--forward-same-char-and-syntax (&optional bound)
  "Move forward consecutive same chars with same syntax.
When BOUND is non-nil, stop before BOUND.

Return the point if success, otherwise return nil."
  (let* ((char-bound (save-excursion
                       (puni--forward-same-char bound)))
         (syntax-bound (save-excursion
                         (puni--forward-same-syntax bound))))
    (when (and char-bound syntax-bound)
      (goto-char (min char-bound syntax-bound)))))

(defun puni--backward-same-char-and-syntax (&optional bound)
  "Backward version of `puni--forward-same-char-and-syntax'."
  (let* ((char-bound (save-excursion
                       (puni--backward-same-char bound)))
         (syntax-bound (save-excursion
                         (puni--backward-same-syntax bound))))
    (when (and char-bound syntax-bound)
      (goto-char (max char-bound syntax-bound)))))

(defun puni--forward-syntax-block ()
  "Move forward a symbol, string or chars with the same syntax.
Return the point if success, otherwise return nil."
  (or (puni--forward-symbol)
      (puni--forward-string)
      (puni--forward-same-char-and-syntax)))

(defun puni--backward-syntax-block ()
  "Backward version of `puni--forward-syntax-block'."
  (or (puni--backward-symbol)
      (puni--backward-string)
      (puni--backward-same-char-and-syntax)))

(defun puni--forward-sexp-wrapper (&optional n)
  "A wrapper around `forward-sexp'.
Move forward N sexp, return the point if success, otherwise
return nil.

This wrapper is here since `forward-sexp' can fail differently in
different major modes, e.g., the built-in one for Lisp will throw
a `scan-error', the one from `nxml-mode' throws a plain error,
while the one from `web-mode' just does nothing and returns nil."
  (condition-case _
      (let ((from (point))
            (to (progn (forward-sexp n) (point))))
        (unless (eq from to) to))
    (error nil)))

(defun puni--primitive-forward-sexp ()
  "Move forward a sexp by `forward-sexp'.
This fixes some of its behaviors, see the implementation."
  ;; In Lisp mode:
  ;;
  ;;     symbol|,__
  ;;
  ;; "_" means space, and its end is the end of buffer.  Try
  ;; `forward/backward-sexp'.
  (if (puni--forward-sexp-wrapper)
      (progn (puni--backward-syntax " ")
             (point))
    ;; In lisp mode, it's common to have this when editing:
    ;;
    ;;     (foo |')
    ;;
    ;; `forward-sexp' can't parse forward since there's no expression after the
    ;; "'", but we should be able to delete it.
    (when (eq (puni--syntax-char-after) ?')
      (forward-char) (point))))

(defun puni--primitive-backward-sexp ()
  "Backward version of `puni--primitive-forward-sexp'."
  (if (puni--forward-sexp-wrapper -1)
      (progn (puni--forward-syntax " ")
             (point))
    (when (eq (puni--syntax-char-after (1- (point))) ?')
      (backward-char) (point))))

;; NOTE: The real challenge is what to do when the delimiter is not the
;; symbol/char at point, but something like "bound of symbol" or "newline"?
(defun puni--pair-or-in-delim-p (beg end)
  "Return t if BEG and END is a pair of delimiter, or in the same delimiter.
This uses syntax table and some heuristic, and is not completely
reliable.  It's also not generic, as it's designed only for a
branch in `puni--inside-delim-p'.  So it assumes one of BEG and
END is the bound of delimiter."
  (when (eq beg end) (error "BEG is the same as END"))
  (or (eq beg (1- end))
      (eq (save-excursion (goto-char beg)
                          (puni--forward-same-syntax end))
          end)
      (let ((beg-syntax (puni--syntax-char-after beg))
            (end-syntax (puni--syntax-char-after (1- end)))
            (beg-char (char-after beg))
            (end-char (char-before end)))
        ;; If beg & end are a pair of delimiters, we think beg is paired with
        ;; end.
        (or (and (eq beg-syntax ?\() (eq end-syntax ?\)))
            (and (eq beg-syntax ?<) (eq end-syntax ?>))
            ;; If we've reached here, we need to consider the situations
            ;; where BOUND is a delimiter (as assumed), but doesn't have
            ;; delimiter syntax.
            (and (eq beg-syntax ?.) (eq end-syntax ?.)
                 (eq beg-char ?<) (eq end-char ?>))
            (and (eq beg-syntax end-syntax)
                 (eq beg-char end-char))))))

(defun puni--inside-delim-p (pt beg end direction)
  "See if PT is inside the delimiters at BEG or END.
if DIRECTION is `forward', check if the char after PT is inside
any of the delimiters, or if DIRECTION is `backward', check the
char before PT instead.

By \"inside\" we mean: It's part of, or paired with the
beginning/end delimiter, and deleting it will cause unbalanced
state."
  (unless (< beg pt end) (error "PT is not between BEG and END"))
  ;; Assume a string can't be a delimiter. We also assume a symbol can't be a
  ;; delimiter.  This is not true, but many major modes thinks "a = b" is a
  ;; sexp, where it's actually safe to delete a or b.  In this situation, it's
  ;; the "bound of symbol" being the delimiter, not the symbol itself.
  (unless (or (save-excursion (goto-char beg) (or (puni--forward-string)
                                                  (puni--forward-symbol)))
              (save-excursion (goto-char end) (or (puni--backward-string)
                                                  (puni--backward-symbol)))
              ;; If BEG is the beginning of a single line comment, and pt is
              ;; inside consecutive comment-opening delimiter chars, we also
              ;; don't think it's inside the delimiter, as it's common to
              ;; delete them one by one.
              (save-excursion
                (goto-char beg)
                (and (puni--begin-of-single-line-comment-p)
                     (looking-at (concat (regexp-quote (char-to-string
                                                        (char-after)))
                                         "*"))
                     (> (match-end 0) (pcase direction
                                        ('forward pt)
                                        ('backward (1- pt))
                                        (_ (error "Invalid DIRECTION")))))))
    (pcase direction
      ('forward (or (puni--pair-or-in-delim-p beg (1+ pt))
                    (puni--pair-or-in-delim-p pt end)))
      ('backward (or (puni--pair-or-in-delim-p beg pt)
                     (puni--pair-or-in-delim-p (1- pt) end)))
      (_ (error "Invalid DIRECTION")))))

(defun puni--strict-primitive-forward-sexp ()
  "Move forward a sexp, return the point if success, otherwise return nil.
If there's a balanced sexp in front, but jumping over it will
move us to a different depth in the whole syntax tree, this won't
go over it.

Notice this doesn't work well in strings, as the built-in
`forward-sexp' thinks the closing quote of this string, and the
opening quote of the next one, forms a string.  It also doesn't
work well for balanced comment blocks.  So we'll build on top of
this function until we have `puni-strict-forward-sexp', which
works on these situations."
  (let* (beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp
             (unhandled-branch-handler
              (lambda ()
                (if puni--debug
                    (error "You've found an unhandled branch in Puni.
Direction: forward
beg: %s, end: %s, another-beg: %s, another-end: %s
Please report relevant part of the buffer, with the location of these points"
                           beg end
                           beg-of-maybe-another-sexp
                           end-of-maybe-another-sexp)
                  (setq end nil))))
             (skipped-part-handler
              (lambda ()
                (setq end (save-excursion
                            (goto-char beg)
                            (puni--forward-syntax-block)))))
             (inside-sexp-handler
              (lambda ()
                (if (puni--inside-delim-p beg beg-of-maybe-another-sexp end
                                          'forward)
                    (setq end nil)
                  (setq end (save-excursion (puni--forward-syntax-block)))))))
    (save-excursion
      (setq beg (point))
      (setq end (puni--primitive-forward-sexp))
      (setq beg-of-maybe-another-sexp (puni--primitive-backward-sexp))
      (setq end-of-maybe-another-sexp (puni--primitive-forward-sexp)))
    ;; Make sure we can actually go forward a sexp.  This also incidentally
    ;; checks if the point is at the end of buffer.  Notice that
    ;; beg/end-of-maybe-another-sexp shouldn't be nil if end is non-nil, unless
    ;; we are using a `forward-sexp-function' that really doesn't work.
    (when (and beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp)
      (cond
       ((< beg-of-maybe-another-sexp beg)
        (cond
         ;; Try:
         ;;
         ;;     foo bar |.        (forward -> backward -> forward sexp)
         ;;
         ;; The cause is "." is completely ignored when searching for the bound
         ;; of a sexp.  This is seen more clear when there are other words and
         ;; puncts after ".".  If this happens, we consider the syntax block at
         ;; the beginning of the ignored part a sexp.
         ((<= end-of-maybe-another-sexp beg)
          (funcall skipped-part-handler))
         ;; Shouldn't happen.
         ((< beg end-of-maybe-another-sexp end)
          (funcall unhandled-branch-handler))
         ;; This means the part between beg-of-maybe-another-sexp and end is a
         ;; sexp.  e.g.:
         ;;
         ;;     <p|>something</p>
         ;;
         ;; or
         ;;
         ;;     <p>something|</p>
         ((>= end-of-maybe-another-sexp end)
          (funcall inside-sexp-handler))))
       ;; This means there's a sexp between BEG and END.  That's perfect, we
       ;; don't need to do anything more.
       ((eq beg-of-maybe-another-sexp beg) nil)
       ;; (> beg-of-maybe-another-sexp beg).  e.g.,
       ;;
       ;;     bar|. foo
       (t
        (funcall skipped-part-handler)))
      (when end (goto-char end)))))

(defun puni--strict-primitive-backward-sexp ()
  "Backward version of `puni--strict-primitive-forward-sexp'."
  (let* (beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp
             (unhandled-branch-handler
              (lambda ()
                (if puni--debug
                    (error "You've found an unhandled branch in Puni.
Direction: backward
beg: %s, end: %s, another-beg: %s, another-end: %s
Please report relevant part of the buffer, with the location of these points"
                           beg end
                           beg-of-maybe-another-sexp
                           end-of-maybe-another-sexp)
                  (setq beg nil))))
             (skipped-part-handler
              (lambda ()
                (setq beg (save-excursion
                            (goto-char end)
                            (puni--backward-syntax-block)))))
             (inside-sexp-handler
              (lambda ()
                (if (puni--inside-delim-p end beg end-of-maybe-another-sexp
                                          'backward)
                    (setq beg nil)
                  (setq beg (save-excursion (puni--backward-syntax-block)))))))
    (save-excursion
      (setq end (point))
      (setq beg (puni--primitive-backward-sexp))
      (setq end-of-maybe-another-sexp (puni--primitive-forward-sexp))
      (setq beg-of-maybe-another-sexp (puni--primitive-backward-sexp)))
    (when (and beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp)
      (cond
       ((> end-of-maybe-another-sexp end)
        (cond
         ((>= beg-of-maybe-another-sexp end)
          (funcall skipped-part-handler))
         ((> end beg-of-maybe-another-sexp beg)
          (funcall unhandled-branch-handler))
         ((<= beg-of-maybe-another-sexp end)
          (funcall inside-sexp-handler))))
       ((eq end-of-maybe-another-sexp end) nil)
       (t
        (funcall skipped-part-handler)))
      (when beg (goto-char beg)))))

(defun puni--strict-primitive-forward-sexp-in-thing (probe thing)
  "Move strict forward a sexp in certain thing.
PROBE is a function that should return non-nil when the point is
in that thing, and nil when it's not.

Return the point after move.  When we can't move forward, return
nil.

When the point is not in the construct in the first place, throw
and error \"Not in a THING\"."
  (when (not (funcall probe))
    (error "Not in a %s" thing))
  (let ((to (save-excursion (puni--strict-primitive-forward-sexp)))
        pos)
    (when to
      (save-excursion
        (while (and (puni--forward-same-syntax to)
                    (funcall probe)
                    (setq pos (point))
                    (< (point) to))))
      ;; We've successfully reached TO, while keeping inside the thing.
      (if (eq pos to)
          (goto-char to)
        ;; If that's not the case, that means we jumped out of the thing by
        ;; `forward-sexp'. This happens in strings with only puncts and blanks
        ;; in it.  When this happens, we go forward one syntax block while
        ;; keeping in the thing.
        (let (goal)
          (save-excursion
            (puni--forward-syntax-block)
            (when (funcall probe)
              (setq goal (point))))
          (when goal (goto-char goal)))))))

(defun puni--strict-primitive-backward-sexp-in-thing (probe thing)
  "Backward version of `puni--strict-primitive-forward-sexp-in-thing'."
  (when (not (funcall probe))
    (error "Not in a %s" thing))
  (let ((to (save-excursion (puni--strict-primitive-backward-sexp)))
        pos)
    (when to
      (save-excursion
        (while (and (puni--backward-same-syntax to)
                    (funcall probe)
                    (setq pos (point))
                    (> (point) to))))
      (if (eq pos to)
          (goto-char to)
        (let (goal)
          (save-excursion
            (puni--backward-syntax-block)
            (when (funcall probe)
              (setq goal (point))))
          (when goal (goto-char goal)))))))

(defun puni-strict-forward-sexp-in-string ()
  "Move strict forward a sexp in when point is in string.
The default `(forward-sexp)' thinks the end quote of a string and
a beginning quote of the next string wraps a sexp.  This fixed
that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil."
  (puni--strict-primitive-forward-sexp-in-thing #'puni--in-string-p "string"))

(defun puni-strict-backward-sexp-in-string ()
  "Backward version of `puni-strict-forward-sexp-in-string'."
  (puni--strict-primitive-backward-sexp-in-thing #'puni--in-string-p "string"))

(defun puni-strict-forward-sexp-in-comment ()
  "Move strict forward a sexp in when point is in a comment.
The default `(forward-sexp)' goes to the end of the sexp after
the end quote of the comment block.  This fixed that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil.

Notice that a point inside the (multichar) quote is not
considered as in the comment."
  (puni--strict-primitive-forward-sexp-in-thing #'puni--in-comment-p
                                                "comment"))

(defun puni-strict-backward-sexp-in-comment ()
  "Backward version of `puni-strict-forward-sexp-in-comment'."
  (let (to)
    (or
     (puni--strict-primitive-backward-sexp-in-thing #'puni--in-comment-p
                                                    "comment")
     ;; If moving backward a sexp takes us out of the comment, but we reach the
     ;; beginning of a single line comment, we accept it as it's common to
     ;; delete the opening delimiters of a single line comment.  Also notice
     ;; that the last form has already confirmed we are actually in a comment.
     (progn
       (save-excursion
         (puni--strict-primitive-backward-sexp)
         (when (puni--begin-of-single-line-comment-p)
           (setq to (point))))
       (when to (goto-char to))))))

;;;; APIs

;;;;; API: Strict forward/backward sexp functions

(defun puni-strict-forward-sexp (&optional skip-single-line-comments)
  "Move strict forward a sexp, including a whole comment block.
Return the point if success, otherwise return nil.

If SKIP-SINGLE-LINE-COMMENTS is non-nil, consider a series of
consecutive single-line comments as a comment block.  Otherwise
move one comment line a time."
  (let ((from (point)))
    (puni--forward-blanks)
    (cond
     ;; `puni--in-comment-p' doesn't consert a point inside the
     ;; (multichar) comment quote as in the comment, but this is fine as
     ;; when a user is deleting things with the point at there, they
     ;; probably want to break the balanced comment quotes.
     ((puni--in-comment-p) (puni-strict-forward-sexp-in-comment))
     ((puni--in-string-p) (puni-strict-forward-sexp-in-string))
     (t (or (when skip-single-line-comments
              (puni--forward-consecutive-single-line-comments))
            (puni--forward-comment-block)
            (puni--strict-primitive-forward-sexp))))
    (let ((to (point)))
      (unless (eq from to) to))))

(defun puni-strict-backward-sexp (&optional skip-single-line-comments)
  "Backward version of `puni-strict-forward-sexp'."
  (let ((from (point)))
    ;; If we `puni--backward-blanks' first, we can't tell if the point is after
    ;; the end of a single line comment, as it takes us before the ending
    ;; newline char of the comment, which is the closing comment delimiter.
    (if (progn
          (puni--backward-syntax " ")
          (puni--end-of-single-line-comment-p))
        (if skip-single-line-comments
            (puni--backward-consecutive-single-line-comments)
          (puni--backward-comment-block))
      (puni--backward-blanks)
      (cond
       ((puni--in-comment-p) (puni-strict-backward-sexp-in-comment))
       ((puni--in-string-p) (puni-strict-backward-sexp-in-string))
       (t (or (when skip-single-line-comments
                (puni--backward-consecutive-single-line-comments))
              (puni--backward-comment-block)
              (puni--strict-primitive-backward-sexp)))))
    (let ((to (point)))
      (unless (eq from to) to))))

(defun puni-strict-beginning-of-sexp ()
  "Go to the beginning of the sexp around point.
This means after the opening delimiter.

Return the point if it's moved."
  (let (moved)
    (while (puni-strict-backward-sexp)
      (setq moved t))
    (when moved (point))))

(defun puni-strict-end-of-sexp ()
  "Backward version of `puni-strict-beginning-of-sexp'."
  (let (moved)
    (while (puni-strict-forward-sexp)
      (setq moved t))
    (when moved (point))))

(defun puni-up-list (&optional backward)
  "Move forward out of the sexp around point.
When BACKWARD is non-nil, move backward.

Return the point if the move succeeded."
  (let ((from (point))
        (beg (save-excursion (or (puni-strict-beginning-of-sexp)
                                 (point))))
        (backward-char-with-spaces
         (lambda ()
           (puni--backward-syntax " ")
           (condition-case _
               (progn (forward-char -1) (point))
             (error nil))))
        end done err)
    (save-excursion
      (while (and (not done) (not err))
        (goto-char beg)
        (setq beg (funcall backward-char-with-spaces))
        (if beg
            (progn
              (setq end (puni-strict-forward-sexp))
              (when (and end (> end from)) (setq done t)))
          (setq err t))))
    (when (and done (not err))
      (goto-char (if backward beg end)))))

;;;;; API: Balance test

(defun puni-region-balance-p (pt1 pt2 &optional strict)
  "Return t when the region from PT1 to PT2 is balanced.
When STRICT is nil, this is tolerant to unbalanced symbol
delimeters like \"if..end if\", \"begin..end\", \"def\", as it's
common to delete part of such a block and then rewrite it.  When
STRICT is non-nil, scan the expressions strictly and don't treat
symbol delimiters differently."
  (cl-block nil
    (when (eq pt1 pt2)
      (cl-return t))
    (save-excursion
      (let ((beg (min pt1 pt2))
            (end (max pt1 pt2)))
        (goto-char beg)
        (while (< (point) end)
          (if (or (unless strict (puni--forward-symbol end))
                  (puni--forward-blanks end)
                  (puni--forward-string)
                  (puni-strict-forward-sexp))
              (when (eq (point) end)
                (cl-return t))
            (cl-return nil)))
        ;; Now we have (> (point) end).  This means the depth at END >= the
        ;; depth at BEG.  If we could also prove that the depth at BEG >= the
        ;; depth at END, we know the region between BEG and END is balanced.
        ;; We do that by go backward from END to BEG.
        (goto-char end)
        (while (> (point) beg)
          ;; We don't need to go back a string or sexp, becuase if END is after
          ;; one (including the situations where there are some blanks between
          ;; END the end of the string/sexp), We've already returned while
          ;; going forward.
          (unless (or (unless strict (puni--backward-symbol beg))
                      (puni--backward-blanks beg))
            (cl-return nil)))
        ;; If we've reached here, we have (<= (point) beg)
        (cl-return t)))))

;;;;; API: Deletion

(defun puni-delete-region (pt1 pt2 &optional kill)
  "Delete the region between PT1 and PT2.
When KILL is non-nil, also save it to kill ring.

Return t if success."
  ;; `kill-region' and `delete-region' signal errors when they fail.
  (if kill
      (kill-region pt1 pt2)
    (delete-region pt1 pt2))
  t)

(defun puni-delete-region-keep-balanced (pt1 pt2 &optional strict kill)
  "Delete the region between PT1 and PT2 if it's balanced.
Return t if success.  When deleting it causes unbalanced state,
don't delete it.

When KILL is non-nil, also save the deleted part to the kill
ring.

This is tolerant to deleting symbol delimiters, unless STRICT is
non-nil, see the explanation in `puni-region-balance-p'."
  (if (puni-region-balance-p pt1 pt2 strict)
      (progn (if kill (kill-region pt1 pt2)
               (delete-region pt1 pt2))
             t)))

(defun puni-soft-delete
    (from to &optional strict-sexp style kill fail-action)
  "Soft delete from point FROM to TO.
If STRICT-SEXP is nil, symbol delimiters like \"if..end if\",
\"begin..end\" and \"def\" are considered as balanced
expressions, and can be delete safely, as it's common to delete
part of such a block and then rewrite it.  If STRICT-SEXP is
non-nil, only consider syntactically balanced expressions as
balanced.

STYLE can be:

- `precise' or nil: Delete the region from FROM to TO if it's
  balanced.
- `within': Delete sexps until no more to delete, or we've
  reached a position at or before TO, and going over one more
  sexp will cause point to go beyond TO.
- `beyond': Delete at least one sexps (can be empty lines) until
  no more to delete, or we've reached a position beyond TO.

A series of consecutive whitespaces is considered a sexp.

When KILL is non-nil, save the deleted part to the kill ring.

When something is deleted, this returns non-nil; when nothing is
deleted, we say the deletion fails.  FAIL-ACTION specifies the
action after failure.  It can be:

- nil: Do nothing and return nil.
- `delete-one': Delete 1 sexp, if there is one.
- `jump': Jump to point TO, and return nil.
- `jump-and-reverse-delete': Jump to point TO, and try soft
  delete from TO to FROM, with the same arguments, but STYLE
  being `within'."
  (setq style (or style 'precise))
  (unless (eq from to)
    (let* ((forward (< from to))
           (move-symbol (if forward
                            #'puni--forward-symbol #'puni--backward-symbol))
           (move-blanks (if forward
                            #'puni--forward-blanks #'puni--backward-blanks))
           (move-sexp (if forward
                          #'puni-strict-forward-sexp
                        #'puni-strict-backward-sexp))
           (move (lambda ()
                   (or (unless strict-sexp
                         (funcall move-symbol
                                  (when (eq style 'precise) to)))
                       (funcall move-blanks to)
                       (funcall move-sexp))))
           (within-p (if forward
                         (lambda () (< (point) to))
                       (lambda () (> (point) to))))
           (beyond-goal (lambda ()
                          (let ((goal (save-excursion
                                        (goto-char from)
                                        (while (and (funcall within-p)
                                                    (funcall move)))
                                        (point))))
                            (when (and goal (not (eq from goal))) goal))))
           (within-goal (lambda ()
                          (let* (prev-goal
                                 (goal
                                  (save-excursion
                                    (goto-char from)
                                    (while (and (funcall within-p)
                                                (setq prev-goal (point))
                                                (funcall move)))
                                    prev-goal)))
                            (when (and goal (not (eq from goal)))
                              goal))))
           (fail-act (lambda ()
                       (pcase fail-action
                         ('nil nil)
                         ('delete-one (save-excursion
                                        (goto-char from)
                                        (funcall move)
                                        (let ((pt (point)))
                                          (unless (eq from pt)
                                            (puni-delete-region
                                             from pt kill)))))
                         ('jump (goto-char to) nil)
                         ('jump-and-reverse-delete
                          (goto-char to)
                          (puni-soft-delete
                           to from strict-sexp 'within kill nil))
                         (_ (error "Invalid FAIL-ACTION"))))))
      (or (pcase style
            ('beyond (when-let ((goal (funcall beyond-goal)))
                       (puni-delete-region from goal kill)))
            ('within (when-let ((goal (funcall within-goal)))
                       (puni-delete-region from goal kill)))
            ('precise (puni-delete-region-keep-balanced
                       from to strict-sexp kill)))
          (funcall fail-act)))))

(defun puni-soft-delete-by-move
    (func &optional strict-sexp style kill fail-action)
  "Soft delete between point and the position after calling FUNC.
This calls `puni-soft-delete' internally, see its docstring for
the meaning of STRICT-SEXP, STYLE, KILL and FAIL-ACTION."
  (let ((pt (point))
        (goal (save-excursion (funcall func) (point))))
    (puni-soft-delete pt goal strict-sexp style kill fail-action)))

;;;;; API: misc

(defun puni-reindent-line ()
  "Reindent current line.
This calls `indent-line-function' internally.  However, if it
can't decide the exact column to indent to, and cycle through
possible indent offsets, this does nothing."
  ;; `indent-for-tab-command' and some of the functions it calls checks if
  ;; `this-command' equals to `last-command', and the "cycle through possible
  ;; offsets" behavior may only be triggered if this is true.
  (let* ((this-command 'indent-for-tab-command)
         (last-command 'indent-for-tab-command)
         (bol (line-beginning-position))
         (orig-indent-pt (save-excursion (back-to-indentation)
                                         (point)))
         (orig-pt (point))
         (orig-spaces (buffer-substring bol orig-indent-pt))
         (new-indent-pt (lambda () (progn
                                     (indent-according-to-mode)
                                     (save-excursion
                                       (back-to-indentation)
                                       (point)))))
         (1st-indent-pt (funcall new-indent-pt))
         (2nd-indent-pt (funcall new-indent-pt)))
    (unless (eq 1st-indent-pt 2nd-indent-pt)
      (delete-region bol 2nd-indent-pt)
      (save-excursion (goto-char bol)
                      (insert orig-spaces))
      (goto-char orig-pt))))

;;;; Deletion Commands

;;;;; Kill/delete active region

;;;###autoload
(defun puni-delete-active-region ()
  "Delete active region.
When this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (puni-region-balance-p beg end)
                  (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
          (puni-delete-region beg end)))
    (user-error "No active region")))

;;;###autoload
(defun puni-kill-active-region ()
  "Kill active region.
When this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (puni-region-balance-p beg end)
                  (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
          (setq this-command 'kill-region)
          (puni-delete-region beg end 'kill)))
    (user-error "No active region")))

;;;;; Char

;;;###autoload
(defun puni-backward-delete-char (&optional n)
  "Delete char backward while keeping expressions balanced.
With prefix argument N, kill that many chars.  Negative argument
means kill chars forward.

This respects the variable `delete-active-region'."
  (interactive "p")
  (setq n (or n 1))
  (if (and (use-region-p)
           delete-active-region
           (eq n 1))
      (if (eq delete-active-region 'kill)
          (puni-kill-active-region)
        (puni-delete-active-region))
    (if (< n 0) (puni-forward-delete-char (- n))
      (dotimes (_ n)
        (puni-soft-delete-by-move #'backward-char nil nil nil
                                  'jump-and-reverse-delete)))))

;;;###autoload
(defun puni-forward-delete-char (&optional n)
  "Delete char forward while keeping expressions balanced.
With prefix argument N, kill that many chars.  Negative argument
means kill chars backward.

This respects the variable `delete-active-region'."
  (interactive "p")
  (setq n (or n 1))
  (if (and (use-region-p)
           delete-active-region
           (eq n 1))
      (if (eq delete-active-region 'kill)
          (puni-kill-active-region)
        (puni-delete-active-region))
    (if (< n 0) (puni-backward-delete-char (- n))
      (dotimes (_ n)
        (puni-soft-delete-by-move #'forward-char nil nil nil
                                  'jump-and-reverse-delete)))))

;;;;; Word

;;;###autoload
(defun puni-forward-kill-word (&optional n)
  "Kill word forward while keeping expressions balanced.
With prefix argument N, kill that many words.  Negative argument
means kill words backward."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (puni-backward-kill-word (- n))
    (dotimes (_ n)
      (puni-soft-delete-by-move #'forward-word nil nil 'kill
                                'jump-and-reverse-delete))))

;;;###autoload
(defun puni-backward-kill-word (&optional n)
  "Kill word backward while keeping expressions balanced.
With prefix argument N, kill that many words.  Negative argument
means kill words forward."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0)
      (puni-forward-kill-word (- n))
    (dotimes (_ n)
      (puni-soft-delete-by-move #'backward-word nil nil 'kill
                                'jump-and-reverse-delete))))

;;;;; Line

;;;###autoload
(defun puni-kill-line (&optional n)
  "Kill a line forward while keeping expressions balanced.
With prefix argument N, kill that many lines.  Negative argument
means kill lines backward.

This respects the variable `kill-whole-line'."
  (interactive "P")
  (let* ((from (point))
         to)
    (if (and n (< n 0))
        (puni-backward-kill-line (- n))
      (setq to (save-excursion (forward-line (or n 1))
                               (point)))
      (unless (or kill-whole-line
                  ;; This is default behavior of Emacs: When the prefix
                  ;; argument is specified, always kill whole line.
                  (not (null n))
                  ;; This means we started from the end of a line, and the
                  ;; following newline char should be killed.
                  (eq to (1+ from)))
        (setq to (1- to)))
      (and (puni-soft-delete from to 'strict-sexp 'beyond 'kill)
           (puni-reindent-line)))))

;;;###autoload
(defun puni-backward-kill-line (&optional n)
  "Kill a line backward while keeping expressions balanced.
With prefix argument N, kill that many lines.  Negative argument
means kill lines forward.

This respects the variable `kill-whole-line'."
  (interactive "P")
  (let ((indent-pt (save-excursion (back-to-indentation)
                                   (point)))
        (from (point))
        to)
    (if (and n (< n 0))
        (puni-kill-line (- n))
      (unless (eq n 0)
        (setq to (save-excursion (forward-line (if n (- n) -1))
                                 (end-of-line)
                                 (point)))
        (unless (or kill-whole-line
                    (not (null n))
                    (eq to (1- from)))
          (setq to (1+ to)))
        (and (puni-soft-delete from to 'strict-sexp 'beyond 'kill)
             ;; If we are killing from a point inside the indent spaces, to the
             ;; beginning of line, we don't reindent.
             (unless (and (null n)
                          (>= indent-pt from))
               (puni-reindent-line)))))))

;;;;; Force delete

;;;###autoload
(defun puni-force-delete ()
  "Force delete backward char, or the active region.
Can be used to fight with undesired behavior of structural
editing."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (when (not (bobp))
      (delete-region (1- (point)) (point)))))

;;;; Navigation commands

;;;;; Sexp

;;;###autoload
(defun puni-forward-sexp (&optional n)
  "Go forward a sexp.
This is the same as `puni-strict-forward-sexp', except that it
jumps forward consecutive single-line comments.

With prefix argument N, go forward that many sexps.  Negative
argument means go backward."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0) (puni-backward-sexp (- n))
    (dotimes (_ n)
      (puni-strict-forward-sexp 'skip-single-line-comments))))

;;;###autoload
(defun puni-backward-sexp (&optional n)
  "Go backward a sexp.
This is the same as `puni-strict-backward-sexp', except that it
jumps backward consecutive single-line comments.

With prefix argument N, go backward that many sexps.  Negative
argument means go forward."
  (interactive "p")
  (setq n (or n 1))
  (if (< n 0) (puni-forward-sexp (- n))
    (dotimes (_ n)
      (puni-strict-backward-sexp 'skip-single-line-comments))))

;;;###autoload
(defun puni-beginning-of-sexp ()
  "Go to the beginning of current sexp.
This means go to the point after the opening delimiter.  If this
is called from there, then go to the point before the delimiter,
so consecutive calling this can take you all the way across
opening delimiters.

If it goes to the beginning of the buffer (likely to happen when
called by accident in the top scope), set a mark at where we
begin so we can pop back to it."
  (interactive)
  (unless (bobp)
    (let ((from (point)))
      (or (puni-strict-beginning-of-sexp)
          (puni-up-list 'backward))
      (when (bobp)
        (push-mark from)))))

;;;###autoload
(defun puni-end-of-sexp ()
  "Go to the end of current sexp.
This means go to the point before the closing delimiter.  If this
is called from there, then go to the point after the delimiter,
so consecutive calling this can take you all the way across
closing delimiters.

If it goes to the end of the buffer (likely to happen when called
by accident in the top scope), set a mark at where we begin so we
can pop back to it."
  (interactive)
  (unless (eobp)
    (let ((from (point)))
      (or (puni-strict-end-of-sexp)
          (puni-up-list))
      (when (eobp)
        (push-mark from)))))

;;;;; Punctuation

;;;###autoload
(defun puni-syntactic-forward-punct ()
  "Jump to next punctuation syntactically.
This means:

- When you doesn't start in a string or comment, jump over
  strings/comments/symbols.
- When there are consecutive same chars, go to the last one
  unless they have parentheses syntax.

This command is designed to give you a \"syntactical navigating\"
feel."
  (interactive)
  (let ((in-str-or-comment-p (or (puni--in-comment-p) (puni--in-string-p)))
        done)
    (while (and (not done)
                ;; For some reason "：" is not considered as a punctuation.
                (re-search-forward "[[:punct:]]\\|："))
      ;; The syntax table of the major mode is often not suitable for comment &
      ;; strings.
      (if in-str-or-comment-p
          (setq done t)
        ;; When we are not inside string/comment at first place, jump over
        ;; them.
        (when (not (or (puni--in-comment-p)
                       (puni--in-string-p)
                       ;; Don't go inside symbols.
                       (eq (puni--syntax-char-after (1- (point))) ?_)))

          (setq done t))))
    (when-let ((syntax-after (puni--syntax-char-after (1- (point)))))
      (unless (memq syntax-after '(?\( ?\)))
        (when (looking-at (concat (regexp-quote (char-to-string (char-before)))
                                  "*"))
          (goto-char (match-end 0)))))))

;;;###autoload
(defun puni-syntactic-backward-punct ()
  "Jump to previous punctuation syntactically.
This means:

- When you doesn't start in a string or comment, jump over
  strings/comments/symbols.
- When there are consecutive same chars, go to the last one
  unless they have parentheses syntax.

This command is designed to give you a \"syntactical navigating\"
feel."
  (interactive)
  (let ((in-str-or-comment-p (or (puni--in-comment-p) (puni--in-string-p)))
        done)
    (while (and (not done)
                (re-search-backward "[[:punct:]]\\|："))
      (if in-str-or-comment-p
          (setq done t)
        (when (not (or (puni--in-comment-p)
                       (puni--in-string-p)
                       (eq (puni--syntax-char-after) ?_)))
          (setq done t))))
    (when-let ((syntax-after (puni--syntax-char-after)))
      (unless (memq syntax-after '(?\( ?\)))
        (when (looking-back (concat (regexp-quote (char-to-string
                                                   (char-after)))
                                    "*")
                            (line-beginning-position))
          (goto-char (match-beginning 0)))))))

;;;; Puni mode

;;;###autoload
(defvar puni-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'puni-backward-delete-char)
    (define-key map (kbd "C-d") 'puni-forward-delete-char)
    (define-key map (kbd "M-d") 'puni-forward-kill-word)
    (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
    (define-key map (kbd "C-k") 'puni-kill-line)
    (define-key map (kbd "C-S-k") 'puni-backward-kill-line)
    (define-key map (kbd "C-c DEL") 'puni-force-delete)
    (define-key map (kbd "C-w") 'puni-kill-active-region)
    (define-key map (kbd "C-M-f") 'puni-forward-sexp)
    (define-key map (kbd "C-M-b") 'puni-backward-sexp)
    (define-key map (kbd "C-M-a") 'puni-beginning-of-sexp)
    (define-key map (kbd "C-M-e") 'puni-end-of-sexp)
    (define-key map (kbd "M-(") 'puni-syntactic-backward-punct)
    (define-key map (kbd "M-)") 'puni-syntactic-forward-punct)
    map)
  "Keymap used for `puni-structural-editing-mode'.")

;;;###autoload
(progn
  (define-minor-mode puni-mode
    "Enable keybindings for Puni commands."
    :keymap puni-mode-map))

;;;###autoload
(progn
  (define-globalized-minor-mode puni-global-mode
    puni-mode
    (lambda () (puni-mode 1))))

;;;###autoload
(defun puni-disable-puni-mode ()
  "Disable Puni mode in current buffer."
  (puni-mode -1))

(provide 'puni)

;;; puni.el ends here
