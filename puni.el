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

;; Puni is a package for structured editing.  Its main features are:

;; - A set of customizable soft deletion commands, enabled by `puni-mode'.
;;   Soft deletion means deleting while keeping expressions balanced.
;; - A simple API `puni-soft-delete-by-move', for defining your own soft
;;   deletion commands.
;; - Sexp navigating and manipulating commands.
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
(require 'pulse)
(require 'subr-x)

;;;; User options

(defgroup puni nil
  "Customizable soft deletion."
  :group 'convenience
  :group 'lisp
  :group 'tools
  :prefix "puni-"
  :link '(url-link "https://github.com/AmaiKinono/puni"))

(defcustom puni-blink-region-face nil
  "A symbol of the face used for blinking region.
Nil means use `pulse-highlight-start-face'."
  :type '(choice (const :tag "Default" nil)
                 (symbol :tag "Face"))
  :group 'puni)

(defcustom puni-confirm-when-delete-unbalanced-active-region t
  "Whether deleting unbalanced active regions needs confirmation."
  :type 'boolean
  :group 'puni)

(defcustom puni-blink-for-slurp-barf t
  "Whether blinking the moved delimiter when slurping & barfing."
  :type 'boolean
  :group 'puni)

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

;;;;; Helpers

(defun puni--move-within (action limit)
  "Call ACTION.  Return t if it moves within LIMIT, otherwise return nil.
ACTION is a function that moves the point.  LIMIT should be a
position after point if ACTION moves forward, or a position
before point if ACTION moves backward.

When ACTION moves within LIMIT, return t.  Otherwise go back to
the start and return nil.

When LIMIT is nil, simply call ACTION."
  (if (null limit)
      (funcall action)
    (let ((from (point))
          (to (progn (funcall action) (point))))
      (cond
       ((> to from) (if (<= to limit) t
                      (goto-char from)
                      nil))
       ((< to from) (if (>= to limit) t
                      (goto-char from)
                      nil))
       (t nil)))))

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
        after-quote to)
    (unless (puni--in-string-p)
      (save-excursion
        (when (progn (puni--forward-syntax "\\")
                     (or (puni--forward-syntax "\"")
                         (puni--forward-syntax "|")))
          (setq after-quote (point))
          ;; The default `forward-sexp' could jump over a string.
          ;; `forward-sexp-function' from the major-mode sometimes doesn't,
          ;; when they jump to the end of a further delimiter.
          (let ((forward-sexp-function nil))
            (goto-char from)
            (or (puni--primitive-forward-sexp)
                ;; This happens when there's no closing quote. In this
                ;; situation, it's safe to delete the opening quote.
                (goto-char after-quote)))
          (setq to (point))))
      (when (and to (not (eq from to)))
        (goto-char to)))))

(defun puni--backward-string ()
  "Backward version of `puni--forward-string'."
  (let ((from (point))
        before-quote to)
    (unless (puni--in-string-p)
      (save-excursion
        (when (or (puni--backward-syntax "\"")
                  (puni--backward-syntax "|"))
          (setq before-quote (point))
          (let ((forward-sexp-function nil))
            (goto-char from)
            (or (puni--primitive-backward-sexp)
                (goto-char before-quote)))
          (setq to (point))))
      (when (and to (not (eq from to)))
        (goto-char to)))))

;;;;; Basic move: comment

(defun puni--forward-comment-block ()
  "Jump forward a whole comment block.
Return the point if success."
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
  "Return t if point is at the opening delimiter of a single line comment."
  (save-excursion
    (and (not (puni--forward-blanks))
         (puni--forward-comment-block)
         (or (eq (char-before) ?\n)
             (and (eobp)
                  (not (memq (puni--syntax-char-after (1- (point)))
                             '(?> ?!))))))))

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
                         (unless (eobp) (beginning-of-line))
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
;; `puni-strict-forward/backward-sexp' functions.

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

(defun puni--forward-syntax-block (&optional limit)
  "Move forward a syntax block.
Moving forward the following things are tried in turn:

- a symbol, string, or comment block
- a pair of parentheses (defined by the syntax table)
- a punctuation forward (if there is one)
- chars with the same syntax

Return the point if success, otherwise return nil.

When LIMIT is non-nil, and there are multiple syntax constructs
after point, choose one that ends before LIMIT.  One example is:

    \"|;\"

Emacs thinks there's a comment after the point to the line end.
But when we move inside the string, we want to move forward the
\";\"."
  (let ((syntax-char (puni--syntax-char-after)))
    (or (puni--move-within #'puni--forward-symbol limit)
        (puni--move-within #'puni--forward-string limit)
        (puni--move-within #'puni--forward-comment-block limit)
        (when (memq syntax-char '(?\( ?$))
          (let ((forward-sexp-function nil))
            (puni--move-within #'puni--primitive-forward-sexp limit)))
        (when (eq syntax-char ?.)
          (progn (forward-char) (point)))
        (puni--move-within #'puni--forward-same-char-and-syntax limit))))

(defun puni--backward-syntax-block (&optional limit)
  "Backward version of `puni--forward-syntax-block'."
  (let ((syntax-char (puni--syntax-char-after (1- (point)))))
    (or (puni--move-within #'puni--backward-symbol limit)
        (puni--move-within #'puni--backward-string limit)
        (when (memq syntax-char '(?\) ?$))
          (let ((forward-sexp-function nil))
            (puni--move-within #'puni--primitive-backward-sexp limit)))
        (when (eq syntax-char ?.)
          (progn (forward-char -1) (point)))
        (puni--move-within #'puni--backward-same-char-and-syntax limit))))

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
                (setq end (save-excursion (puni--forward-syntax-block)))))
             (inside-sexp-handler
              (lambda ()
                (if (puni--inside-delim-p beg beg-of-maybe-another-sexp end
                                          'forward)
                    (setq end nil)
                  (setq end (save-excursion (puni--forward-syntax-block))))))
             (no-sexp-forward-handler
              (lambda ()
                (unless (eobp)
                  (let ((after-syntax-block (save-excursion
                                              (puni--forward-syntax-block))))
                    (save-excursion
                      (goto-char after-syntax-block)
                      (when (eq (puni--strict-primitive-backward-sexp) beg)
                        (setq end after-syntax-block))))))))
    (save-excursion
      (setq beg (point))
      (setq end (puni--primitive-forward-sexp))
      (setq beg-of-maybe-another-sexp (puni--primitive-backward-sexp))
      (setq end-of-maybe-another-sexp (puni--primitive-forward-sexp)))
    (cond
     ((null end)
      ;; `forward-sexp' thinks there's no sexp forward, but it can be wrong
      ;; when a punctuation is forward, and there's no sexp after that punct,
      ;; e.g., in `c-mode':
      ;;
      ;;     { foo|; }          // Call `forward-sexp'
      (funcall no-sexp-forward-handler))
     ((or (null beg-of-maybe-another-sexp)
          (null end-of-maybe-another-sexp))
      (funcall unhandled-branch-handler))
     ((< beg-of-maybe-another-sexp beg)
      ;; Try this in `python-mode':
      ;;
      ;;     n|.
      ;;     #        (forward -> backward -> forward sexp)
      ;;
      ;; The cause of this problem is not clear.  What I know is: "." is a
      ;; punctuation, but is part of "n.", which is a sexp. The first jump
      ;; forward ignores the punctuation and jumps to the end of sexp after it.
      ;; The second jump forward jumps over the whole sexp "n.".  If this
      ;; happens, we set the end of sexp at point to be after "s.".
      (when (< beg end-of-maybe-another-sexp end)
        (setq end end-of-maybe-another-sexp))
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
       ;; Shouldn't happen as we've handled it above.
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
       ;;
       ;; We should also consider the situation where BEG is after a
       ;; expression prefix:
       ;;
       ;;     '|()
       ;;
       ;; For this we don't need to anything.
       ((>= end-of-maybe-another-sexp end)
        (unless (save-excursion
                  (goto-char beg-of-maybe-another-sexp)
                  (puni--forward-syntax "'" beg))
          (funcall inside-sexp-handler)))))
     ;; This means there's a sexp between BEG and END.  That's perfect, we
     ;; don't need to do anything more.
     ((eq beg-of-maybe-another-sexp beg) nil)
     ;; (> beg-of-maybe-another-sexp beg).  e.g.,
     ;;
     ;;     bar|. foo
     (t
      (funcall skipped-part-handler)))
    (when end (goto-char end))))

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
        (when (> end beg-of-maybe-another-sexp beg)
          (setq beg beg-of-maybe-another-sexp))
        (cond
         ((>= beg-of-maybe-another-sexp end)
          (funcall skipped-part-handler))
         ((> end beg-of-maybe-another-sexp beg)
          (funcall unhandled-branch-handler))
         ((<= beg-of-maybe-another-sexp end)
          (unless (save-excursion
                    (goto-char beg)
                    (puni--forward-syntax "'" end))
            (funcall inside-sexp-handler)))))
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
        ;; `forward-sexp'.  Try `forward-sexp' in:
        ;;
        ;;     "| " ""
        ;;
        ;; When this happens, we go forward one syntax block while keeping in
        ;; the thing.
        (let (goal)
          (save-excursion
            (puni--forward-syntax-block pos)
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
            (puni--backward-syntax-block pos)
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
  (puni--strict-primitive-backward-sexp-in-thing #'puni--in-comment-p
                                                 "comment"))

;;;;; Indent

(defun puni--indent-line ()
  "Indent current line.
This calls `indent-line-function' internally.  However, if it
can't decide the exact column to indent to, and cycle through
possible indent offsets, this doesn't change the indentation.

In any situation, this tries to restore the cursor to a
reasonable position, and returns the change of indentation (can
be zero)."
  ;; `indent-for-tab-command' and some of the functions it calls checks if
  ;; `this-command' equals to `last-command', and the "cycle through possible
  ;; offsets" behavior may only be triggered if this is true.
  (let* ((this-command 'indent-for-tab-command)
         (last-command 'indent-for-tab-command)
         (bol (line-beginning-position))
         (orig-indentation (current-indentation))
         (orig-col-relative-to-indentation (- (current-column)
                                              orig-indentation))
         (orig-spaces (buffer-substring bol
                                        (save-excursion (back-to-indentation)
                                                        (point))))
         (new-indentation (lambda () (progn
                                       (indent-according-to-mode)
                                       (current-indentation))))
         (1st-indentation (funcall new-indentation))
         (2nd-indentation (funcall new-indentation)))
    (if (eq 1st-indentation 2nd-indentation)
        (progn
          (move-to-column (max 0 (+ 1st-indentation
                                    orig-col-relative-to-indentation)))
          (- 1st-indentation orig-indentation))
      (delete-region bol (save-excursion (back-to-indentation)
                                         (point)))
      (goto-char bol)
      (insert orig-spaces)
      (move-to-column (+ orig-indentation
                         orig-col-relative-to-indentation))
      0)))

(defun puni--column-of-position (pos)
  "Column of position POS."
  (save-excursion (goto-char pos)
                  (current-column)))

(defun puni--reindent-region (beg end original-column &optional no-recalculate)
  "Reindent region between BEG and END.
Notice this doesn't reindent the region line-by-line like
`indent-region'.  Rather, it assumes the region was originally
properly indented, and the column at its beginning was
ORIGINAL-COLUMN.  Then, this column changed, because the text
before it changed, or the whole region was inserted at BEG.  This
function then adjust the indentation of the lines in region, to
keep their relative indentation unchanged.

When necessary, the indentation is recalculated by
`puni--indent-line', and the relative indentation of the lines is
still kept unchanged.  The \"necessary\" situations are:

- When the first non-empty line is not the line of BEG.
- When there are only blanks between BEG and the start of its line.

This can be overrided by NO-RECALCULATE.

This function tries to restore the cursor to a reasonable
position (if it's between BEG and END), and returns the change of
indentation of the rest lines in region (can be zero).

It's designed like this to keep manually adjusted indentation by
the user.  When that's desired, this works better than
`indent-region'."
  (let ((orig-col-relative-to-indentation
         (when (<= beg (point) end)
           (- (current-column) (current-indentation))))
        offset)
    ;; Point at the end of region will change when we adjust indentation line
    ;; by line, so we use a marker.
    (setq end (save-excursion (goto-char end)
                              (point-marker)))
    (save-excursion
      (goto-char beg)
      (setq offset (- (current-column) original-column))
      (unless no-recalculate
        (cond
         ((looking-at (rx (* blank) line-end))
          (progn (puni--forward-blanks end)
                 (if (eq (point) end)
                     (setq offset 0)
                   (setq offset (puni--indent-line)))))
         ((looking-back (rx line-start (* blank))
                        (line-beginning-position))
          (setq offset (+ offset (puni--indent-line))))))
      (unless (eq offset 0)
        (while (and (eq (forward-line) 0)
                    (bolp)
                    (< (point) end))
          (unless (puni--line-empty-p)
            (let* ((orig-indentation (current-indentation))
                   (new-indentation (+ orig-indentation offset)))
              (when (wholenump new-indentation)
                (indent-line-to new-indentation)))))))
    (when orig-col-relative-to-indentation
      (move-to-column (max 0 (+ (current-indentation)
                                orig-col-relative-to-indentation))))
    offset))

;;;;; Active region

(defun puni--active-region-direction ()
  "Return the direction of active region.
If the point is at the beginning of it, return `backward',
otherwise return `forward'.  If there's no active region, return
nil."
  (when (use-region-p)
    (if (> (mark) (point))
        'backward
      'forward)))

(defun puni--mark-region (beg end &optional direction replace-mark)
  "Mark and activate a region between BEG and END.
If DIRECTION is `forward', mark at BEG and goto END.  If
DIRECTION is `backward', mark at END and goto BEG.  If it's nil,
and there's an active region, using the direction of that region,
otherwise mark at BEG and goto END.

If REPLACE-MARK is non-nil, replace the existing mark using the
new mark, otherwise push the existing one into mark ring."
  (unless direction
    (setq direction (or (puni--active-region-direction)
                        'forward)))
  (setq deactivate-mark nil)
  (pcase direction
    ('forward (if replace-mark (set-mark beg) (push-mark beg 'nomsg))
              (goto-char end)
              (activate-mark))
    ('backward (if replace-mark (set-mark end) (push-mark end 'nomsg))
               (goto-char beg)
               (activate-mark))
    (_ (error "Invalid DIRECTION"))))

;;;;; Misc

(defun puni--interval-contain-p (i1 i2)
  "Check if I2 is inside I1.
I1 and I2 are cons pairs of integers.  We say I2 is inside I1
when it's a true subset of I1.

If I1 is nil, return nil."
  (when i1
    (and (<= (car i1) (car i2) (cdr i2) (cdr i1))
         (not (and (eq (car i1) (car i2))
                   (eq (cdr i1) (cdr i2)))))))

(defun puni--bigger-interval (i1 i2)
  "Return the one that contains the other one in I1 and I2.
I1 and I2 are cons pairs of integers.

When I1 or I2 is nil, or neither of them contains the other one,
return nil."
  (when (and i1 i2)
    (cond
     ((and (<= (car i1) (car i2) (cdr i2) (cdr i1))) i1)
     ((and (<= (car i2) (car i1) (cdr i1) (cdr i2))) i2))))

(defun puni--smaller-interval (i1 i2)
  "Return the one that is contained by the other one in I1 and I2.
I1 and I2 are cons pairs of integers.

When I1 or I2 is nil, or neither of them contains the other one,
return nil."
  (when (and i1 i2)
    (cond
     ((and (<= (car i1) (car i2) (cdr i2) (cdr i1))) i2)
     ((and (<= (car i2) (car i1) (cdr i1) (cdr i2))) i1))))

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
     ;; `puni--in-comment-p' doesn't consider a point inside the
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
    (if (progn (puni--backward-syntax " ")
               (puni--end-of-single-line-comment-p))
        (if skip-single-line-comments
            (puni--backward-consecutive-single-line-comments)
          (puni--backward-comment-block))
      (puni--backward-blanks)
      (cond
       ((puni--in-comment-p) (puni-strict-backward-sexp-in-comment))
       ((puni--in-string-p) (puni-strict-backward-sexp-in-string))
       (t (or (puni--backward-comment-block)
              (puni--strict-primitive-backward-sexp)))))
    (let ((to (point)))
      (unless (eq from to) to))))

(defun puni-strict-backward-sexp-or-single-line-comment-quotes ()
  "Move backward a sexp or open quotes of single line comment.
Return the point if success, otherwise return nil."
  (or (puni-strict-backward-sexp)
      (when (and (not (bobp))
                 (save-excursion
                   (forward-char -1)
                   (puni--begin-of-single-line-comment-p)))
        (puni--backward-same-char))))

(defun puni-beginning-of-list-around-point ()
  "Go to the beginning of the list around point.
Return the point if it's moved."
  (let (moved)
    (while (puni-strict-backward-sexp)
      (setq moved t))
    (when moved (point))))

(defun puni-end-of-list-around-point ()
  "Backward version of `puni-beginning-of-list-around-point'."
  (let (moved)
    (while (puni-strict-forward-sexp)
      (setq moved t))
    (when moved (point))))

(defun puni-up-list (&optional backward)
  "Move forward out of the sexp around point.
When BACKWARD is non-nil, move backward.

Return the point if the move succeeded."
  (when-let ((bounds (puni-bounds-of-sexp-around-point)))
    (if backward
        (goto-char (car bounds))
      (goto-char (cdr bounds)))))

(defun puni-before-sexp-p ()
  "See if point is before a sexp.
If it is, return the end of that sexp, otherwise return nil."
  (save-excursion (puni-strict-forward-sexp)))

(defun puni-after-sexp-p ()
  "See if point is after a sexp.
If it is, return the end of that sexp, otherwise return nil."
  (save-excursion (puni-strict-backward-sexp)))

;;;;; API: Bounds of sexp-related things

(defun puni-bounds-of-sexp-at-point ()
  "Bounds of sexp at or after point.
It's returned as a cons cell.  If there's no sexp at point,
return nil."
  (let ((from (point))
        beg-forward
        end-forward
        beg-backward
        end-backward
        smaller-bounds)
    (save-excursion
      (setq end-forward (puni-strict-forward-sexp)
            beg-forward (puni-strict-backward-sexp)))
    (save-excursion
      (setq beg-backward (puni-strict-backward-sexp)
            end-backward (puni-strict-forward-sexp)))
    (cond
     ;; At the beginning of a sexp.
     ((eq beg-forward from) (cons beg-forward end-forward))
     ;; At the end of a sexp.
     ((eq end-backward from) (cons beg-backward end-backward))
     ;; Inside a sexp.
     ((and (setq smaller-bounds (puni--smaller-interval
                                 (cons beg-forward end-forward)
                                 (cons beg-backward end-backward)))
           (<= (car smaller-bounds) from (cdr smaller-bounds)))
      smaller-bounds)
     ;; Outside a sexp, so we do nothing.
     )))

(defun puni-beginning-pos-of-list-around-point ()
  "Beginning position of list around point."
  ;; We allow `puni-beginning-of-list-around-point' to fail, as if that
  ;; happens, we are at one of the bounds.
  (save-excursion (or (puni-beginning-of-list-around-point)
                      (point))))

(defun puni-end-pos-of-list-around-point ()
  "End position of list around point."
  ;; We allow `puni-end-of-list-around-point' to fail, as if that happens, we
  ;; are at one of the bounds.
  (save-excursion (or (puni-end-of-list-around-point)
                      (point))))

(defun puni-bounds-of-list-around-point ()
  "Bounds of list around point.
It's returned as a cons cell.  If there's no sexp around point,
meaning the point is at the top level scope, positions at the
beginning/end of buffer is returned."
  (when-let ((beg (puni-beginning-pos-of-list-around-point))
             (end (puni-end-pos-of-list-around-point)))
    (cons beg end)))

(defun puni-bounds-of-sexp-around-point ()
  "Bounds of the sexp around point.
It's returned as a cons cell.  If there's no sexp around point,
return nil."
  (let ((from (point))
        (beg (save-excursion (or (puni-beginning-of-list-around-point)
                                 (point))))
        (backward-within-delimiter
         (lambda ()
           (puni--backward-syntax " ")
           (unless (bobp)
             (or (puni--backward-symbol)
                 (forward-char -1))
             (point))))
        end done err)
    (save-excursion
      (while (and (not done) (not err))
        (goto-char beg)
        (setq beg (funcall backward-within-delimiter))
        (if beg
            (progn
              (setq end (puni-strict-forward-sexp))
              (when (and end (> end from)) (setq done t)))
          (setq err t))))
    (when (and done (not err))
      (cons beg end))))

(defun puni-beginning-pos-of-sexp-around-point ()
  "Beginning position of sexp around point.
If there's no sexp around point, return nil."
  (when-let ((bounds (puni-bounds-of-sexp-around-point)))
    (car bounds)))

(defun puni-end-pos-of-sexp-around-point ()
  "End position of sexp around point.
If there's no sexp around point, return nil."
  (when-let ((bounds (puni-bounds-of-sexp-around-point)))
    (cdr bounds)))

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
            (end (max pt1 pt2))
            (end-of-sexp))
        (goto-char beg)
        (while (< (point) end)
          (if (or (unless strict (puni--forward-symbol end))
                  (puni--forward-blanks end)
                  (puni--forward-string)
                  (puni-strict-forward-sexp))
              (when (eq (point) end)
                (cl-return t))
            (cl-return nil)))
        ;; Now we have (> (point) end).  The point jumps over END while moving,
        ;; this means the depth at END >= the depth at BEG.  If we could also
        ;; prove that the depth at BEG >= the depth at END, we know the region
        ;; between BEG and END is balanced.  Notice that BEG and (point) have
        ;; the same depth, so we do this by trying to go from END to (point).
        (setq end-of-sexp (point))
        (goto-char end)
        (while (< (point) end-of-sexp)
          ;; We don't need to go back a string or sexp, becuase if END is after
          ;; one (including the situations where there are some blanks between
          ;; END the end of the string/sexp), We've already returned while
          ;; going forward.
          (if (or (unless strict (puni--forward-symbol end-of-sexp))
                  (puni--forward-blanks end-of-sexp)
                  (puni-strict-forward-sexp))
              (when (eq (point) end-of-sexp)
                (cl-return t))
            (cl-return nil)))))))

(defun puni-dangling-delimiter-p (&optional point)
  "Return t if the char at POINT is a dangling delimiter.
A dangling delimiter is a single char opening/closing delimiter
that doesn't have a matched delimiter.

When POINT is nil, see if the char at point is a dangling
delimiter."
  (let ((point (or point (point))))
    (unless (or (< point (point-min))
                (>= point (point-max)))
      (save-excursion
        (and (progn (goto-char point)
                    (not (save-excursion
                           (puni-strict-forward-sexp))))
             (progn (goto-char (1+ point))
                    (not (save-excursion
                           (puni-strict-backward-sexp)))))))))

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
    (from to &optional strict-sexp style kill fail-action return-region)
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
  being `within'.

When RETURN-REGION is non-nil, don't actually delete the region,
but return its beginning and end position in a cons cell."
  (setq style (or style 'precise))
  (unless (eq from to)
    (let* ((forward (< from to))
           (move-symbol (if forward
                            #'puni--forward-symbol #'puni--backward-symbol))
           (move-blanks (if forward
                            #'puni--forward-blanks #'puni--backward-blanks))
           (move-sexp
            (if forward
                #'puni-strict-forward-sexp
              #'puni-strict-backward-sexp-or-single-line-comment-quotes))
           (move (lambda ()
                   (or (unless strict-sexp
                         (funcall move-symbol
                                  (when (eq style 'precise) to)))
                       (funcall move-blanks to)
                       (funcall move-sexp))))
           (within-p (if forward
                         (lambda () (< (point) to))
                       (lambda () (> (point) to))))
           ;; This is useful when we go forward a single line comment, and
           ;; reach after the newline, but we don't want to delete that newline
           ;; char.
           (fix-blanks (lambda ()
                         (if forward
                             (and (> (point) to) (puni--backward-blanks to))
                           (and (< (point) to) (puni--forward-blanks to)))))
           (beyond-goal (lambda ()
                          (let ((goal (save-excursion
                                        (goto-char from)
                                        (while (and (funcall within-p)
                                                    (funcall move)))
                                        (funcall fix-blanks)
                                        (point))))
                            (when (and goal (not (eq from goal))) goal))))
           (within-goal (lambda ()
                          (let* (prev-goal
                                 (goal
                                  (save-excursion
                                    (goto-char from)
                                    (while (and (funcall within-p)
                                                (setq prev-goal (point))
                                                (when (funcall move)
                                                  (funcall fix-blanks)
                                                  t)))
                                    prev-goal)))
                            (when (and goal (not (eq from goal)))
                              goal))))
           (act-on-region (lambda (from to)
                            (if return-region
                                (cons (min from to) (max from to))
                              (puni-delete-region from to kill))))
           (fail-act (lambda ()
                       (pcase fail-action
                         ('nil nil)
                         ('delete-one (save-excursion
                                        (goto-char from)
                                        (funcall move)
                                        (let ((pt (point)))
                                          (unless (eq from pt)
                                            (funcall act-on-region from pt)))))
                         ('jump (goto-char to) nil)
                         ('jump-and-reverse-delete
                          (goto-char to)
                          (puni-soft-delete
                           to from strict-sexp 'within kill nil return-region))
                         (_ (error "Invalid FAIL-ACTION"))))))
      (or (pcase style
            ('beyond (when-let ((goal (funcall beyond-goal)))
                       (funcall act-on-region from goal)))
            ('within (when-let ((goal (funcall within-goal)))
                       (funcall act-on-region from goal)))
            ('precise (when (puni-region-balance-p from to strict-sexp)
                        (funcall act-on-region from to))))
          (funcall fail-act)))))

(defun puni-soft-delete-by-move
    (func &optional strict-sexp style kill fail-action)
  "Soft delete between point and the position after calling FUNC.
This calls `puni-soft-delete' internally, see its docstring for
the meaning of STRICT-SEXP, STYLE, KILL and FAIL-ACTION."
  (let ((pt (point))
        (goal (save-excursion (funcall func) (point))))
    (puni-soft-delete pt goal strict-sexp style kill fail-action)))

;;;; Deletion Commands

;;;;; Kill/delete active region

;;;###autoload
(defun puni-delete-active-region ()
  "Delete active region.
When this will cause unbalanced state, ask the user to confirm,
unless `puni-confirm-when-delete-unbalanced-active-region' is
nil."
  (interactive)
  (unless (use-region-p) (user-error "No active region"))
  (if (bound-and-true-p rectangle-mark-mode)
      (funcall region-extract-function 'delete-only)
    (let ((beg (region-beginning))
          (end (region-end)))
      (when (or (not puni-confirm-when-delete-unbalanced-active-region)
                (puni-region-balance-p beg end)
                (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
        (puni-delete-region beg end)))))

;;;###autoload
(defun puni-kill-region ()
  "Kill text between point and mark.
When this will cause unbalanced state, ask the user to confirm,
unless `puni-confirm-when-delete-unbalanced-active-region'.

When `rectangle-mark-mode' is enabled, kill the marked
rectangular region instead."
  (interactive)
  (if (bound-and-true-p rectangle-mark-mode)
      ;; There is a rectangular region active.  The user probably
      ;; knows what they are doing, defer to the stock `kill-region'
      ;; function for it to handle the rectangular region.
      (kill-region nil nil 'region)
    (let ((beg (region-beginning))
          (end (region-end)))
      (when (or (not puni-confirm-when-delete-unbalanced-active-region)
                (puni-region-balance-p beg end)
                (y-or-n-p "Delete the region will cause unbalanced state.  \
  Continue? "))
        (setq this-command 'kill-region)
        (puni-delete-region beg end 'kill)))))

;;;###autoload
(defun puni-kill-active-region ()
  "Kill active region.
When this will cause unbalanced state, ask the user to confirm,
unless `puni-confirm-when-delete-unbalanced-active-region' is
nil.

When `rectangle-mark-mode' is enabled, kill the marked
rectangular region instead."
  (interactive)
  (if (use-region-p)
      (puni-kill-region)
    (user-error "No active region")))

;;;;; Char

;;;###autoload
(defun puni-backward-delete-char (&optional n)
  "Delete char backward while keeping expressions balanced.
With prefix argument N, kill that many chars.  Negative argument
means kill chars forward.

Pressing \\[universal-argument] one or more times without
entering a number would force this command to delete 1 char
backward, even if this breaks the balance.

This respects the variable `delete-active-region'."
  (interactive "P")
  ;; If N is a non-empty list, It's typed by one or more C-u.
  (if (and n (listp n))
      (delete-char -1)
    (setq n (prefix-numeric-value n))
    (if (and (use-region-p)
             delete-active-region
             (eq n 1))
        (if (eq delete-active-region 'kill)
            (puni-kill-active-region)
          (puni-delete-active-region))
      (if (< n 0) (puni-forward-delete-char (- n))
        (dotimes (_ n)
          (or
           (puni-soft-delete-by-move #'backward-char)
           ;; Try to delete a dangling delimiter.  We want to handle this
           ;; before the empty sexp case (see below), since if there's a
           ;; dangling delimiter, `puni-bounds-of-sexp-around-point' can be
           ;; laggy.
           (when (puni-dangling-delimiter-p (1- (point)))
             (delete-char -1)
             t)
           ;; Maybe we are inside an empty sexp, so we delete it.
           (unless (or (puni-before-sexp-p)
                       (puni-after-sexp-p))
             (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
               (puni-delete-region (car sexp-bounds) (cdr sexp-bounds))))
           ;; Nothing can be deleted, move backward.
           (forward-char -1)))))))

;;;###autoload
(defun puni-forward-delete-char (&optional n)
  "Delete char forward while keeping expressions balanced.
With prefix argument N, kill that many chars.  Negative argument
means kill chars backward.

Pressing \\[universal-argument] one or more times without
entering a number would force this command to delete 1 char
forward, even if this breaks the balance.

This respects the variable `delete-active-region'."
  (interactive "P")
  (if (and n (listp n))
      (delete-char 1)
    (setq n (prefix-numeric-value n))
    (if (and (use-region-p)
             delete-active-region
             (eq n 1))
        (if (eq delete-active-region 'kill)
            (puni-kill-active-region)
          (puni-delete-active-region))
      (if (< n 0) (puni-backward-delete-char (- n))
        (dotimes (_ n)
          (or (puni-soft-delete-by-move #'forward-char)
              (when (puni-dangling-delimiter-p)
                (delete-char 1)
                t)
              (unless (or (puni-before-sexp-p)
                          (puni-after-sexp-p))
                (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
                  (puni-delete-region (car sexp-bounds) (cdr sexp-bounds))))
              (forward-char 1)))))))

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

;;TODO: kill single line comment without killing newline.
;;;###autoload
(defun puni-kill-line (&optional n)
  "Kill a line forward while keeping expressions balanced.
With prefix argument N, kill that many lines.  Negative argument
means kill lines backward.

This respects the variable `kill-whole-line'."
  (interactive "P")
  (let* ((from (point))
         to col-after-spaces-in-line delete-spaces-to)
    (if (and n (< n 0))
        (puni-backward-kill-line (- n))
      (setq to (save-excursion (forward-line (or n 1))
                               (point)))
      (unless (or kill-whole-line
                  ;; This is default behavior of Emacs: When the prefix
                  ;; argument is specified, always kill whole line.
                  n
                  ;; This means we started from the end of a line, and the
                  ;; following newline char should be killed.
                  (eq to (1+ from))
                  (save-excursion (goto-char to)
                                  (and (eobp) (eolp))))
        (setq to (1- to)))
      (when (looking-at (rx (* blank)))
        (setq col-after-spaces-in-line
              (puni--column-of-position (match-end 0))))
      (puni-soft-delete from to 'strict-sexp 'beyond 'kill)
      (when (and (looking-at (rx (* blank) (not (any blank "\n"))))
                 (setq delete-spaces-to (1- (match-end 0)))
                 (> (puni--column-of-position delete-spaces-to)
                    col-after-spaces-in-line))
        (save-excursion
          (move-to-column col-after-spaces-in-line)
          (puni-delete-region (point) delete-spaces-to 'kill))))))

;;;###autoload
(defun puni-backward-kill-line (&optional n)
  "Kill a line backward while keeping expressions balanced.
With prefix argument N, kill that many lines.  Negative argument
means kill lines forward.

This respects the variable `kill-whole-line'."
  (interactive "P")
  (let ((from (point))
        to)
    (if (and n (< n 0))
        (puni-kill-line (- n))
      (unless (eq n 0)
        (setq to (save-excursion (forward-line (if n (- n) -1))
                                 (unless (bobp) (end-of-line))
                                 (point)))
        (unless (or kill-whole-line
                    n
                    (eq to (1- from))
                    (save-excursion (goto-char to)
                                    (and (bobp) (bolp))))
          (setq to (1+ to)))
        (puni-soft-delete from to 'strict-sexp 'beyond 'kill)))))

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
  (interactive "^p")
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
  (interactive "^p")
  (setq n (or n 1))
  (if (< n 0) (puni-forward-sexp (- n))
    (dotimes (_ n)
      (puni-strict-backward-sexp 'skip-single-line-comments))))

;;;###autoload
(defun puni-forward-sexp-or-up-list (&optional n)
  "Go forward a sexp, or an ending delimiter if there's no sexp forward.
With prefix argument N, do this that many times.  Negative
argument means go backward."
  (interactive "^p")
  (setq n (or n 1))
  (if (< n 0) (puni-backward-sexp-or-up-list (- n))
    (dotimes (_ n)
      (or (puni-strict-forward-sexp 'skip-single-line-comments)
          (puni-up-list)))))

;;;###autoload
(defun puni-backward-sexp-or-up-list (&optional n)
  "Go backward a sexp, or a starting delimiter if there's no sexp backward.
With prefix argument N, do this that many times.  Negative
argument means go forward."
  (interactive "^p")
  (setq n (or n 1))
  (if (< n 0) (puni-forward-sexp-or-up-list (- n))
    (dotimes (_ n)
      (or (puni-strict-backward-sexp 'skip-single-line-comments)
          (puni-up-list 'backward)))))

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
  (interactive "^")
  (unless (bobp)
    (let ((from (point)))
      (or (puni-beginning-of-list-around-point)
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
  (interactive "^")
  (unless (eobp)
    (let ((from (point)))
      (or (puni-end-of-list-around-point)
          (puni-up-list))
      (when (eobp)
        (push-mark from)))))

;;;;; Punctuation

;;;###autoload
(defun puni-syntactic-forward-punct ()
  "Jump to next punctuation syntactically.
This means:

- When the point is outside of strings or comments, jump over
  strings/comments/symbols.
- When there are consecutive same chars, go to the last one
  unless they have parentheses syntax.

This command is designed to give you a \"syntactical navigating\"
feeling."
  (interactive "^")
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

- When the point is outside of strings or comments, jump over
  strings/comments/symbols.
- When there are consecutive same chars, go to the last one
  unless they have parentheses syntax.

This command is designed to give you a \"syntactical navigating\"
feeling."
  (interactive "^")
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

;;;; Selection commands

;;;###autoload
(defun puni-mark-sexp-at-point ()
  "Mark the sexp at or after point."
  (interactive)
  (when-let ((bounds (puni-bounds-of-sexp-at-point)))
    (puni--mark-region (car bounds) (cdr bounds))))

;;;###autoload
(defun puni-mark-list-around-point ()
  "Mark the list around point.
The list around point is the part inside the sexp around point,
i.e., after its opening delimiter, and before its closing
delimiter.  If the point is already at the top scope, then the
whole buffer is the list around point."
  (interactive)
  (when-let ((bounds (puni-bounds-of-list-around-point)))
    (puni--mark-region (car bounds) (cdr bounds))))

;;;###autoload
(defun puni-mark-sexp-around-point ()
  "Mark the sexp around point."
  (interactive)
  (when-let ((bounds (puni-bounds-of-sexp-around-point)))
    (puni--mark-region (car bounds) (cdr bounds))))

;;;###autoload
(defun puni-expand-region ()
  "Expand selected region by semantic units."
  (interactive)
  (let* ((orig-bounds (if (use-region-p)
                          (cons (region-beginning) (region-end))
                        (cons (point) (point))))
         (find-bigger-bounds
          (lambda (func)
            (puni--bigger-interval
             (save-excursion (goto-char (car orig-bounds))
                             (funcall func))
             (save-excursion (goto-char (cdr orig-bounds))
                             (funcall func)))))
         (bounds-of-sexp-at
          (lambda ()
            (funcall find-bigger-bounds #'puni-bounds-of-sexp-at-point)))
         (bounds-of-list-around
          (lambda ()
            (when-let (bounds (funcall find-bigger-bounds
                                       #'puni-bounds-of-list-around-point))
              ;; Don't select blanks around the list.
              (save-excursion
                (goto-char (car bounds))
                (puni--forward-blanks (car orig-bounds))
                (setf (car bounds) (point))
                (goto-char (cdr bounds))
                (puni--backward-blanks (cdr orig-bounds))
                (setf (cdr bounds) (point)))
              bounds)))
         (bounds-of-sexp-around
          (lambda ()
            (funcall find-bigger-bounds #'puni-bounds-of-sexp-around-point)))
         (replace-mark (eq last-command this-command))
         current-bounds)
    (unless
        (cl-dolist (f (list bounds-of-sexp-at bounds-of-list-around
                            bounds-of-sexp-around))
          (setq current-bounds (funcall f))
          (when (puni--interval-contain-p current-bounds orig-bounds)
            (puni--mark-region (car current-bounds) (cdr current-bounds)
                               nil replace-mark)
            (cl-return t)))
      (user-error "Active region is not balanced"))))

;;;; Sexp manipulating commands

;;;;; Internals

(defun puni--strict-forward-sexp-until-line-end ()
  "Move strict forward sexp until there's no sexp forward in the line.
In other words, until there's only blanks between the point and
the line end, or there's no sexp forward.  If that's the case at
the beginning, don't move forward.

Return the point if success, otherwise return nil."
  (let (success-flag)
    (while (and (not (looking-at (rx (* space) "\n")))
                (puni-strict-forward-sexp)
                (setq success-flag t)))
    (when success-flag (point))))

(defun puni--set-undo-position (&optional point)
  "Call this in a command so that undoing the command moves cursor to POINT.
When POINT is nil, the current cursor position is used."
  (when (listp buffer-undo-list)
    (push (or point (point)) buffer-undo-list)))

(defun puni--point-marker (&optional point)
  "Return the marker at POINT.
If POINT is nil, return the marker at point."
  (if point
      (save-excursion (goto-char point)
                      (point-marker))
    (point-marker)))

(defun puni--backward-blanks-till-line-beg ()
  "Move backward whitespaces till the beginning of line.
Return t if success.  If the point is already at the beginning of
line, also return t.  If there are non-blank chars between the
beginning of line and the point, don't move and return nil."
  (when (string-match (rx bol (* space) eos)
                      (buffer-substring (line-beginning-position)
                                        (point)))
    (beginning-of-line)
    t))

(defun puni--forward-blanks-till-line-end ()
  "Move forward whitespaces till the end of line.
Return t if success.  If the point is already at the end of line,
also return t.  If there are non-blank chars between the end of
line and the point, don't move and return nil."
  (when (string-match (rx bos (* space) eol)
                      (buffer-substring (point)
                                        (line-end-position)))
    (end-of-line)
    t))

(defun puni--maybe-blink-region (beg end)
  "Maybe blink the region between BEG and END.
This depends on `puni-blink-for-slurp-barf'."
  (when puni-blink-for-slurp-barf
    (pulse-momentary-highlight-region beg end puni-blink-region-face)))

(defun puni--beg-pos-of-sexps-around-point ()
  "Beginning position of consecutive delimiters after current list."
  (when-let ((pt (puni-beginning-pos-of-sexp-around-point)))
    (while (save-excursion (goto-char pt)
                           (not (puni-after-sexp-p)))
      (setq pt (save-excursion
                 (goto-char pt)
                 (puni-beginning-pos-of-sexp-around-point))))
    pt))

(defun puni--end-pos-of-sexps-around-point ()
  "End position of consecutive delimiters after current list."
  (when-let ((pt (puni-end-pos-of-sexp-around-point)))
    (while (save-excursion (goto-char pt)
                           (not (puni-before-sexp-p)))
      (setq pt (save-excursion
                 (goto-char pt)
                 (puni-end-pos-of-sexp-around-point))))
    pt))

;;;;; Commands

;;;###autoload
(defun puni-squeeze ()
  "Copy the list around point, and delete the sexp around point.
This can be used to \"rewrap\" a sexp.  You could squeeze it
first, type in the new delimiters, and then yank inside them.

When there's an active balanced region, copy it and delete the
sexp around it."
  (interactive)
  (when-let ((bounds-inside
              (if (use-region-p)
                  (let ((beg (region-beginning))
                        (end (region-end)))
                    (unless (puni-region-balance-p beg end 'strict)
                      (user-error "The active region is not balanced"))
                    (cons beg end))
                (puni-bounds-of-list-around-point)))
             (bounds-around (puni-bounds-of-sexp-around-point)))
    (copy-region-as-kill (car bounds-inside) (cdr bounds-inside))
    (puni-delete-region (car bounds-around) (cdr bounds-around))))

;;;###autoload
(defun puni-slurp-forward (&optional n)
  "Move the closing delimiter of sexp around point forward one sexp.
With positive prefix argument N, slurp that many sexps.

This also works for consecutive opening delimiters after current
list, e.g.,

     ((|foo)) bar ;; Call `puni-slurp-backward'
  => ((|foo bar))"
  (interactive "p")
  (setq n (or n 1))
  (when-let* ((end-of-list (puni-end-pos-of-list-around-point))
              ;; If the delimiter begins in its own line, we let the it include
              ;; the blanks and the newline char before it, so keyword
              ;; delimiters like "end" in
              ;;
              ;;     begin
              ;;         ...
              ;;     end
              ;;     ...
              ;;
              ;; could be moved correctly.
              (beg-of-delim (save-excursion
                              (goto-char end-of-list)
                              (and (puni--backward-blanks-till-line-beg)
                                   (backward-char))
                              (point)))
              (end-of-delim (puni--end-pos-of-sexps-around-point))
              (delim-length (- end-of-delim beg-of-delim))
              (delim-length-without-blanks (- end-of-delim end-of-list))
              (reindent-region-beg-column
               (puni--column-of-position end-of-delim))
              (end-of-sexp (save-excursion
                             (goto-char end-of-delim)
                             (cl-dotimes (_ n)
                               (while (or (puni--forward-blanks)
                                          (puni--forward-comment-block)))
                               (or (puni-strict-forward-sexp)
                                   (cl-return)))
                             (when (not (eq (point) end-of-delim))
                               (point))))
              (delim (buffer-substring beg-of-delim end-of-delim)))
    (save-excursion
      (puni--set-undo-position)
      (goto-char end-of-sexp)
      (insert delim)
      (puni-delete-region beg-of-delim end-of-delim)
      ;; Reindent the slurped sexps.
      (puni--reindent-region beg-of-delim (- end-of-sexp delim-length)
                             reindent-region-beg-column)
      (puni--maybe-blink-region
       (point) (- (point) delim-length-without-blanks))
      (setq deactivate-mark nil))))

;;;###autoload
(defun puni-barf-forward (&optional n)
  "Move the closing delimiter of sexp around point backward one sexp.
With positive prefix argument N, barf that many sexps."
  (interactive "p")
  (setq n (or n 1))
  (when-let* ((from (point))
              (end-of-list (puni-end-pos-of-list-around-point))
              (beg-of-delim (save-excursion
                              (goto-char end-of-list)
                              (and (puni--backward-blanks-till-line-beg)
                                   (backward-char))
                              (point)))
              (end-of-delim (puni-end-pos-of-sexp-around-point))
              (delim-length (- end-of-delim beg-of-delim))
              (delim-length-without-blanks (- end-of-delim end-of-list))
              (beg-of-sexp (save-excursion
                             (goto-char beg-of-delim)
                             (cl-dotimes (_ n)
                               (or (puni-strict-backward-sexp)
                                   (cl-return))
                               (while (or (puni--backward-comment-block)
                                          (puni--backward-blanks))))
                             (when (not (eq (point) beg-of-delim))
                               (point))))
              (reindent-region-beg-column
               (puni--column-of-position beg-of-sexp))
              (delim (buffer-substring beg-of-delim end-of-delim)))
    (let (beg-of-moved-delim)
      (save-excursion
        (puni--set-undo-position)
        (puni-delete-region beg-of-delim end-of-delim)
        (goto-char beg-of-sexp)
        (insert delim)
        ;; Reindent the barfed out sexps.
        (puni--reindent-region (+ beg-of-sexp delim-length) end-of-delim
                               reindent-region-beg-column)
        (setq beg-of-moved-delim (- (point) delim-length-without-blanks))
        (puni--maybe-blink-region (point) beg-of-moved-delim)
        (setq deactivate-mark nil))
      (when (>= from beg-of-delim)
        (goto-char beg-of-moved-delim)))))

;;;###autoload
(defun puni-slurp-backward (&optional n)
  "Move the opening delimiter of sexp around point backward one sexp.
With positive prefix argument N, slurp that many sexps.

This also works for consecutive opening delimiters before current
list, e.g.,

     foo ((|bar)) ;; Call `puni-slurp-backward'
  => ((foo |bar))"
  (interactive "p")
  (setq n (or n 1))
  (when-let* ((beg-of-list (puni-beginning-pos-of-list-around-point))
              (end-of-delim (save-excursion
                              (goto-char beg-of-list)
                              (and (puni--forward-blanks-till-line-end)
                                   (forward-char))
                              (point)))
              (beg-of-delim (puni--beg-pos-of-sexps-around-point))
              (delim-length (- end-of-delim beg-of-delim))
              (delim-length-without-blanks (- beg-of-list beg-of-delim))
              (beg-of-sexp (save-excursion
                             (goto-char beg-of-delim)
                             (cl-dotimes (_ n)
                               (while (or (puni--backward-comment-block)
                                          (puni--backward-blanks)))
                               (or (puni-strict-backward-sexp)
                                   (cl-return)))
                             (when (not (eq (point) beg-of-delim))
                               (point))))
              (sexp-beg-column (puni--column-of-position beg-of-sexp))
              (beg-column-after-sexp (puni--column-of-position end-of-delim))
              (delim (buffer-substring beg-of-delim end-of-delim)))
    (let (moved-delim-beg-marker)
      (save-excursion
        (puni--set-undo-position)
        (puni-delete-region beg-of-delim end-of-delim)
        (goto-char beg-of-sexp)
        (insert delim)
        (setq moved-delim-beg-marker (puni--point-marker beg-of-sexp))
        (goto-char end-of-delim)
        ;; Reindent the slurped sexps.
        (puni--reindent-region (+ beg-of-sexp delim-length) (point)
                               sexp-beg-column)
        ;; Reindent the sexps after the slurped sexps, as the indentation may
        ;; be altered by the slurping.
        (when-let ((following-sexps-end
                    (save-excursion
                      (puni--strict-forward-sexp-until-line-end))))
          (puni--reindent-region (point) following-sexps-end
                                 beg-column-after-sexp))
        (puni--maybe-blink-region
         moved-delim-beg-marker
         (+ moved-delim-beg-marker delim-length-without-blanks))
        (setq deactivate-mark nil)))))

;;;###autoload
(defun puni-barf-backward (&optional n)
  "Move the opening delimiter of sexp around point forward one sexp.
With positive prefix argument N, barf that many sexps."
  (interactive "p")
  (setq n (or n 1))
  (when-let* ((from (point))
              (beg-of-list (puni-beginning-pos-of-list-around-point))
              (end-of-delim (save-excursion
                              (goto-char beg-of-list)
                              (and (puni--forward-blanks-till-line-end)
                                   (forward-char))
                              (point)))
              (beg-of-delim (puni-beginning-pos-of-sexp-around-point))
              (delim-length (- end-of-delim beg-of-delim))
              (delim-length-without-blanks (- beg-of-list beg-of-delim))
              (end-of-sexp (save-excursion
                             (goto-char end-of-delim)
                             (cl-dotimes (_ n)
                               (or (puni-strict-forward-sexp)
                                   (cl-return))
                               (while (or (puni--forward-blanks)
                                          (puni--forward-comment-block))))
                             (when (not (eq (point) end-of-delim))
                               (point))))
              (sexp-beg-column
               (puni--column-of-position end-of-delim))
              (beg-column-after-sexp
               (puni--column-of-position end-of-sexp))
              (delim (buffer-substring beg-of-delim end-of-delim)))
    (let (moved-delim-end-marker)
      (save-excursion
        (puni--set-undo-position)
        (goto-char end-of-sexp)
        (insert delim)
        (setq moved-delim-end-marker
              (puni--point-marker (+ end-of-sexp delim-length-without-blanks)))
        (puni-delete-region beg-of-delim end-of-delim)
        (goto-char (- end-of-sexp delim-length))
        ;; Reindent barfed out sexps.
        (puni--reindent-region beg-of-delim (point)
                               sexp-beg-column)
        ;; Reindent the sexps after the inserted delimiter, as the indentation
        ;; may be altered by the barfing.
        (when-let ((following-sexps-end
                    (save-excursion
                      (puni--strict-forward-sexp-until-line-end))))
          (puni--reindent-region (+ (point) delim-length) following-sexps-end
                                 beg-column-after-sexp))
        (puni--maybe-blink-region
         (- moved-delim-end-marker delim-length-without-blanks)
         moved-delim-end-marker)
        (setq deactivate-mark nil))
      (when (<= from end-of-sexp)
        (goto-char moved-delim-end-marker)))))

;;;###autoload
(defun puni-splice ()
  "Remove the delimiters of sexp around point."
  (interactive)
  (when-let* ((bounds-inside (puni-bounds-of-list-around-point))
              (bounds-around (puni-bounds-of-sexp-around-point))
              (beg1 (car bounds-around))
              (end1 (car bounds-inside))
              (beg2 (cdr bounds-inside))
              (end2 (cdr bounds-around))
              (open-delim-length (- end1 beg1))
              (close-delim-length (- end2 beg2)))
    (puni-delete-region beg1 end1)
    (puni-delete-region (- beg2 open-delim-length)
                        (- end2 open-delim-length))
    (pulse-momentary-highlight-region
     beg1 (- end2 open-delim-length close-delim-length)
     puni-blink-region-face)
    (setq deactivate-mark nil)))

;;;###autoload
(defun puni-splice-killing-backward ()
  "Kill all sexps before point in the current list, then splice it.
Splicing is done by removing the delimiters of the list."
  (interactive)
  (puni-soft-delete-by-move
   #'puni-beginning-of-list-around-point nil nil 'kill)
  (puni-splice))

;;;###autoload
(defun puni-splice-killing-forward ()
  "Kill all sexps after point in the current list, then splice it.
Splicing is done by removing the delimiters of the list."
  (interactive)
  (puni-soft-delete-by-move
   #'puni-end-of-list-around-point nil nil 'kill)
  (puni-splice))

;;;###autoload
(defun puni-split ()
  "Split the list around point into two sexps."
  (interactive)
  (when-let* ((from (point))
              (bounds-inside (puni-bounds-of-list-around-point))
              (bounds-around (puni-bounds-of-sexp-around-point))
              (open-delim (buffer-substring (car bounds-around)
                                            (car bounds-inside)))
              (end-delim (buffer-substring (cdr bounds-inside)
                                           (cdr bounds-around)))
              (open-delim-length (length open-delim)))
    (let (beg-of-next-sexp col-of-next-sexp spaces-before end-of-sexp-before)
      (puni--backward-blanks)
      (setq spaces-before (- from (point)))
      (insert end-delim)
      (setq end-of-sexp-before (point))

      (puni--forward-blanks)
      (setq beg-of-next-sexp (point))
      (setq col-of-next-sexp (current-column))
      (insert open-delim)

      (save-excursion
        (goto-char beg-of-next-sexp)
        (when-let ((end-of-next-sexp (puni-strict-forward-sexp)))
          (puni--reindent-region (+ beg-of-next-sexp open-delim-length)
                                 (- end-of-next-sexp (length end-delim))
                                 col-of-next-sexp 'no-recalculate)
          (setq deactivate-mark nil)))
      (goto-char end-of-sexp-before)
      (forward-char spaces-before))))

;;;###autoload
(defun puni-raise ()
  "Replace the sexp around point with sexp at or after point.
If there's an active balanced region, replace the sexp around it
with it."
  (interactive)
  (let ((active-region-direction (puni--active-region-direction)))
    (when-let* ((from (point))
                (bounds-of-this
                 (if active-region-direction
                     (let ((beg (region-beginning))
                           (end (region-end)))
                       (unless (puni-region-balance-p beg end 'strict)
                         (user-error "The active region is not balanced"))
                       (cons beg end))
                   (puni-bounds-of-sexp-at-point)))
                (col-at-beg-of-this (puni--column-of-position
                                     (car bounds-of-this)))
                (bounds-of-parent (puni-bounds-of-sexp-around-point))
                (beg-of-parent (car bounds-of-parent))
                (end-of-parent (cdr bounds-of-parent))
                (sexp (buffer-substring (car bounds-of-this)
                                        (cdr bounds-of-this))))
      (puni-delete-region beg-of-parent end-of-parent)
      (goto-char beg-of-parent)
      (let (end-of-sexp-marker)
        (insert sexp)
        (setq end-of-sexp-marker (point-marker))
        (puni--reindent-region beg-of-parent (point)
                               col-at-beg-of-this 'no-recalculate)
        (if active-region-direction
            (puni--mark-region beg-of-parent end-of-sexp-marker
                               active-region-direction)
          (goto-char (+ beg-of-parent (- from (car bounds-of-this)))))))))

;;;###autoload
(defun puni-transpose ()
  "Swap the sexp before and after point."
  (interactive)
  (let (beg1 end1 beg2 end2 fail-flag)
    (save-excursion
      (puni-strict-backward-sexp)
      (when-let ((bounds (puni-bounds-of-sexp-at-point)))
        (setq beg1 (car bounds)
              end1 (cdr bounds))
        (unless (eq beg1 (point))
          (setq fail-flag t))))
    (save-excursion
      (puni--forward-blanks)
      (when-let ((bounds (puni-bounds-of-sexp-at-point)))
        (setq beg2 (car bounds)
              end2 (cdr bounds))
        (unless (eq beg2 (point))
          (setq fail-flag t))))
    (when (and beg1 end1 beg2 end2 (not fail-flag))
      (let ((sexp1 (buffer-substring beg1 end1))
            (sexp2 (buffer-substring beg2 end2))
            (col1 (puni--column-of-position beg1))
            (col2 (puni--column-of-position beg2))
            (point-pos-in-blanks-between (- (point) end1)))
        (puni-delete-region beg2 end2)
        (goto-char beg2)
        (insert sexp1)
        (puni--reindent-region beg2 (point) col1 'no-recalculate)
        (puni-delete-region beg1 end1)
        (goto-char beg1)
        (insert sexp2)
        (puni--reindent-region beg1 (point) col2 'no-recalculate)
        (forward-char point-pos-in-blanks-between)))))

;;;###autoload
(defun puni-convolute ()
  "Exchange the order of application of two closest outer forms."
  (interactive)
  (when-let ((body-beg (point))
             (body-end (puni-end-pos-of-list-around-point))
             (body-beg-col (current-column))
             (body-end-col (puni--column-of-position body-end))
             (inner-bounds (puni-bounds-of-sexp-around-point))
             (inner-beg (car inner-bounds))
             (inner-end (cdr inner-bounds))
             (inner-beg-col (puni--column-of-position inner-beg))
             (inner-end-col (puni--column-of-position inner-end))
             (outer-bounds (save-excursion
                             (goto-char (car inner-bounds))
                             (puni-bounds-of-sexp-around-point)))
             (outer-beg (car outer-bounds))
             (outer-end (cdr outer-bounds))
             (outer-beg-col (puni--column-of-position outer-beg))
             (outer-end-col (puni--column-of-position outer-end))
             (body (buffer-substring body-beg body-end))
             (inner-open (buffer-substring inner-beg body-beg))
             (inner-close (buffer-substring body-end inner-end))
             (outer-open (buffer-substring outer-beg inner-beg))
             (outer-close (buffer-substring inner-end outer-end)))
    (let* (pt pt-to-restore
              (maybe-move-to-column
               (lambda (col)
                 (let ((col-pos (save-excursion (move-to-column col)
                                                (point)))
                       move)
                   (save-excursion
                     (goto-char (min col-pos pt))
                     (when (puni--forward-blanks (max col-pos pt))
                       (setq move t)))
                   (when move
                     (goto-char col-pos)
                     (setq pt col-pos))))))
      (puni-delete-region outer-beg outer-end)
      (goto-char outer-beg)

      (insert inner-open)
      (puni--reindent-region outer-beg (point) inner-beg-col 'no-recalculate)
      (setq pt (point))
      (funcall maybe-move-to-column inner-beg-col)

      (insert outer-open)
      (puni--reindent-region pt (point) outer-beg-col 'no-recalculate)
      (setq pt (point))
      (funcall maybe-move-to-column body-beg-col)
      (setq pt-to-restore pt)

      (insert body)
      (puni--reindent-region pt (point) body-beg-col 'no-recalculate)
      (setq pt (point))

      (insert outer-close)
      (puni--reindent-region pt (point) inner-end-col 'no-recalculate)
      (setq pt (point))

      (insert inner-close)
      (puni--reindent-region pt (point) body-end-col 'no-recalculate)

      (goto-char pt-to-restore))))

;;; Wrapping

(defun puni--wrap-region (beg end beg-delim end-delim)
  "Wrap region between BEG and END with BEG-DELIM and END-DELIM.
The indentation of the region is adjusted to make it the same
like before wrapping.  BEG and END are integers, not markers."
  (let ((beg (min beg end))
        (end (max beg end)))
    (save-excursion
      (goto-char end)
      (insert end-delim)
      (goto-char beg)
      (insert beg-delim)
      (puni--reindent-region
       (+ beg (length beg-delim))
       (+ end (length beg-delim) (length end-delim))
       (puni--column-of-position beg))))
  (if (< beg end)
      (goto-char (+ beg (length beg-delim)))
    (goto-char end)
    (puni-strict-forward-sexp)
    (goto-char (- (point) (length end-delim)))))

;;;###autoload
(defun puni-wrap-next-sexps (n beg-delim end-delim)
  "Wrap next N S-expressions with BEG-DELIM and END-DELIM.
- If N is a positive integer, wrap N sexps after the point.
- If N is `to-end', wrap sexps from the point to the end of
  current list.
- If N is a negative integer, wrap N sexps before the point.
- If N is `to-beg', wrap sexps from the point to the beginning of
  current list.
- If N is `region', and there's an active region, wrap the region
  instead, otherwise throw an error."
  (puni--set-undo-position)
  (if (eq n 'region)
      (puni--wrap-region (point) (mark) beg-delim end-delim)
    (let* ((end (pcase n
                  ('to-end (puni-end-pos-of-list-around-point))
                  ('to-beg (puni-beginning-pos-of-list-around-point))
                  ((pred numberp)
                   (save-excursion
                     (catch 'end-of-list
                       (dotimes (_ (abs n))
                         (or (if (>= n 0)
                                 (puni-strict-forward-sexp)
                               (puni-strict-backward-sexp))
                             (throw 'end-of-list nil))))
                     (point)))
                  (_ (user-error
                      "Expected 'to-end, 'to-beg, 'region, or integer as N, \
got: %S"
                      n))))
           (beg (save-excursion
                  (if (>= end (point))
                      (puni--forward-blanks)
                    (puni--backward-blanks))
                  (point))))
      (puni--wrap-region beg end beg-delim end-delim))))

(defun puni--parse-interactive-argument-for-wrap (n)
  "Convert N to a value understood by `puni-wrap-next-sexps'."
  (cond ((use-region-p) 'region)
        ((integerp n) n)
        ((consp n) 'to-end)
        (t 1)))

;;;###autoload
(defun puni-wrap-round (&optional n)
  "Wrap the following S-expression with parentheses.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "(" ")"))

;;;###autoload
(defun puni-wrap-square (&optional n)
  "Wrap the following S-expression with square brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "[" "]"))

;;;###autoload
(defun puni-wrap-curly (&optional n)
  "Wrap the following S-expression with curly brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "{" "}"))

;;;###autoload
(defun puni-wrap-angle (&optional n)
  "Wrap the following S-expression with angle brackets.
If a ‘C-u’ prefix argument is given, wrap all S-expressions
following the point until the end of the buffer or of the
enclosing list.  If a numeric prefix argument N is given, wrap N
S-expressions.  Automatically indent the newly wrapped
S-expression."
  (interactive "P")
  (puni-wrap-next-sexps
   (puni--parse-interactive-argument-for-wrap n)
   "<" ">"))

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
    (define-key map (kbd "C-w") 'puni-kill-region)
    (define-key map (kbd "C-M-f") 'puni-forward-sexp)
    (define-key map (kbd "C-M-b") 'puni-backward-sexp)
    (define-key map (kbd "C-M-a") 'puni-beginning-of-sexp)
    (define-key map (kbd "C-M-e") 'puni-end-of-sexp)
    (define-key map (kbd "M-(") 'puni-syntactic-backward-punct)
    (define-key map (kbd "M-)") 'puni-syntactic-forward-punct)
    map)
  "Keymap used for `puni-mode'.")

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
