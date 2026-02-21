;;; markdown-overlays-parser.el --- Inline markdown parser for markdown-overlays  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Edd Wilder-James https://ewj.me
;; URL: https://github.com/xenodium/shell-maker

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Shared inline markdown parser for `markdown-overlays'.
;;
;; Parses inline markdown syntax (bold, italic, code, links, strikethrough)
;; and returns propertized strings with faces applied and delimiters removed.
;;
;; Used by both:
;; - `markdown-overlays-tables.el' for table cell content (before-string)
;; - `markdown-overlays.el' for buffer text (in-place overlays via position map)

;;; Code:

(require 'seq)

(defun markdown-overlays--apply-face-to-unpropertized (str face)
  "Apply FACE to characters in STR lacking a `face' property.
Characters that already have a `face' property are left untouched."
  (let ((result (copy-sequence str))
        (i 0)
        (len (length str)))
    (while (< i len)
      (unless (get-text-property i 'face result)
        (put-text-property i (1+ i) 'face face result))
      (setq i (1+ i)))
    result))

(defun markdown-overlays--replace-markup (str regex groups face
                                              &optional nestable prefix-group)
  "Replace REGEX matches in STR, applying FACE to captured text.
GROUPS is a list of capture group numbers to try; the first non-nil
match provides the inner text whose delimiters are removed.

When NESTABLE is non-nil, FACE is layered on top of any existing face
using `add-face-text-property' (for italic/strikethrough inside bold).
Otherwise, matches inside already-propertized regions are skipped
entirely (protecting code spans from further processing).

When PREFIX-GROUP is non-nil, that group's text is preserved verbatim
before the styled text (used for italic's lookbehind character)."
  (let ((parts nil)
        (pos 0))
    (while (string-match regex str pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (existing (get-text-property match-start 'face str))
             (protected (and existing
                             (if nestable
                                 (not (memq existing '(bold italic)))
                               t))))
        (if protected
            ;; Inside a protected region — emit verbatim and skip past.
            (let ((prop-end (next-single-property-change
                             match-start 'face str (length str))))
              (push (substring str pos prop-end) parts)
              (setq pos prop-end))
          ;; Extract inner text from first non-nil capture group
          (let* ((inner (seq-some (lambda (g) (match-string g str)) groups))
                 (prefix (if prefix-group (or (match-string prefix-group str) "") ""))
                 (styled (if nestable
                             (let ((s (copy-sequence inner)))
                               (add-face-text-property 0 (length s) face t s)
                               s)
                           (markdown-overlays--apply-face-to-unpropertized
                            inner face))))
            (push (substring str pos match-start) parts)
            (when (> (length prefix) 0)
              (push prefix parts))
            (push styled parts)
            (setq pos match-end)))))
    (push (substring str pos) parts)
    (apply #'concat (nreverse parts))))

(defun markdown-overlays--propertize-inline-markdown (content)
  "Process inline markdown in CONTENT string, return propertized string.
Strips markup delimiters and applies faces for: inline code, links,
bold-italic, bold, italic, and strikethrough.  Used by both table cell
rendering and buffer overlay rendering."
  (let ((result content))
    ;; Process inline code FIRST so its contents are protected from
    ;; bold/italic processing (e.g., `**text**` should render as code).
    (setq result (markdown-overlays--replace-markup
                  result (rx "`" (group (+ (not (any "`")))) "`")
                  '(1) 'font-lock-doc-markup-face))

    ;; Links need special handling for keymap.
    ;; Skip matches inside already-propertized regions (e.g. inline code).
    (let ((link-re (rx "[" (group (+ (not (any "]")))) "]("
                       (group (+ (not (any ")")))) ")"))
          (parts nil)
          (pos 0))
      (while (string-match link-re result pos)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0)))
          (if (get-text-property match-start 'face result)
              (let ((prop-end (next-single-property-change
                               match-start 'face result (length result))))
                (push (substring result pos prop-end) parts)
                (setq pos prop-end))
            (let ((title (match-string 1 result))
                  (url (match-string 2 result))
                  (link-map (make-sparse-keymap)))
              (push (substring result pos match-start) parts)
              (define-key link-map [mouse-1]
                          (lambda () (interactive) (browse-url url)))
              (define-key link-map (kbd "RET")
                          (lambda () (interactive) (browse-url url)))
              (push (propertize title
                                'face 'link
                                'mouse-face 'highlight
                                'keymap link-map
                                'help-echo url)
                    parts)
              (setq pos match-end)))))
      (push (substring result pos) parts)
      (setq result (apply #'concat (nreverse parts))))

    ;; Bold-italic, bold
    (setq result (markdown-overlays--replace-markup
                  result (rx "***" (group (+ (not (any "*")))) "***")
                  '(1) '(:weight bold :slant italic)))
    (setq result (markdown-overlays--replace-markup
                  result (rx (or (seq "**" (group (+? anything)) "**")
                                 (seq "__" (group (+ (not (any "_")))) "__")))
                  '(1 2) 'bold))
    ;; Italic: nestable inside bold, with lookbehind for escaped \*text\*
    (setq result (markdown-overlays--replace-markup
                  result (rx (group (or string-start (not (any "\\"))))
                             (or (seq "*" (group (+ (not (any "*")))) "*")
                                 (seq "_" (group (+ (not (any "_")))) "_")))
                  '(2 3) 'italic t 1))
    ;; Strikethrough: nestable inside bold/italic
    (setq result (markdown-overlays--replace-markup
                  result (rx "~~" (group (+? anything)) "~~")
                  '(1) '(:strike-through t) t))

    result))

(provide 'markdown-overlays-parser)

;;; markdown-overlays-parser.el ends here
