;;; markdown-overlays.el --- Overlays to prettify Markdown buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
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
;; Setup:
;;
;; (require 'markdown-overlays)
;;
;; Put all supported Markdown overlays with:
;;
;;  (markdown-overlays-put)
;;
;; Remove all supported Markdown overlays with:
;;
;;  (markdown-overlays-remove)

;;; Code:

(require 'map)
(require 'org-faces)
(require 'markdown-overlays-parser)
(require 'markdown-overlays-tables)

(defcustom markdown-overlays-highlight-blocks t
  "Whether or not to highlight source blocks."
  :type 'boolean
  :group 'markdown-overlays)

(defcustom markdown-overlays-insert-dividers nil
  "Whether or not to display a divider between requests and responses."
  :type 'boolean
  :group 'markdown-overlays)

(defcustom markdown-overlays-render-latex nil
  "Whether or not to render LaTeX blocks (experimental).

Experimental.  Please report issues."
  :type 'boolean
  :group 'markdown-overlays)

(defcustom markdown-overlays-language-mapping '(("elisp" . "emacs-lisp")
                                                ("objective-c" . "objc")
                                                ("objectivec" . "objc")
                                                ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '(alist :key-type (string :tag "Language Name/Alias")
                :value-type (string :tag "Mode Name (without -mode)"))
  :group 'markdown-overlays)

(defvar markdown-overlays--source-block-regexp
  (rx  bol (zero-or-more whitespace) (group "```") (zero-or-more whitespace) ;; ```
       (group (zero-or-more (or alphanumeric "-" "+" "#"))) ;; languages like: emacs-lisp C++ C#
       (zero-or-more whitespace)
       (one-or-more "\n")
       (group (*? anychar)) ;; body
       (one-or-more "\n")
       (zero-or-more whitespace)
       (group "```") (or "\n" eol)))

(defun markdown-overlays-remove ()
  "Remove all Markdown overlays."
  (remove-overlays (point-min) (point-max) 'category 'markdown-overlays))

(defun markdown-overlays-put ()
  "Put all Markdown overlays."
  (let* ((source-blocks (markdown-overlays--source-blocks))
         (source-block-ranges (seq-map (lambda (block)
                                  (cons (car (map-elt block 'start))
                                        (cdr (map-elt block 'end))))
                                source-blocks))
         (tables (when (bound-and-true-p markdown-overlays-prettify-tables)
                   (markdown-overlays--find-tables source-block-ranges)))
         (table-ranges (seq-map (lambda (table)
                                  (cons (map-elt table 'start)
                                        (map-elt table 'end)))
                                 tables))
         (block-ranges (sort (append (copy-sequence source-block-ranges)
                                     (copy-sequence table-ranges))
                             (lambda (a b) (< (car a) (car b))))))
    (markdown-overlays-remove)
    (dolist (block source-blocks)
      (markdown-overlays--fontify-source-block
       (car (map-elt block 'start))
       (cdr (map-elt block 'start))
       (buffer-substring-no-properties (car (map-elt block 'language))
                                       (cdr (map-elt block 'language)))
       (car (map-elt block 'language))
       (cdr (map-elt block 'language))
       (car (map-elt block 'body))
       (cdr (map-elt block 'body))
       (car (map-elt block 'end))
       (cdr (map-elt block 'end))))
    (when markdown-overlays-insert-dividers
      (dolist (divider (markdown-overlays--divider-markers))
        (markdown-overlays--fontify-divider (car divider) (cdr divider))))
    (dolist (header (markdown-overlays--markdown-headers block-ranges))
      (markdown-overlays--fontify-header
       (map-elt header 'start)
       (map-elt header 'end)
       (car (map-elt header 'level))
       (cdr (map-elt header 'level))
       (car (map-elt header 'title))
       (cdr (map-elt header 'title))
       (map-elt header 'needs-trailing-newline)))
    ;; Unified inline formatting (bold, italic, code, links, strikethrough).
    ;; Process each region outside source blocks and tables.
    (dolist (range (markdown-overlays--invert-ranges
                    block-ranges (point-min) (point-max)))
      (markdown-overlays--apply-inline-overlays (car range) (cdr range)))
    (markdown-overlays--fontify-tables tables)
    (when markdown-overlays-render-latex
      (require 'org)
      ;; Silence org-element warnings.
      (let ((major-mode 'org-mode))
        (save-excursion
          (dolist (range (markdown-overlays--invert-ranges
                          block-ranges
                          (point-min)
                          (point-max)))
            (org-format-latex
             (concat org-preview-latex-image-directory "markdown-overlays")
             (car range) (cdr range)
             temporary-file-directory
             'overlays nil 'forbuffer org-preview-latex-default-process)))))))

(defun markdown-overlays--match-source-block ()
  "Return a matched source block by the previous search/regexp operation."
  (list
   'start (cons (match-beginning 1)
                (match-end 1))
   'end (cons (match-beginning 4)
              (match-end 4))
   'language (when (and (match-beginning 2)
                        (match-end 2))
               (cons (match-beginning 2)
                     (match-end 2)))
   'body (cons (match-beginning 3) (match-end 3))))

(defun markdown-overlays--source-blocks ()
  "Get a list of all source blocks in buffer."
  (let ((markdown-blocks '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              markdown-overlays--source-block-regexp
              nil t)
        (when-let ((begin (match-beginning 0))
                   (end (match-end 0)))
          (push (markdown-overlays--match-source-block)
                markdown-blocks))))
    (nreverse markdown-blocks)))

(defun markdown-overlays--put (overlay &rest props)
  "Set multiple properties on OVERLAY via PROPS."
  (unless (= (mod (length props) 2) 0)
    (error "Props missing a property or value"))
  (while props
    (overlay-put overlay 'category 'markdown-overlays)
    (overlay-put overlay (pop props) (pop props))))

(defun markdown-overlays--compute-position-map (original propertized)
  "Map each position in PROPERTIZED back to its position in ORIGINAL.
Returns a vector where element i is the original-string position of
propertized-string character i.  Characters removed by markup processing
\(delimiters like ** ` ~~ etc.\) are skipped via a two-pointer walk.

Example: ORIGINAL = \"**bold**\" PROPERTIZED = \"bold\"
  map[0]=2  map[1]=3  map[2]=4  map[3]=5
  positions 0,1,6,7 (the ** delimiters) are unmapped."
  (let ((map (make-vector (length propertized) 0))
        (oi 0)
        (pi 0)
        (olen (length original))
        (plen (length propertized)))
    (while (< pi plen)
      (while (and (< oi olen)
                  (not (eq (aref original oi) (aref propertized pi))))
        (setq oi (1+ oi)))
      (when (< oi olen)
        (aset map pi oi)
        (setq oi (1+ oi)))
      (setq pi (1+ pi)))
    map))

(defun markdown-overlays--apply-inline-overlays (buf-start buf-end)
  "Apply inline markdown overlays between BUF-START and BUF-END.
Uses `markdown-overlays--propertize-inline-markdown' to parse the text,
then creates in-place overlays that hide delimiters and style content.
Text remains navigable — point can move through all visible characters.

For \"**bold**\": invisible overlays hide the ** pairs, a face overlay
applies bold to \"bold\".  The buffer text is unchanged underneath."
  (let* ((original (buffer-substring-no-properties buf-start buf-end))
         (propertized (markdown-overlays--propertize-inline-markdown original))
         (plen (length propertized))
         (olen (length original)))
    (when (< plen olen)
      (let ((pos-map (markdown-overlays--compute-position-map original propertized)))
        ;; 1. Create invisible overlays for deleted characters (delimiters).
        ;; pos-map values are monotonically increasing, so we merge-walk
        ;; against the original positions to find unmapped (delimiter) ranges.
        (let ((mi 0)
              (range-start nil))
          (dotimes (i olen)
            (if (and (< mi plen) (= i (aref pos-map mi)))
                (progn
                  (when range-start
                    (markdown-overlays--put
                     (make-overlay (+ buf-start range-start) (+ buf-start i))
                     'evaporate t 'invisible t)
                    (setq range-start nil))
                  (setq mi (1+ mi)))
              (unless range-start
                (setq range-start i))))
          (when range-start
            (markdown-overlays--put
             (make-overlay (+ buf-start range-start) (+ buf-start olen))
             'evaporate t 'invisible t)))
        ;; 2. Apply text properties from propertized string as overlays
        (let ((pos 0))
          (while (< pos plen)
            (let ((next (or (next-property-change pos propertized) plen))
                  (props (text-properties-at pos propertized)))
              (when props
                (let ((ov (make-overlay
                           (+ buf-start (aref pos-map pos))
                           (+ buf-start (aref pos-map (1- next)) 1))))
                  (overlay-put ov 'category 'markdown-overlays)
                  (overlay-put ov 'evaporate t)
                  (let ((pl (copy-sequence props)))
                    (while pl
                      (overlay-put ov (pop pl) (pop pl))))))
              (setq pos next))))))))

(defun markdown-overlays--resolve-internal-language (language)
  "Resolve external Markdown LANGUAGE to Emacs internal.

For example \"elisp\" -> \"emacs-lisp\"."
  (when language
    (or (map-elt markdown-overlays-language-mapping
                 (downcase (string-trim language)))
        (when (intern (concat (downcase (string-trim language))
                              "-mode"))
          (downcase (string-trim language))))))

(defun markdown-overlays--fontify-source-block (quotes1-start
                                                quotes1-end
                                                lang
                                                lang-start
                                                lang-end
                                                body-start
                                                body-end
                                                quotes2-start
                                                quotes2-end)
  "Fontify a source block.
Use QUOTES1-START QUOTES1-END LANG LANG-START LANG-END BODY-START
 BODY-END QUOTES2-START and QUOTES2-END."
  ;; Overlay beginning "```" with a copy block button.
  (markdown-overlays--put
   (make-overlay quotes1-start quotes1-end)
   'evaporate t
   'display
   (propertize "📋 "
               'pointer 'hand
               'keymap (markdown-overlays--make-ret-binding-map
                        (lambda ()
                          (interactive)
                          (kill-ring-save body-start body-end)
                          (message "Copied")))))
  ;; Hide end "```" altogether.
  (markdown-overlays--put
   (make-overlay quotes2-start quotes2-end)
   'evaporate t
   'invisible 't)
  (unless (eq lang-start lang-end)
    (markdown-overlays--put
     (make-overlay lang-start lang-end)
     'evaporate t
     'face '(:box t))
    (markdown-overlays--put
     (make-overlay lang-end (1+ lang-end))
     'evaporate t
     'display "\n\n"))
  (let ((lang-mode (intern (concat (or
                                    (markdown-overlays--resolve-internal-language lang)
                                    (downcase (string-trim lang)))
                                   "-mode")))
        (string (buffer-substring-no-properties body-start body-end))
        (pos 0)
        (props)
        (overlay)
        (propertized-text))
    (if (and markdown-overlays-highlight-blocks
             (fboundp lang-mode))
        (progn
          (setq propertized-text
                (with-current-buffer
                    (get-buffer-create
                     (format " *markdown-overlays-fontification:%s*" lang-mode))
                  (let ((inhibit-modification-hooks nil)
                        (inhibit-message t))
                    (erase-buffer)
                    ;; Additional space ensures property change.
                    (insert string " ")
                    (funcall lang-mode)
                    (font-lock-ensure))
                  (buffer-string)))
          (while (< pos (length propertized-text))
            (setq props (text-properties-at pos propertized-text))
            (setq overlay (make-overlay (+ body-start pos)
                                        (+ body-start (1+ pos))))
            (markdown-overlays--put
             overlay
             'evaporate t
             'face (plist-get props 'face))
            (setq pos (1+ pos))))
      (markdown-overlays--put
       (make-overlay body-start body-end)
       'evaporate t
       'face 'font-lock-doc-markup-face))))

(defun markdown-overlays--fontify-divider (start end)
  "Display text between START and END as a divider."
  (markdown-overlays--put
   (make-overlay start end)
   'evaporate t
   'display
   (concat (propertize (concat (make-string (window-body-width) ? ) "")
                       'face '(:underline t)) "\n")))
(defun markdown-overlays--markdown-headers (&optional avoid-ranges)
  "Extract markdown headers with AVOID-RANGES."
  (let ((headers '())
        (case-fold-search nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              (rx bol (zero-or-more space) (group (one-or-more "#"))
                  (one-or-more space)
                  (group (one-or-more (not (any "\n")))) eol)
              nil t)
        (if-let ((begin (match-beginning 0))
                 (end (match-end 0))
                 (level-start (match-beginning 1))
                 (level-end (match-end 1))
                 (title-start (match-beginning 2))
                 (title-end (match-end 2)))
            (unless (seq-find (lambda (avoided)
                                (and (>= begin (car avoided))
                                     (<= end (cdr avoided))))
                              avoid-ranges)
              (push
               (list
                'start begin
                'end end
                'level (cons level-start level-end)
                'title (cons title-start title-end)
                'needs-trailing-newline (save-excursion
                                          (goto-char end)
                                          (and (not (eobp))
                                               (not (looking-at-p "\n[ \t]*$"))
                                               (not (looking-at-p "\n\n")))))
               headers))
          (let ((message-log-max t))
            (message "markdown-overlays: Warning: incomplete header match at position %s: %S"
                     (match-beginning 0)
                     (buffer-substring-no-properties
                      (match-beginning 0)
                      (min (+ (match-beginning 0) 50) (point-max))))))))
    (nreverse headers)))
(defun markdown-overlays--fontify-header (_start end level-start level-end title-start title-end &optional needs-trailing-newline)
  "Fontify a markdown header.
Use START END LEVEL-START LEVEL-END TITLE-START TITLE-END and
NEEDS-TRAILING-NEWLINE."
  ;; Hide markup before
  (markdown-overlays--put
   (make-overlay level-start title-start)
   'evaporate t
   'invisible 't)
  ;; Show title as header
  (markdown-overlays--put
   (make-overlay title-start title-end)
   'evaporate t
   'face
   (cond ((eq (- level-end level-start) 1)
          'org-level-1)
         ((eq (- level-end level-start) 2)
          'org-level-2)
         ((eq (- level-end level-start) 3)
          'org-level-3)
         ((eq (- level-end level-start) 4)
          'org-level-4)
         ((eq (- level-end level-start) 5)
          'org-level-5)
         ((eq (- level-end level-start) 6)
          'org-level-6)
         ((eq (- level-end level-start) 7)
          'org-level-7)
         ((eq (- level-end level-start) 8)
          'org-level-8)
         (t
          'org-level-1)))
  (when (and needs-trailing-newline (< end (point-max)))
    (markdown-overlays--put
     (make-overlay (1+ end) (1+ end))
     'evaporate t
     'before-string "\n")))
(defun markdown-overlays--invert-ranges (ranges min max)
  "Invert a list of RANGES within the interval [MIN, MAX].
Each range is a cons of start and end integers."
  (let ((result nil)
        (start min))
    (dolist (range ranges)
      (when (< start (car range))
        (push (cons start (car range)) result))
      (setq start (cdr range)))
    (when (< start max)
      (push (cons start max) result))
    result))

(defun markdown-overlays--make-ret-binding-map (fun)
  "Make (kbd \"RET\") binding map to FUN."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") fun)
    (define-key map [mouse-1] fun)
    (define-key map [remap self-insert-command] 'ignore)
    map))

(defun markdown-overlays--divider-markers ()
  "Return locations of all recognized divider lines."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (rx line-start
                       (* (any " \t"))
                       ;; TODO: Remove shell-maker specific regex.
                       (or "<shell-maker-end-of-prompt>"
                           (or (seq "***" (* "*"))
                               (seq "---" (* "-"))
                               (seq "___" (* "_"))))
                       (* (any " \t"))
                       line-end))
          matches)
      (while (re-search-forward pattern nil t)
        (push (cons (match-beginning 0) (match-end 0)) matches))
      (nreverse matches))))

(defun markdown-overlays-make-local-file-link (filename)
  "Convert FILENAME to a Markdown link.

Returns a string like '[file.txt](file:///absolute/path/to/file.txt)'
if the file exists, nil otherwise."
  (when (file-exists-p filename)
    (let* ((absolute-path (expand-file-name filename))
           (file-uri (concat "file://" absolute-path))
           (basename (file-name-nondirectory absolute-path)))
      (format "[%s](%s)" basename file-uri))))

(defun markdown-overlays-expand-local-links (markdown)
  "Expand file:// links in MARKDOWN to code blocks with file contents.

Transforms links like [file.txt](file:///absolute/path/to/file.txt)
into:

```file.txt
Content of file.txt
```"
  (with-temp-buffer
    (insert markdown)
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](file://\\([^)]+\\))" nil t)
      (let* ((filename (match-string 1))
             (filepath (match-string 2))
             (match-start (match-beginning 0))
             (match-end (match-end 0)))
        (when (file-exists-p filepath)
          (goto-char match-start)
          (delete-region match-start match-end)
          (insert (format "\n```%s\n%s\n```\n"
                          filename (with-temp-buffer
                                     (insert-file-contents filepath)
                                     (buffer-string)))))))
    (buffer-string)))

(provide 'markdown-overlays)

;;; markdown-overlays.el ends here
