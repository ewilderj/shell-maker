;;; markdown-overlays-bench.el --- Benchmark for inline markdown parser  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Usage:
;;   emacs --batch -L . -l markdown-overlays-bench -f markdown-overlays-bench-run
;;
;; Or from a running Emacs:
;;   (load-file "markdown-overlays-bench.el")
;;   (markdown-overlays-bench-run)

;;; Code:

(require 'markdown-overlays-parser)
(require 'markdown-overlays-tables)
(require 'markdown-overlays)

(defvar markdown-overlays-bench-sample
  (concat
   "This has **bold** and *italic* and `code` in a sentence.\n"
   "Visit [Example](https://example.com) and ~~deleted~~ text.\n"
   "More **bold** words and ***bold-italic*** and __underscore bold__.\n"
   "Nested: **bold with *italic* inside** and ~~struck **bold** struck~~.\n"
   "Consecutive: **a** **b** **c** **d** **e** **f**.\n"
   "A [link](https://a.com) then **bold [link](https://b.com)** end.\n")
  "One block of markup-dense text (~368 chars).")

(defun markdown-overlays-bench--time-call (func iterations)
  "Call FUNC for ITERATIONS, return milliseconds per call."
  (garbage-collect)
  (let ((start (float-time)))
    (dotimes (_ iterations)
      (funcall func))
    (let ((elapsed (- (float-time) start)))
      (/ (* 1000.0 elapsed) iterations))))

(defun markdown-overlays-bench-run ()
  "Run the benchmark suite and print results."
  (interactive)
  (let ((iters 10)
        (copies 100)
        (sep (make-string 60 ?-)))
    (message "\n%s" sep)
    (message "Markdown inline parser benchmark")
    (message "  Sample: %d chars × %d copies = %d chars"
             (length markdown-overlays-bench-sample) copies
             (* (length markdown-overlays-bench-sample) copies))
    (message "  Iterations: %d" iters)
    (message "%s" sep)

    (with-temp-buffer
      (dotimes (_ copies) (insert markdown-overlays-bench-sample))
      (let* ((original (buffer-substring-no-properties (point-min) (point-max)))
             (size (length original))
             propertized)

        ;; 1. Propertize only (string→propertized string)
        (let ((ms (markdown-overlays-bench--time-call
                   (lambda () (setq propertized
                                    (markdown-overlays--propertize-inline-markdown original)))
                   iters)))
          (message "  propertize-inline-markdown:  %7.1fms/call" ms))

        ;; 2. Position map (propertized→position vector)
        (setq propertized (markdown-overlays--propertize-inline-markdown original))
        (let ((ms (markdown-overlays-bench--time-call
                   (lambda () (markdown-overlays--compute-position-map original propertized))
                   iters)))
          (message "  compute-position-map:        %7.1fms/call" ms))

        ;; 3. Full apply-inline-overlays (propertize + map + overlays)
        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (remove-overlays)
                     (markdown-overlays--apply-inline-overlays (point-min) (point-max)))
                   iters)))
          (message "  apply-inline-overlays:       %7.1fms/call" ms))

        ;; 4. Full markdown-overlays-put (everything including headers, code blocks)
        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (remove-overlays)
                     (markdown-overlays-put))
                   iters)))
          (message "  markdown-overlays-put:       %7.1fms/call" ms))

        (message "%s" sep)

        ;; Propertize sub-stages
        (message "  Propertize sub-stages:")

        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (markdown-overlays--replace-markup
                      original markdown-overlays--inline-code-regexp
                      '(1) 'font-lock-doc-markup-face))
                   iters)))
          (message "    code spans:                %7.1fms/call" ms))

        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (markdown-overlays--replace-markup
                      original markdown-overlays--bold-italic-regexp
                      '(1) '(:weight bold :slant italic)))
                   iters)))
          (message "    bold-italic:               %7.1fms/call" ms))

        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (markdown-overlays--replace-markup
                      original markdown-overlays--bold-regexp
                      '(1 2) 'bold))
                   iters)))
          (message "    bold:                      %7.1fms/call" ms))

        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (markdown-overlays--replace-markup
                      original markdown-overlays--italic-regexp
                      '(2 3) 'italic t 1))
                   iters)))
          (message "    italic:                    %7.1fms/call" ms))

        (let ((ms (markdown-overlays-bench--time-call
                   (lambda ()
                     (markdown-overlays--replace-markup
                      original markdown-overlays--strikethrough-regexp
                      '(1) '(:strike-through t) t))
                   iters)))
          (message "    strikethrough:             %7.1fms/call" ms))

        (message "%s" sep)
        (message "Done.\n")))))

(provide 'markdown-overlays-bench)

;;; markdown-overlays-bench.el ends here
