;;; markdown-overlays-bench-streaming.el --- Streaming table benchmark  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Simulates agent-shell's streaming pattern: chunks arrive one at a time,
;; `markdown-overlays-put' is called after each.
;;
;; Two scenarios:
;;   1. Pathological — 10 tables × 10 rows × 10 cols (agent-shell#403)
;;   2. Typical — prose with 1 small table (5 rows × 3 cols)
;;
;; Usage:
;;   emacs --batch -L <path-to-lib> \
;;     -l markdown-overlays-bench-streaming \
;;     -f markdown-overlays-bench-streaming-run

;;; Code:

(require 'markdown-overlays)
(require 'markdown-overlays-tables)

;;; --- Chunk generators ---

(defun bench-streaming--make-header (ncols)
  "Generate a markdown table header row with NCOLS columns."
  (concat "| "
          (mapconcat (lambda (i) (format "Col_%d" i))
                     (number-sequence 1 ncols) " | ")
          " |\n"))

(defun bench-streaming--make-separator (ncols)
  "Generate a markdown table separator row with NCOLS columns."
  (concat "|"
          (mapconcat (lambda (_) "------") (number-sequence 1 ncols) "|")
          "|\n"))

(defun bench-streaming--make-data-row (ncols row-num)
  "Generate a data row with NCOLS columns, mixed inline markup at ROW-NUM."
  (concat "| "
          (mapconcat (lambda (c)
                       (pcase (mod c 5)
                         (0 (format "**bold_%d**" row-num))
                         (1 (format "*italic_%d*" c))
                         (2 (format "`code_%d`" row-num))
                         (3 (format "plain_%d_%d" row-num c))
                         (4 (format "~~struck_%d~~" c))))
                     (number-sequence 1 ncols) " | ")
          " |\n"))

(defun bench-streaming--generate-table-chunks (ntables nrows ncols)
  "Return list of chunk strings building NTABLES tables of NROWS × NCOLS."
  (let (chunks)
    (dotimes (t-idx ntables)
      (push (bench-streaming--make-header ncols) chunks)
      (push (bench-streaming--make-separator ncols) chunks)
      (dotimes (r nrows)
        (push (bench-streaming--make-data-row ncols (1+ r)) chunks))
      (push "\n" chunks))
    (nreverse chunks)))

(defconst bench-streaming--prose-lines
  '("Here is the analysis you requested.\n"
    "\n"
    "The **key finding** is that performance degrades linearly with table count.\n"
    "Each `markdown-overlays-put` call re-renders *all* tables from scratch,\n"
    "which means the cost per chunk grows as O(n × tables).\n"
    "\n"
    "See the table below for a summary:\n"
    "\n")
  "Prose lines that precede the table in the typical scenario.")

(defconst bench-streaming--prose-suffix
  '("\n"
    "As you can see, the ~~old approach~~ has been replaced.\n"
    "The new [caching strategy](https://example.com) is significantly faster.\n"
    "\n"
    "### Next steps\n"
    "\n"
    "1. **Commit** the overlay cache changes\n"
    "2. Run the *full* benchmark suite\n"
    "3. Open a `draft` PR for review\n")
  "Prose lines that follow the table in the typical scenario.")

(defun bench-streaming--generate-typical-chunks ()
  "Return chunk list for a typical response: prose + 1 table (5×3) + prose.
Simulates line-by-line streaming."
  (let (chunks)
    (dolist (line bench-streaming--prose-lines)
      (push line chunks))
    (dolist (chunk (bench-streaming--generate-table-chunks 1 5 3))
      (push chunk chunks))
    (dolist (line bench-streaming--prose-suffix)
      (push line chunks))
    (nreverse chunks)))

;;; --- Benchmark runner ---

(defun bench-streaming--run-scenario (name chunks)
  "Run streaming benchmark NAME with CHUNKS, printing results."
  (let* ((nchunks (length chunks))
         (markdown-overlays-prettify-tables t)
         (chunk-times (make-vector nchunks 0.0))
         (gc-count-before (if (fboundp 'gc-count) (gc-count) 0))
         (total-start (float-time))
         (idx 0)
         (sep (make-string 65 ?-)))

    (with-temp-buffer
      (dolist (chunk chunks)
        (goto-char (point-max))
        (insert chunk)
        (let ((t0 (float-time)))
          (markdown-overlays-remove)
          (markdown-overlays-put)
          (aset chunk-times idx (- (float-time) t0)))
        (setq idx (1+ idx)))

      (let* ((total-elapsed (- (float-time) total-start))
             (gc-count-after (if (fboundp 'gc-count) (gc-count) 0))
             (total-ms (* 1000.0 total-elapsed))
             (chunk-ms-list (append chunk-times nil))
             (avg-ms (/ (seq-reduce #'+ chunk-ms-list 0.0) nchunks))
             (max-ms (seq-max (seq-map (lambda (x) (* 1000.0 x)) chunk-ms-list)))
             (min-ms (seq-min (seq-map (lambda (x) (* 1000.0 x)) chunk-ms-list)))
             (p50 (bench-streaming--percentile chunk-ms-list 50))
             (p95 (bench-streaming--percentile chunk-ms-list 95))
             (p99 (bench-streaming--percentile chunk-ms-list 99))
             (final-overlays (length (overlays-in (point-min) (point-max))))
             (final-chars (buffer-size)))

        (message "\n%s" sep)
        (message "  %s" name)
        (message "%s" sep)
        (message "  Chunks: %d  |  Final: %d chars, %d overlays"
                 nchunks final-chars final-overlays)
        (message "  TOTAL time:     %8.1f ms" total-ms)
        (message "  Per-chunk avg:  %8.2f ms" (* 1000.0 avg-ms))
        (message "  Per-chunk min:  %8.2f ms" min-ms)
        (message "  Per-chunk max:  %8.2f ms" max-ms)
        (message "  P50:            %8.2f ms" p50)
        (message "  P95:            %8.2f ms" p95)
        (message "  P99:            %8.2f ms" p99)
        (message "  GCs:            %8d" (- gc-count-after gc-count-before))
        (message "%s" sep)))))

(defun markdown-overlays-bench-streaming-run ()
  "Run all streaming benchmark scenarios."
  (interactive)
  (let ((sep (make-string 65 ?=)))
    (message "\n%s" sep)
    (message "STREAMING TABLE BENCHMARK SUITE")
    (message "%s" sep)

    ;; Scenario 1: Pathological — 10 tables × 10 rows × 10 cols
    (bench-streaming--run-scenario
     "PATHOLOGICAL: 10 tables × 10 rows × 10 cols (130 chunks)"
     (bench-streaming--generate-table-chunks 10 10 10))

    ;; Scenario 2: Typical — prose + 1 small table + prose
    (bench-streaming--run-scenario
     "TYPICAL: prose + 1 table (5 rows × 3 cols) + prose (27 chunks)"
     (bench-streaming--generate-typical-chunks))

    (message "\n%s" sep)
    (message "Done.")
    (message "%s\n" sep)))

(defun bench-streaming--percentile (time-list pct)
  "Return the PCTth percentile from TIME-LIST (seconds), as milliseconds."
  (let* ((sorted (sort (copy-sequence time-list) #'<))
         (n (length sorted))
         (idx (min (1- n) (floor (* n (/ pct 100.0))))))
    (* 1000.0 (nth idx sorted))))

(provide 'markdown-overlays-bench-streaming)

;;; markdown-overlays-bench-streaming.el ends here
