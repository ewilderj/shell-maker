;;; markdown-overlays-blocks-tests.el --- Tests for code block fontification -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'markdown-overlays)

;;; Helpers

(defun markdown-overlays-blocks-tests--face-runs ()
  "Return list of (BEG END FACE) for contiguous `font-lock-face' regions."
  (let ((pos (point-min))
        (end (point-max))
        runs)
    (while (< pos end)
      (let* ((face (get-text-property pos 'font-lock-face))
             (next (or (next-single-property-change pos 'font-lock-face nil end)
                       end)))
        (when (and face (not (equal face '(:box t))))
          (push (list pos next face) runs))
        (setq pos next)))
    (nreverse runs)))

;;; Tests

(ert-deftest markdown-overlays-blocks-test-face-run-spans ()
  "Faces should span contiguous runs, not individual characters."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun foo (x)\n  x)\n```\n")
    (markdown-overlays-put)
    (let ((runs (markdown-overlays-blocks-tests--face-runs)))
      (should (> (length runs) 0))
      ;; The "defun" keyword should be covered by a single contiguous run.
      (goto-char (point-min))
      (search-forward "defun")
      (let* ((defun-start (match-beginning 0))
             (defun-end (match-end 0))
             (run (seq-find (lambda (r)
                              (and (<= (nth 0 r) defun-start)
                                   (>= (nth 1 r) defun-end)))
                            runs)))
        (should run)))))

(ert-deftest markdown-overlays-blocks-test-correct-faces ()
  "Keyword faces from font-lock should be applied."
  (with-temp-buffer
    (insert "```emacs-lisp\n(defun foo (x)\n  x)\n```\n")
    (markdown-overlays-put)
    (let ((runs (markdown-overlays-blocks-tests--face-runs)))
      (should (seq-find (lambda (r) (eq (nth 2 r) 'font-lock-keyword-face))
                        runs)))))

(ert-deftest markdown-overlays-blocks-test-unknown-language-fallback ()
  "Unknown languages should get a single fallback face span covering the body."
  (with-temp-buffer
    (insert "```unknownlang99\nhello world\n```\n")
    (markdown-overlays-put)
    (let ((runs (markdown-overlays-blocks-tests--face-runs)))
      (should (= (length runs) 1))
      (should (eq (nth 2 (car runs)) 'font-lock-doc-markup-face)))))

(provide 'markdown-overlays-blocks-tests)

;;; markdown-overlays-blocks-tests.el ends here
