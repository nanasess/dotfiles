;;; copilot-chat-benchmark.el --- Benchmark copilot-chat models -*- lexical-binding: t; -*-

;;; Commentary:
;; Benchmark script for copilot-chat commit message generation.
;;
;; Usage:
;;   1. Load this file:
;;      (load-file "~/.emacs.d/test/copilot-chat-benchmark.el")
;;
;;   2. Stage some changes with git:
;;      $ git add <files>
;;
;;   3. Run the benchmark:
;;      M-x copilot-chat-benchmark-run
;;
;;   4. Results will be displayed in *Copilot Chat Benchmark* buffer
;;
;; Customization:
;;   - `copilot-chat-benchmark-models': List of models to benchmark
;;   - M-x copilot-chat-benchmark-add-model: Add a model to the list

;;; Code:

(require 'copilot-chat)
(require 'copilot-chat-common)
(require 'copilot-chat-git)
(require 'copilot-chat-copilot)

(defvar copilot-chat-benchmark-models
  '("gpt-4o-mini" "claude-haiku-4.5" "grok-code-fast-1" "gpt-4.1" "gpt-4o")
  "Models to benchmark.")

(defvar copilot-chat-benchmark-results nil
  "Alist of (model time-in-seconds content) results.")

(defvar copilot-chat-benchmark--start-time nil
  "Start time of current benchmark.")

(defvar copilot-chat-benchmark--current-model nil
  "Current model being benchmarked.")

(defvar copilot-chat-benchmark--pending-models nil
  "Models remaining to benchmark.")

(defvar copilot-chat-benchmark--buffer nil
  "Buffer for benchmark output.")

(defvar copilot-chat-benchmark--original-model nil
  "Original commit model to restore after benchmark.")

(defun copilot-chat-benchmark--get-diff ()
  "Get staged diff content synchronously."
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (shell-command-to-string "git diff --cached")))

(defun copilot-chat-benchmark--run-single (model callback)
  "Run benchmark for a single MODEL, then call CALLBACK with result."
  (setq copilot-chat-benchmark--current-model model)
  (setq copilot-chat-benchmark--start-time (current-time))

  ;; Set the commit model
  (setq copilot-chat-commit-model model)

  ;; Get diff content
  (let* ((diff-content (copilot-chat-benchmark--get-diff))
         (instance (copilot-chat--ensure-commit-instance))
         (accumulated ""))

    ;; Clear history for fresh generation
    (setf (copilot-chat-history instance) nil)

    (message "Benchmarking model: %s..." model)

    ;; Call copilot-chat--ask directly with our callback
    ;; Callback receives (instance content) where content is copilot-chat--magic when done
    (copilot-chat--with-commit-context
     (copilot-chat--ask
      instance
      diff-content
      (lambda (inst content)
        (if (string= content copilot-chat--magic)
            ;; Done
            (let* ((end-time (current-time))
                   (elapsed (float-time (time-subtract end-time copilot-chat-benchmark--start-time))))
              (push (list model elapsed accumulated) copilot-chat-benchmark-results)
              (funcall callback model elapsed accumulated))
          ;; Accumulate content
          (setq accumulated (concat accumulated (or content "")))))
      t))))

(defun copilot-chat-benchmark--format-results ()
  "Format benchmark results as a string."
  (let ((sorted (sort (copy-sequence copilot-chat-benchmark-results)
                      (lambda (a b) (< (nth 1 a) (nth 1 b))))))
    (concat
     "┌─────────────────────────────────────────────────┐\n"
     "│     Copilot Chat Commit Message Benchmark       │\n"
     "├──────────────────────┬────────────────┬─────────┤\n"
     "│ Model                │ Time (seconds) │ Rank    │\n"
     "├──────────────────────┼────────────────┼─────────┤\n"
     (mapconcat
      (lambda (result)
        (let* ((model (nth 0 result))
               (time (nth 1 result))
               (rank (1+ (cl-position result sorted :test #'equal))))
          (format "│ %-20s │ %14.2f │ #%-6d │"
                  (truncate-string-to-width model 20)
                  time
                  rank)))
      sorted
      "\n")
     "\n└──────────────────────┴────────────────┴─────────┘\n")))

(defun copilot-chat-benchmark--format-outputs ()
  "Format generated commit messages from all models."
  (mapconcat
   (lambda (result)
     (let* ((model (nth 0 result))
            (time (nth 1 result))
            (content (nth 2 result)))
       (concat
        (format "\n### %s (%.2fs)\n" model time)
        "```\n"
        (string-trim content)
        "\n```\n")))
   (sort (copy-sequence copilot-chat-benchmark-results)
         (lambda (a b) (< (nth 1 a) (nth 1 b))))
   ""))

(defun copilot-chat-benchmark--next ()
  "Run next model benchmark or finish."
  (if copilot-chat-benchmark--pending-models
      (let ((model (pop copilot-chat-benchmark--pending-models)))
        (copilot-chat-benchmark--run-single
         model
         (lambda (_model elapsed _content)
           (with-current-buffer copilot-chat-benchmark--buffer
             (goto-char (point-max))
             (insert (format "  %s: %.2fs\n" _model elapsed)))
           ;; Wait a bit before next request to avoid rate limiting
           (run-with-timer 2 nil #'copilot-chat-benchmark--next))))
    ;; All done
    (copilot-chat-benchmark--finish)))

(defun copilot-chat-benchmark--finish ()
  "Finish benchmark and display results."
  ;; Restore original model
  (setq copilot-chat-commit-model copilot-chat-benchmark--original-model)

  (with-current-buffer copilot-chat-benchmark--buffer
    (goto-char (point-max))
    (insert "\n\n")
    (insert (copilot-chat-benchmark--format-results))
    (insert "\n\n## Generated Commit Messages\n")
    (insert (copilot-chat-benchmark--format-outputs))
    (insert "\n\nBenchmark complete. Original model restored.\n")
    (goto-char (point-min)))

  (display-buffer copilot-chat-benchmark--buffer)
  (message "Benchmark complete! See *Copilot Chat Benchmark* buffer."))

;;;###autoload
(defun copilot-chat-benchmark-run ()
  "Run benchmark for all models in `copilot-chat-benchmark-models'."
  (interactive)

  ;; Check for staged changes
  (let ((diff (copilot-chat-benchmark--get-diff)))
    (when (string-empty-p diff)
      (user-error "No staged changes found. Please stage some changes first")))

  ;; Initialize
  (setq copilot-chat-benchmark-results nil)
  (setq copilot-chat-benchmark--pending-models (copy-sequence copilot-chat-benchmark-models))
  (setq copilot-chat-benchmark--original-model copilot-chat-commit-model)

  ;; Create output buffer
  (setq copilot-chat-benchmark--buffer (get-buffer-create "*Copilot Chat Benchmark*"))
  (with-current-buffer copilot-chat-benchmark--buffer
    (erase-buffer)
    (insert "Copilot Chat Commit Message Benchmark\n")
    (insert "=====================================\n\n")
    (insert (format "Models to test: %s\n\n" (string-join copilot-chat-benchmark-models ", ")))
    (insert "Running benchmarks...\n\n"))

  (display-buffer copilot-chat-benchmark--buffer)

  ;; Start benchmarking
  (copilot-chat-benchmark--next))

;;;###autoload
(defun copilot-chat-benchmark-add-model (model)
  "Add MODEL to the benchmark list."
  (interactive
   (list (completing-read "Model: "
                          (mapcar (lambda (m) (alist-get 'id m))
                                  (copilot-chat-connection-models copilot-chat--connection)))))
  (add-to-list 'copilot-chat-benchmark-models model)
  (message "Added %s to benchmark models." model))

(provide 'copilot-chat-benchmark)
;;; copilot-chat-benchmark.el ends here
