(add-hook 'after-init-hook  (lambda() (eshell)))

(setq eshell-modify-global-environment t)
(add-hook 'eshell-mode-hook
	  '(lambda nil
	     (eshell/export "LC_ALL=ja_JP.UTF-8")))
(setq eshell-ask-to-save-history 'always)
(setq eshell-history-size 100000)
(setq eshell-ls-use-in-dired t)
(setq eshell-ls-dired-initial-args '("-h"))
(setq eshell-ls-exclude-regexp "~\\'")
(setq eshell-ls-initial-args "-h")
(setq eshell-visual-commands '("vi" "top" "screen" "less" "ssh"))
(setq eshell-modules-list '(eshell-alias eshell-basic eshell-cmpl eshell-dirs
					 eshell-glob eshell-hist eshell-ls
					 eshell-pred eshell-prompt eshell-script
					 eshell-term eshell-unix eshell-xtra))
(eval-after-load "em-ls"
    '(progn
       (defun ted-eshell-ls-find-file-at-point (point)
	 "RET on Eshell's `ls' output to open files."
	 (interactive "d")
	 (find-file (buffer-substring-no-properties
		     (previous-single-property-change point 'help-echo)
		     (next-single-property-change point 'help-echo))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
	 "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
	 (interactive "e")
	 (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

       (let ((map (make-sparse-keymap)))
	 (define-key map (kbd "RET")	  'ted-eshell-ls-find-file-at-point)
	 (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
	 (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
	 (defvar ted-eshell-ls-keymap map))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
	 "Eshell's `ls' now lets you click or RET on file names to open them."
	 (add-text-properties 0 (length ad-return-value)
			      (list 'help-echo "RET, mouse-2: visit this file"
				    'mouse-face 'highlight
				    'keymap ted-eshell-ls-keymap)
			      ad-return-value)
	 ad-return-value)))

(defun eshell-scroll-to-bottom (window display-start)
  (if (and window (window-live-p window))
      (let ((resize-mini-windows nil))
	(save-selected-window
	  (select-window window)
	  (save-restriction
	    (widen)
	    (when (> (point) eshell-last-output-start) ; we're editing a line. Scroll.
	      (save-excursion
		(recenter -1)
		(sit-for 0))))))))

(defun eshell-add-scroll-to-bottom ()
  (interactive)
  (make-local-hook 'window-scroll-functions)
  (add-hook 'window-scroll-functions 'eshell-scroll-to-bottom nil t))

(add-hook 'eshell-mode-hook 'eshell-add-scroll-to-bottom)

(setq eshell-output-filter-functions '(eshell-handle-control-codes 
				       eshell-watch-for-password-prompt
				       eshell-postoutput-scroll-to-bottom))
(setq eshell-scroll-show-maximum-output t)
(setq eshell-scroll-to-bottom-on-output nil)

;; aliase
(defun eshell/clear ()
  "Clear the current buffer, leaving one prompt at the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
(defun eshell/f (file)
  (find-file file))
(defun eshell/less (file)
  (view-file file))
(defalias 'eshell/more 'eshell/less)

(provide 'eshell-settings)