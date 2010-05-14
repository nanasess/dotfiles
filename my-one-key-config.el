(defvar one-key-menu-C-x-and-RET-alist nil
  "The `one-key' menu alist for C-x and RET.")

(setq one-key-menu-C-x-and-RET-alist
      '(
	(("C-\\" . "Set Input Method") . set-input-method)
	(("F" . "Set File Name Coding System") . set-file-name-coding-system)
	(("X" . "Set Next Selection Coding System") . set-next-selection-coding-system)
	(("c" . "Universal Coding System Argument") . universal-coding-system-argument)
	(("f" . "Set Buffer File Coding System") . set-buffer-file-coding-system)
	(("k" . "Set Keyboard Coding System") . set-keyboard-coding-system)
	(("l" . "Set Language Environment") . set-language-environment)
	(("p" . "Set Buffer Process Coding System") . set-buffer-process-coding-system)
	(("r" . "Revert Buffer With Coding System") . revert-buffer-with-coding-system)
	(("t" . "Set Terminal Coding System") . set-terminal-coding-system)
	(("x" . "Set Selection Coding System") . set-selection-coding-system)
	))

(defun one-key-menu-C-x-and-RET ()
  "The `one-key' menu for C-x and RET"
  (interactive)
  (one-key-menu "C-x and RET" one-key-menu-C-x-and-RET-alist))

(define-key ctl-x-map [return] 'one-key-menu-C-x-and-RET)

(defvar one-key-menu-DVC-alist nil
  "The `one-key' menu alist for DVC.")

(setq one-key-menu-DVC-alist
      '(
	;; (("ESC" . "Prefix Command") . Prefix Command)
	(("=" . "Dvc Diff") . dvc-diff)
	(("?" . "Tla Help") . tla-help)
	(("A" . "Tla Archives") . tla-archives)
	(("C" . "Dvc Clone") . dvc-clone)
	(("F" . "Dvc Pull") . dvc-pull)
	(("I" . "Dvc Init") . dvc-init)
	(("L" . "Dvc Log") . dvc-log)
	(("M" . "Dvc Merge") . dvc-merge)
	(("P" . "Dvc Push") . dvc-push)
	(("R" . "Tla Redo") . tla-redo)
	(("U" . "Tla Undo") . tla-undo)
	(("a" . "Dvc Add Log Entry") . dvc-add-log-entry)
	(("b" . "Dvc Bookmarks") . dvc-bookmarks)
	(("c" . "Dvc Log Edit") . dvc-log-edit)
	(("d" . "Dvc File Diff") . dvc-file-diff)
	(("e" . "Dvc File Ediff") . dvc-file-ediff)
	;; (("f" . "Prefix Command") . Prefix Command)
	(("i" . "Dvc Inventory") . dvc-inventory)
	(("l" . "Dvc Changelog") . dvc-changelog)
	(("m" . "Dvc Missing") . dvc-missing)
	;; (("o" . "Prefix Command") . Prefix Command)
	(("p" . "Dvc Submit Patch") . dvc-submit-patch)
	(("r" . "Tla Tree Revisions") . tla-tree-revisions)
	(("s" . "Dvc Status") . dvc-status)
	(("t" . "Tla Tag Insert") . tla-tag-insert)
	(("u" . "Dvc Update") . dvc-update)
	;; (("v" . "Prefix Command") . Prefix Command)
	;; (("w" . "Prefix Command") . Prefix Command)

	;; (("w a" . "Tla Save Archive To Kill Ring") . tla-save-archive-to-kill-ring)
	;; (("w r" . "Tla Save Revision To Kill Ring") . tla-save-revision-to-kill-ring)
	;; (("w v" . "Tla Save Version To Kill Ring") . tla-save-version-to-kill-ring)

	;; (("v =" . "Tla Changes Goto") . tla-changes-goto)
	;; (("v L" . "Tla Tree Lint Goto") . tla-tree-lint-goto)
	;; (("v e" . "Dvc Show Last Error Buffer") . dvc-show-last-error-buffer)
	;; (("v i" . "Tla Inventory Goto") . tla-inventory-goto)
	;; (("v l" . "Dvc Open Internal Log Buffer") . dvc-open-internal-log-buffer)
	;; (("v p" . "Dvc Show Process Buffer") . dvc-show-process-buffer)
	;; (("v r" . "Tla Tree Revisions Goto") . tla-tree-revisions-goto)
	;; (("v s" . "Baz Status Goto") . baz-status-goto)

	;; (("f =" . "Dvc File Diff") . dvc-file-diff)
	;; (("f D" . "Dvc Remove Files") . dvc-remove-files)
	;; (("f M" . "Dvc Rename") . dvc-rename)
	;; (("f R" . "Dvc Revert Files") . dvc-revert-files)
	;; (("f X" . "Dvc Purge Files") . dvc-purge-files)
	;; (("f a" . "Dvc Add Files") . dvc-add-files)

	;; (("o c" . "Dvc Create Branch") . dvc-create-branch)
	;; (("o l" . "Dvc List Branches") . dvc-list-branches)
	;; (("o s" . "Dvc Select Branch") . dvc-select-branch)

	(("M-l" . "Tla Tree Lint") . tla-tree-lint)
	))

(defun one-key-menu-DVC ()
  "The `one-key' menu for DVC"
  (interactive)
  (one-key-menu "DVC" one-key-menu-DVC-alist))

(define-key global-map "\C-xV" 'one-key-menu-DVC)

(provide 'my-one-key-config)