;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))

;; see https://github.com/jschaf/esup/issues/54#issue-317095645
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq byte-compile-warnings '(cl-functions))
(setq load-prefer-newer t)
(push '(tool-bar-lines . 0) default-frame-alist)

(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 16)
  (setq native-comp-speed 3))

;; see https://zenn.dev/takeokunn/articles/56010618502ccc#el-get%E3%81%AEpackage%E3%82%82nativecomp%E3%81%99%E3%82%8B
(defun my/native-comp-packages ()
  (interactive)
  (native-compile-async "~/.emacs.d/init.el")
  (native-compile-async "~/.emacs.d/init.d" 'recursively)
  (native-compile-async "~/.emacs.d/early-init.el")
  (native-compile-async "~/.emacs.d/el-get/.loaddefs.el")
  (native-compile-async "~/.emacs.d/el-get-init.d" 'recursively)
  (native-compile-async "~/.emacs.d/elpa" 'recursively)
  (native-compile-async "~/.emacs.d/el-get" 'recursively))

(provide 'early-init)
