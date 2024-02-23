;;----------------------- window settings ---------------------------
;; sudo apt-get install xfonts-mplus
(if (display-graphic-p)
    (progn
      (set-face-attribute 'default nil :family "UDEV Gothic JPDOC" :height 120)
      (when (equal window-system 'pgtk)
        (when (executable-find "wl-copy")
          ;; https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4
          ;; credit: yorickvP on Github
          (setq wl-copy-process nil)
          (defun wl-copy (text)
            (setq wl-copy-process (make-process :name "wl-copy"
                                                :buffer nil
                                                :command '("wl-copy" "-f" "-n")
                                                :connection-type 'pipe))
            (process-send-string wl-copy-process text)
            (process-send-eof wl-copy-process))
          (defun wl-paste ()
            (if (and wl-copy-process (process-live-p wl-copy-process))
                nil ; should return nil if we're the current paste owner
              (shell-command-to-string "wl-paste -n | tr -d \r")))
          (setq interprogram-cut-function 'wl-copy)
          (setq interprogram-paste-function 'wl-paste)))))
;; (setq initial-frame-alist
;;       (append (list
;;                '(height . 32)
;;                '(width  . 82)
;;                initial-frame-alist)))
;; (setq default-frame-alist initial-frame-alist))
;; )
;;;
;;; see https://github.com/4U6U57/wsl-open
(when (executable-find "wsl-open")
  (setq browse-url-generic-program "wsl-open")
  (setq browse-url-browser-function 'browse-url-generic))

(require 'server)
(unless (server-running-p)
  (server-start))
(provide 'linux-init)
