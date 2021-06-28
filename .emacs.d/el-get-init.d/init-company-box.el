(add-hook 'company-mode-hook 'company-box-mode)
(add-hook 'company-box-mode-hook #'(lambda () (require 'all-the-icons)))
(with-eval-after-load-feature 'company-box
  (defun company-box--update-width (&optional no-update height)
    (unless no-update
      (redisplay))
    (-let* ((frame (company-box--get-frame))
            (window (frame-parameter nil 'company-box-window))
            (start (window-start window))
            (char-width (frame-char-width frame))
            (end (or (and height (with-current-buffer (window-buffer window)
                                   (save-excursion
                                     (goto-char start)
                                     (forward-line height)
                                     (point))))
                     (window-end window)))
            ;; (max-width (- (frame-pixel-width) company-box--x char-width))
            (max-width (- (x-display-pixel-width)
                          (max (eval (frame-parameter nil 'left)) 0)
                          company-box--x char-width))
            (width (+ (company-box--calc-len (window-buffer window) start end char-width)
                      (if (company-box--scrollbar-p frame) (* 2 char-width) 0)
                      char-width))
            (width (max (min width max-width)
                        (* company-tooltip-minimum-width char-width)))
            (diff (abs (- (frame-pixel-width frame) width))))
      (or (and no-update width)
          (and (> diff 2) (set-frame-width frame width nil t)))))

  (setq company-box-enable-icon t)
  (setq company-box-show-single-candidate t)
  ;; (setq company-box-max-candidates 50)
  (setq company-box-doc-delay 0.5)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  ;; see https://github.com/zenith-john/zenith-emacs/blob/8d85e5e5d9e477873762452063683609ae2dc91e/config/init-company.el
  (defconst company-box-icons--phpactor-alist
    '(("interface" . Interface)
      ("class" . Class)
      ("method" . Method)
      ("function" . Function)
      ("property" . Property)
      ("constant" . Constant)
      ("variable" . Variable)
      ("interface" . Interface)
      ("module" . Module)
      ("template" . Template)))
  (defun company-box-icons--phpactor (candidate)
    ;; (message "omnisharp-item: %s" (get-text-property 0 'omnisharp-item candidate))
    (when (derived-mode-p 'php-mode)
      (let ((key (get-text-property 0 'type candidate)))
        (cdr (assoc key company-box-icons--phpactor-alist)))))
  (defun company-box-icons--yasnippet+ (candidate)
    (message "%s" (get-text-property 0 'yas-annotation candidate))
    (when (get-text-property 0 'yas-annotation candidate)
      'Yasnippet))

  (setq company-box-icons-functions
        '(company-box-icons--yasnippet+ company-box-icons--lsp company-box-icons--elisp company-box-icons--phpactor))

  (setq company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-blue-alt))
          (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
          (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
          (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
          (Field         . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
          (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-purple))
          (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-purple))
          (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-purple))
          (Property      . ,(all-the-icons-material "settings_applications"    :height 0.8 :face 'all-the-icons-purple))
          (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-purple))
          (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-purple))
          (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-purple))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-purple))
          (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-purple))
          (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-purple))
          (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-purple))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-purple))
          (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-purple))
          (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-purple))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-purple))
          (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-purple))
          (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-purple))
          (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-purple))
          (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-purple))
          ;; (Template   . ,(company-box-icons-image "Template.png"))))
          (Yasnippet     . ,(all-the-icons-material "share"               :height 0.8 :face 'all-the-icons-green))
          (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-purple))
          (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
          (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
          (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink))))
  (setq company-box-backends-colors
        '((company-yasnippet . (:selected (:background "#DEB542" :weight bold)))
          (company-dabbrev . (:selected (:background "PaleTurquoise" :weight bold)))))
  (defface company-box-scrollbar
    '((t (:background "#073642" :weight bold))) nil :group 'company-box))
