(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))
(with-eval-after-load 'plantuml-mode
  (setq plantuml-indent-level 2)
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-output-type "png"))
