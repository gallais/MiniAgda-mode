;; this should be inserted in your .emacs
;; be careful to replace PATH/TO/ with the path...

;; MiniAgda
(autoload 'miniagda-mode "PATH/TO/ma.el" nil t)
(add-to-list 'auto-mode-alist '("\\.ma\\'" . miniagda-mode))
