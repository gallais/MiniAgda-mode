;; based on: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; define several class of keywords
(setq mylsl-keywords '("let" "in" "fun" "mutual" "sized" "fields" "data" "codata" "record"))
(setq mylsl-types '("Set" "Size"))

;; create the regex string for each class of keywords
(setq mylsl-keywords-regexp (regexp-opt mylsl-keywords 'words))
(setq mylsl-type-regexp (regexp-opt mylsl-types 'words))

;; clear memory
(setq mylsl-keywords nil)
(setq mylsl-types nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq mylsl-font-lock-keywords
  `(
    (,mylsl-type-regexp . font-lock-type-face)
    (,mylsl-keywords-regexp . font-lock-keyword-face)
))

;; syntax table
(defvar miniagda-syntax-table nil "Syntax table for `miniagda-mode'.")
(setq miniagda-syntax-table
  (let ((synTable (make-syntax-table)))

  ;; single line
  (modify-syntax-entry ?- ". 123b" synTable)
  (modify-syntax-entry ?\n "> b" synTable)
	;; multiple lines
	(modify-syntax-entry ?{ ". 1n" synTable)
  (modify-syntax-entry ?} ". 4n" synTable)

        synTable))

;; definition of the compilation command for MiniAgda
(setq compilation-read-command nil)
(setq compilation-ask-about-save nil)
(setq compilation-finish-function 'jumpEndCompile)
(add-hook 'miniagda-mode-hook (lambda ()
   (set (make-local-variable 'compile-command)
        (concat "miniagda " (buffer-file-name (current-buffer))))))

;; automatically jumping to the end of the compilation buffer
;; after `compile` has been run.
(defun jumpEndCompile (buf str)
   (let ((output-win (get-buffer-window "*compilation*"))
         (orig-win (selected-window)))
   (select-window output-win)
   (end-of-buffer)
   (select-window orig-win)))

;; define the mode
(define-derived-mode miniagda-mode fundamental-mode
  "MiniAgda mode"
  ;; handling comments
  :syntax-table miniagda-syntax-table
  ;; code for syntax highlighting
  (setq font-lock-defaults '((mylsl-font-lock-keywords)))
  (setq mode-name "MiniAgda")
  ;; clear memory
  (setq mylsl-keywords-regexp nil)
  (setq mylsl-types-regexp nil)
  ;; set compilation key binding
  (local-set-key (kbd "C-c C-l") 'compile)
)

(provide 'miniagda-mode)
