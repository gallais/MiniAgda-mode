;; based on: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; define several class of keywords
(setq mykeywords  '("let" "in" "case" "max" "fun" "cofun" "mutual" "sized" "fields" "data" "codata" "record" "impredicative" "pattern" "eval" "trustme" "check" "fail"))
(setq myoperators '("<|" "|>" "\#" "$" "\\" "->" ":" "<" "<=" ">" "++" "+" "-" "^" "*" "."))
(setq mytypes     '("Set" "CoSet" "Size"))

;; create the regex string for each class of keywords
(setq mykeywords-regexp  (regexp-opt mykeywords  'words))
(setq myoperators-regexp (regexp-opt myoperators))
(setq mytypes-regexp     (regexp-opt mytypes     'words))

;; clear memory
(setq mykeywords  nil)
(setq myoperators nil)
(setq mytypes     nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq myfont-lock-keywords
  `(
    (,mytypes-regexp     . font-lock-type-face)
    (,myoperators-regexp . font-lock-builtin-face)
    (,mykeywords-regexp  . font-lock-keyword-face)
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
  (setq font-lock-defaults '((myfont-lock-keywords)))
  (setq mode-name "MiniAgda")
  ;; clear memory
  (setq mykeywords-regexp nil)
  (setq mytypes-regexp nil)
  ;; set compilation key binding
  (local-set-key (kbd "C-c C-l") 'compile)
)

(provide 'miniagda-mode)
