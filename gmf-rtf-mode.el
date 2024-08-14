;;; gmf-rtf-mode.el --- A major mode for editing GMF RTF scripts. -*- lexical-binding: t -*-

;; Modified from `scad-mode.el'.

;;; Code:

(require 'compat)
(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cl-lib))

(require 'cape-keyword)

(require 'smartparens)

(defgroup gmf-rtf nil
  "A major mode for editing GMF RTF scripts."
  :link '(url-link :tag "Homepage" "https://github.com/bohonghuang/gmf-rtf-mode")
  :link '(emacs-library-link :tag "Library Source" "gmf-rtf-mode.el")
  :group 'languages
  :prefix "gmf-rtf-")

(defcustom gmf-rtf-keywords
  '("pipe" "control" "event" "bind" "import")
  "GMF RTF keywords."
  :type '(repeat string))

(defcustom gmf-rtf-functions
  '("start" "stop" "set" "sel")
  "GMF RTF functions."
  :type '(repeat string))

(defcustom gmf-rtf-modules
  '()
  "GMF RTF modules."
  :type '(repeat string))

(defcustom gmf-rtf-deprecated
  '()
  "GMF RTF deprecated modules and functions."
  :type '(repeat string))

(defcustom gmf-rtf-operators
  '("->")
  "GMF RTF operators."
  :type '(repeat string))

(defvar-keymap gmf-rtf-mode-map
  :doc "Keymap for `gmf-rtf-mode'."
  :parent c-mode-base-map
  "TAB" #'indent-for-tab-command
  "M-TAB" #'completion-at-point)

(defvar gmf-rtf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (c-populate-syntax-table st)
    st)
  "Syntax table for `gmf-rtf-mode'.")

(defface gmf-rtf-font-lock-keyword-face
  '((default :inherit font-lock-keyword-face))
  "Face for highlighting keywords in GMF RTF mode.")

(cl-eval-when (:compile-toplevel :load-toplevel :execute)
  (put 'gmf-rtf-mode 'c-mode-prefix "gmf-rtf-"))

(defvar gmf-rtf-font-lock-keywords
  `(("\\(pipe\\)[ \t]+\\(\\sw+\\)" . ((1 'gmf-rtf-font-lock-keyword-face nil) (2 'font-lock-function-name-face nil t)))
    ("[^A-Za-z_]\\([0-9]+\\(?:\\.[0-9]+\\)?[fd]?\\)" . ((1 'font-lock-constant-face nil)))
    ("<\\(\\sw+\\)>" . ((1 'font-lock-variable-use-face nil)))
    ("^#[ \t]*import" . font-lock-preprocessor-face)
    ,@(c-lang-const c-complex-decl-matchers gmf-rtf)
    (,(regexp-opt gmf-rtf-keywords 'words)   . font-lock-keyword-face)
    (,(regexp-opt gmf-rtf-modules 'words)    . font-lock-builtin-face)
    (,(regexp-opt gmf-rtf-functions 'words)  . font-lock-function-name-face)
    (,(regexp-opt gmf-rtf-deprecated 'words) . font-lock-warning-face)
    (,(regexp-opt gmf-rtf-operators) . font-lock-operator-face))
  "Keyword highlighting specification for `gmf-rtf-mode'.")

(defconst gmf-rtf-font-lock-keywords-1 gmf-rtf-font-lock-keywords)
(defconst gmf-rtf-font-lock-keywords-2 gmf-rtf-font-lock-keywords)
(defconst gmf-rtf-font-lock-keywords-3 gmf-rtf-font-lock-keywords)

(defvar gmf-rtf-completions
  (append gmf-rtf-keywords gmf-rtf-functions gmf-rtf-modules)
  "List of known words for completion.")

(setf (alist-get 'gmf-rtf-mode cape-keyword-list) gmf-rtf-completions)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rtf\\'" . gmf-rtf-mode))

;;;###autoload
(define-derived-mode gmf-rtf-mode prog-mode "GMF RTF"
  "Major mode for editing GMF RTF scripts."
  :group 'gmf-rtf
  :after-hook (c-update-modeline)
  ;; (add-hook 'completion-at-point-functions
  ;;           #'gmf-rtf-completion-at-point nil 'local)
  (c-initialize-cc-mode t)
  (c-init-language-vars gmf-rtf-mode)
  (c-common-init 'gmf-rtf-mode)
  (c-set-offset 'cpp-macro 0 nil)
  (c-run-mode-hooks 'c-mode-common-hook))

(defun gmf-rtf-completion-at-point ()
  "Completion at point function."
  (when-let (bounds (bounds-of-thing-at-point 'word))
    (list (car bounds) (cdr bounds)
          gmf-rtf-completions
          :exclusive 'no)))

(defun sp-gmf-rtf-filter-angle-brackets (_id action context)
  "Non-nil if we should allow ID's ACTION in CONTEXT for angle brackets."
  ;; See the docstring for `sp-pair' for the possible values of ID,
  ;; ACTION and CONTEXT.
  (cond
   ;; Inside strings, don't do anything with < or >.
   ((eq context 'string)
    nil)
   ;; Don't do any smart pairing inside comments either.
   ((eq context 'comment)
    nil)
   ;; Otherwise, we're in code.
   ((eq context 'code)
    (let ((on-fn-return-type
           (looking-back (rx "->") nil))
          (on-match-branch
           (looking-back (rx "=>") nil)))
      (cond
       ;; Only insert a matching > if we're not looking at a
       ;; comparison.
       ((eq action 'insert)
        (and (not on-fn-return-type) (not on-match-branch)))
       ;; Always allow wrapping in a pair if the region is active.
       ((eq action 'wrap)
        (not on-match-branch))
       ;; When pressing >, autoskip if we're not looking at a
       ;; comparison.
       ((eq action 'autoskip)
        (and (not on-fn-return-type) (not on-match-branch)))
       ;; Allow navigation, highlighting and strictness checks if it's
       ;; not a comparison.
       ((eq action 'navigate)
        (and (not on-fn-return-type) (not on-match-branch))))))))

(defun sp-gmf-rtf-skip-match-angle-bracket (_ms _mb me)
  "Non-nil if we should ignore the bracket as valid delimiter."
  (save-excursion
    (goto-char me)
    (let ((on-fn-return-type
           (sp--looking-back-p (rx "->") nil))
          (on-match-branch
           (sp--looking-back-p (rx "=>") nil)))
      (or on-fn-return-type on-match-branch))))

(sp-with-modes '(gmf-rtf-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "<" ">"
                 :when '(sp-gmf-rtf-filter-angle-brackets)
                 :skip-match 'sp-gmf-rtf-skip-match-angle-bracket))

(provide 'gmf-rtf-mode)
;;; gmf-rtf-mode.el ends here
