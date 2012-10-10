;;; Commentary:

;; ------------------------------------------------------------------------
;; Copyright (C) 2006-2007 Jens Peter Secher
;; https://github.com/pdorrell/emacs-site-lisp/blob/master/haxe-mode.el
;; Copyright (C) Ritchie Turner (blackdog@cloudshift.cl)
;; https://github.com/cloudshift/hx-emacs/blob/master/hxc-complete.el
;; Copyright (C) Oleg Sivokon (olegsivokon@gmail.com)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ------------------------------------------------------------------------

;; This is my (Oleg) modification and extension of the original haxe-mode
;; written by Jens Peter Secher, it also borrows from the auto-completion
;; extension written by Ritchie Turner. The original commentary follows.
;; This mode introduces dependency to auto-complete library, one can be
;; found here: http://cx4a.org/software/auto-complete/

;; This is haxe-mode, an Emacs major mode for the haXe programming
;; language (http://haxe.org).

;; haxe-mode is built on top of the excellent cc-mode, inspired by the
;; guide http://cc-mode.sourceforge.net/derived-mode-ex.el.

;; haxe-mode is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.1.1 - Fixed typedef indentation.
;;            Fixed lexical analysis so that type names can contain digits.
;;    0.2.0 - Base on java-mode instead of c++-mode.
;;            Added compile-error parser for the haXe compiler output.
;;            Loads of improvements.
;;    0.2.1 - Fix buffer-local comment-start-skip problem.
;;    0.2.2 - Recognize keyword override.
;;    0.3.0 - Switched to GPLv3 license because that is what cc-mode is using.
;;    0.3.1 - Fix compile problem with emacs23.
;;

;;; Usage:
;;
;; Include something like this in your .emacs:
;; (require 'haxe-mode)
;; (defconst my-haxe-style
;;   '("java" (c-offsets-alist . ((case-label . +)
;;                                (arglist-intro . +)
;;                                (arglist-cont-nonempty . 0)
;;                                (arglist-close . 0)
;;                                (cpp-macro . 0))))
;;   "My haXe Programming Style")
;; (add-hook 'haxe-mode-hook
;;   (function (lambda () (c-add-style "haxe" my-haxe-style t))))
;; (add-hook 'haxe-mode-hook
;;           (function
;;            (lambda ()
;;              (setq tab-width 4)
;;              (setq indent-tabs-mode t)
;;              (setq fill-column 80)
;;              (local-set-key [(return)] 'newline-and-indent))))


;;; Code:

(eval-when-compile (require 'cl))
(require 'cc-mode)
(require 'cc-fonts)
(require 'cc-langs)
(require 'cc-bytecomp)
(require 'compile)
;; ------------------- my change -------------------------------------
(require 'flymake)
(require 'xml)
(require 'ehelp)
(require 'haxe-help)
(require 'haxe-project)
(require 'haxe-completion)
(require 'haxe-log)
;; ------------------- my change -------------------------------------

;; The language constants are needed when compiling.
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)
    (load "cc-bytecomp" nil t)))

(eval-and-compile
  ;; Tell the language constant system about haXe and base it on Java.
  (c-add-language 'haxe-mode 'java-mode))

;;; Lexer-level syntax (identifiers, tokens etc).

;; No other operators in identifiers.
(c-lang-defconst c-after-id-concat-ops
  haxe nil)

;; Conditional compilation and metadata prefices.
(c-lang-defconst c-opt-cpp-prefix
  haxe "\\s *#" haxe "\\s *@:")

;; No strings in conditional compilation.
(c-lang-defconst c-cpp-message-directives
  haxe nil)

;; No file name in angle brackets or quotes in conditional compilation.
(c-lang-defconst c-cpp-include-directives
  haxe nil)

;; No macro definition in conditional compilation.
(c-lang-defconst c-opt-cpp-macro-define
  haxe nil)

;; Conditional compilation directives followed by expressions.
(c-lang-defconst c-cpp-expr-directives
  haxe '("if" "else"))

;; No functions in conditional compilation.
(c-lang-defconst c-cpp-expr-functions
  haxe nil)

;; haXe operators.
(c-lang-defconst c-operators
  haxe `(
         ;; Preprocessor.
         (prefix "#")
         ;; Standard operators.
         ,@(c-lang-const c-identifier-ops)
         ;; Generics.
         (postfix-if-paren "<" ">")
         ;; Postfix.
         (left-assoc "." "->")
         (postfix "++" "--" "[" "]" "(" ")")
         ;; Unary.
         (prefix "++" "--" "+" "-" "!" "~" "new")
         ;; Multiplicative.
         (left-assoc "*" "/" "%")
         ;; Additive.
         (left-assoc "+" "-")
         ;; Shift.
         (left-assoc "<<" ">>" ">>>")
         ;; Relational.
         (left-assoc "<" ">" "<=" ">=")
         ;; Iteration.
         (left-assoc "...")
         ;; Equality.
         (left-assoc "==" "!=" "===" "!==")
         ;; Bitwise and.
         (left-assoc "&")
         ;; Bitwise exclusive or.
         (left-assoc "^")
         ;; Bitwise or.
         (left-assoc "|")
         ;; Logical and.
         (left-assoc "&&")
         ;; Logical or.
         (left-assoc "||")
         ;; Assignment.
         (right-assoc ,@(c-lang-const c-assignment-operators))
         ;; Exception.
         (prefix "throw")
         ;; Sequence.
         (left-assoc ",")
         ;; metadata (macros)
         (prefix "@:")))

;; No overloading.
(c-lang-defconst c-overloadable-operators
  haxe nil)
(c-lang-defconst c-opt-op-identitier-prefix
  haxe nil)

;;; Keywords.

;; I will treat types uniformly below since they all start with capital
;; letters.
(c-lang-defconst c-primitive-type-kwds
  haxe nil)

;; TODO: check double occurrence of enum.
;; Type-introduction is straight forward in haXe.
(c-lang-defconst c-class-decl-kwds
  haxe '( "class" "interface" "enum" "typedef" ))

;; Recognises enum constants.
;; TODO: find a way to also recognise parameterised constants.
(c-lang-defconst c-brace-list-decl-kwds
  haxe '( "enum" ))

;; Keywords introducing declarations where the identifier follows directly
;; after the keyword, without any type.
(c-lang-defconst c-typeless-decl-kwds
  haxe (append '( "function" "var" )
               (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds)))
  
;; Definition modifiers.
(c-lang-defconst c-modifier-kwds
  haxe '( "private" "public" "static" "override"))
(c-lang-defconst c-other-decl-kwds
  haxe nil)

;; Namespaces.
(c-lang-defconst c-ref-list-kwds
 haxe '( "import" "package"))

;; Statement keywords followed directly by a substatement.
(c-lang-defconst c-block-stmt-1-kwds
  haxe '( "do" "else" "try" ))

;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  haxe '( "for" "if" "switch" "while" "catch" ))

;; Statement keywords followed by an expression or nothing.
(c-lang-defconst c-simple-stmt-kwds
  haxe '( "break" "continue" "return" "default" "new" ))

;; No ';' inside 'for'.
(c-lang-defconst c-paren-stmt-kwds
  haxe nil)

;; Keywords for constants.
(c-lang-defconst c-constant-kwds
  haxe '( "false" "true" "null" ))

;; Keywords for expressions.
(c-lang-defconst c-primary-expr-kwds
  haxe '( "this" "super" ))

(c-lang-defconst c-decl-hangon-kwds
  haxe '( "in" ))

;; No other labels.
(c-lang-defconst c-before-label-kwds
  haxe nil)

;; No classes inside expressions.
(c-lang-defconst c-inexpr-class-kwds
  haxe nil)

;; No brace lists inside expressions.
(c-lang-defconst c-inexpr-brace-list-kwds
  haxe nil)

;; All identifiers starting with a capital letter are types.
(c-lang-defconst c-cpp-matchers
  haxe (append
        (c-lang-const c-cpp-matchers c)
        '(("\\<\\([A-Z][A-Za-z0-9_]*\\)\\>" 1 font-lock-type-face))))

;; Generic types.
(c-lang-defconst c-recognize-<>-arglists
  haxe t)

;; Fontification degrees.
(defconst haxe-font-lock-keywords-1 (c-lang-const c-matchers-1 haxe)
  "Minimal highlighting for haxe mode.")

(defconst haxe-font-lock-keywords-2 (c-lang-const c-matchers-2 haxe)
  "Fast normal highlighting for haxe mode.")

(defconst haxe-font-lock-keywords-3 (c-lang-const c-matchers-3 haxe)
  "Accurate normal highlighting for haxe mode.")

(defvar haxe-font-lock-keywords haxe-font-lock-keywords-3
  "Default expressions to highlight in haxe mode.")

(defvar haxe-mode-syntax-table nil
  "Syntax table used in HaXe mode buffers.")
(or haxe-mode-syntax-table
    (setq haxe-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table haxe))))

(defvar haxe-mode-abbrev-table nil
  "Abbreviation table used in haxe mode buffers.")
(c-define-abbrev-table 'haxe-mode-abbrev-table
  ;; Keywords that, if they occur first on a line, might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar haxe-mode-map ()
  "Keymap used in haxe mode buffers.")
(if haxe-mode-map
    nil
  (setq haxe-mode-map (c-make-inherited-keymap)))

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

(make-variable-buffer-local 'compile-command)

;; Tell compilation-mode how to parse error messages.  You need to set
;; compilation-error-screen-columns to nil to get the right
;; interpretation of tabs.
(add-to-list 'compilation-error-regexp-alist
             '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
               1 2 3))

;; ------------------- My edits --------------------------------------------

;; TODO: we need to use when generating TAGS, but before we
;; can use it, we need a good way to generate TAGS for HaXe code
(defcustom haxe-std-library nil
  "The location of HaXe built-ins, it is needed for TAGS generation"
  :type 'string :group 'haxe-mode)

(defvar old-flymake-after-change-function nil
  "Stores the function flymake uses to update after code change
once we turn it off")

(defun haxe-flymake-install ()
  "Install flymake stuff for HaXe files."
  (add-to-list
   'compilation-error-regexp-alist
   '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
     1 2 3))
  (flymake-log 3 "HaXe flymake installed")
  (haxe-start-waiting-server)
  (let* ((key "\\.hx\\'")
         (haxeentry (assoc key flymake-allowed-file-name-masks)))
    (if haxeentry
        (setcdr haxeentry '(haxe-flymake-init haxe-flymake-cleanup))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'haxe-flymake-init 'haxe-flymake-cleanup)))))

(defun haxe-flymake-init ()
  "initialize flymake for HaXe."
  (message "haxe-flymake-init completion-requested %s" completion-requested)
  (unless completion-requested
    (let ((create-temp-f 'haxe-flymake-create-temp-intemp)
	  (use-relative-base-dir nil)
	  (use-relative-source nil)
	  (get-cmdline-f 'haxe-flymake-get-cmdline)
	  args
	  temp-source-file-name)
      (haxe-log 3 "Flymake HaXe init")
      (setq temp-source-file-name
	    (flymake-init-create-temp-buffer-copy create-temp-f)
	    args (flymake-get-syntax-check-program-args
		  temp-source-file-name (resolve-project-root)
		  use-relative-base-dir use-relative-source
		  get-cmdline-f))
      args)))

(defun haxe-flymake-get-cmdline (source base-dir)
  "Gets the cmd line for running a flymake session in a HaXe buffer.
This gets called by flymake itself. The output is a list of two elements:
the command to run, and a list of arguments.  The resulting command is like:

  $ haxe `(resolve-project-root)'/`build-hxml'

"
  (save-buffer)
  (list haxe-compiler
	(append
	 (list
	  "--connect"
	  (concat haxe-server-host ":" (number-to-string haxe-server-port)))
	 (haxe-build-flymake-list
	  (replace-all (substring (file-name-sans-extension (buffer-file-name))
				  (+ (length (resolve-project-root)) 5)) [?/] [?.])))))

(defun haxe-flymake-cleanup ()
  "Called by flymake when it needs to cleanup after reporting"
  (flymake-simple-cleanup))

(defun haxe-flymake-create-temp-intemp (file-name prefix)
  "We need this to let flymake create a temp buffer 
so that it doesn't kill our files..."
  (make-temp-file
   (file-name-nondirectory
    (file-name-sans-extension file-name)) nil "tmp"))

(defun haxe-identify-project-root ()
  "Lame attempt at finding the root directory of our project.
The assumtion is that most people would call it `src', so we 
climb up the directory tree to see if there's a directory with this
name on the path and assume the last such instance to be our project
directory, for example /home/user/projects/foo/src/org/user/utils/Bar.hx
wil result in /home/user/projects/foo/src/ being selected as the root
directory"
  (let* ((current (buffer-file-name))
	 (pos (string-match "/src/" current)))
    (when pos
      (setq project-root (substring current 0 pos)))))

(defun haxe-listen-filter (proc input)
  "Is called by the running HaXe server to report events, if any."
  ;; We are only interested in recording the completion XMLs
  (block nil
    (cond
     ((null input)
      (haxe-log 3 "HaXe compiler sends no input")
      (when (= received-status 2)
	(setq last-compiler-response "No input"
	      completion-requested nil)
	(return-from nil)))
     ((and (= received-status 2) (not (null input))
	   input (char-equal (aref input 0) ?<))
      (if (or (and (string= (substring input 0 6) "<list>")
		   (string= response-terminator "</list>\n"))
	      (and (string= (substring input 0 6) "<type>")
		   (string= response-terminator "</type>\n")))
	  (setq received-status 0
		last-compiler-response input)
	(progn
	  (setq last-compiler-response "Wrong tag"
		completion-requested nil)
	  (haxe-log 3 "Received wrong result, expected %s, received %s"
		    (substring response-terminator 0 -1) input)
	  (return-from nil))))
     ((= received-status 1)
      (setq last-compiler-response (concat last-compiler-response input)))
     ((= received-status 2)
      (haxe-log 3 "Compiler had something to say:

'%s'

But chosen a bad time to do it" input)
      (setq last-compiler-response nil)
      (return-from nil)))
    
    (if (and (< received-status 2)
	     (string= (substring last-compiler-response
				 (- (length response-terminator)))
		      response-terminator))
	(setq received-status 2)
      (setq received-status 1))

    (haxe-log 3 "filter received: %s %s"
	      received-status
	      (string= (substring last-compiler-response
				  (- (length response-terminator)))
		       response-terminator))))

;; ----------------------------------------------------------------------------
;; Ritchie Turner (blackdog@cloudshift.cl)
;; remake of https://github.com/cloudshift/hx-emacs/blob/master/hxc-complete.el

(defun haxe-build-flymake-list (source)
  "Builds the command run by flymake on the current buffer"
  (let ((conditionals (haxe-conditional-comps))
	(result
	 (append
	  (haxe-build-cwd)
	  (haxe-read-hxml)
	  (list source))))
    (if conditionals
	(append conditionals result)
      result)))

(defadvice flymake-after-change-function
  (around haxe-override-flymake-change (start stop len))
  "Overrides `flymake-after-change-function' to prevent it from running
when autocompletion is in progress"
  (unless (= 2 received-status) ad-do-it))

;; (defun haxe-flymake-change-function (start stop len)
;;   "Substitute for `flymake-after-change-function' to prevent it from updating
;; when autocompletion is in process"
;;   (unless (= 2 received-status)
;;     (when old-flymake-after-change-function
;; 	(funcall old-flymake-after-change-function start stop len))))

;; ;; TODO: All this nonsense has to be replaced with defadvice.
;; ;; File: elisp.info,  Node: Simple Advice

;; (defun haxe-toggle-flymake-inbetween-saves ()
;;   "Toggles flymake updates made after the code is changed"
;;   (interactive)
;;   (if old-flymake-after-change-function
;;       (progn
;; 	(haxe-log 3 "restoring flymake-after-change-function")
;; 	(fset #'flymake-after-change-function old-flymake-after-change-function)
;; 	(setq old-flymake-after-change-function nil))
;;     (progn
;;       (haxe-log 3 "disabling flymake-after-change-function")
;;       (setq old-flymake-after-change-function #'flymake-after-change-function)
;;       (make-local-variable #'flymake-after-change-function)
;;       (fset #'flymake-after-change-function #'haxe-flymake-change-function))))

(defun haxe-kill-network-process ()
  "Kill connection to HaXe compiler server and Flymake process in this buffer"
  (when (equal major-mode 'haxe-mode)
    (let ((proc (get-process haxe-compiler-process))
	  (fly-proc (get-buffer-process (buffer-name))))
      (when fly-proc
	(flymake-mode -1)
	(delete-process fly-proc)
	(haxe-log 3 "Flymake process killed"))
      (when proc
	(delete-process proc)
	(haxe-log 3 "Disconnecting from HaXe compiler server")))))

(defun haxe-try-set-ecb-outlines ()
  "See if ECB is installed and tell it to use `speedbar-fetch-dynamic-tags'
to parse HaXe outlines in the current buffer. This is hack-ish, because we
locally rebind (or at least I hope that that is what we do) 
`speedbar-fetch-dynamic-imenu' to `haxe-parse-tags'. There is probably an
easier way to do the same thing. If you find it - let me know."
  (when (boundp 'ecb-non-semantic-parsing-function)
    (haxe-log 0 "Enabling ECB outline support")
    (make-local-variable #'speedbar-fetch-dynamic-imenu)
    ;; TODO: This has to be buffer-local
    (fset #'speedbar-fetch-dynamic-imenu #'haxe-parse-tags)))

    ;; speedbar-insert-imenu-list
    ;; speedbar-insert-etags-list
    ;; (setq ecb-process-non-semantic-files t
    ;; 	  speedbar-use-imenu-flag nil
    ;; 	  ecb-non-semantic-parsing-function
    ;; 	  '(haxe-mode . haxe-parse-tags))
    ;; (append-to-list speedbar-dynamic-tags-function-list
    ;; 		    '(haxe-parse-tags . semantic-sb-insert-tag-table))

(defun haxe-parse-tags (file)
  "Calls `haxe-etags-program' with specially crafter arguments to obtain
tag information for FILE"
  (let ((buff-contents (buffer-string))
	(x 0) (y 0)
	newlist line word type)
    (save-excursion
      (when (get-buffer "*haxe-tags-parser*")
	(kill-buffer "*haxe-tags-parser*"))
      (set-buffer (get-buffer-create "*haxe-tags-parser*"))
      (shell-command
       (concat haxe-etags-program " \\
--lang=none --regex='/[ \\t]*class[ \\t]+\\([^ \\t{\\/]+\\)/\\1/' \\
--regex='/[ \\t]*typedef[ \\t]+\\([^ \\t{\\/=]+\\)/\\1/' \\
--regex='/[ \\t]*enum[ \\t]+\\([^ \\t{\\/]+\\)/\\1/' \\
--regex='/[ \\t]*\\(\\(public\\|private\\|static\\|override\\|inline\\)[ \\t]\\)+function[ \\t]\\([^ \\t(]+\\)/\\3/' \\
--regex='/[ \\t]*\\(\\(public\\|private\\|static\\|override\\|inline\\)[ \\t]\\)+var[ \\t]\\([^ \\t:=]+\\)/\\3/' \\
-o - " (expand-file-name file)) "*haxe-tags-parser*")
      (goto-char (point-min))
      (forward-line)
      (while (not (eobp))
	(forward-line)
	(beginning-of-line-text 1)
	(setq type nil)
	(while (not (eolp))
	  (setq word (thing-at-point 'symbol))
	  (cond
	   ((null word))
	   ((or (string= word "class")
		(string= word "function") 
		(string= word "var")
		(string= word "enum")
		(string= word "typedef"))
	    (setq type (concat word " ")))
	   ((or (string= word "private")
		(string= word "public")
		(string= word "override")
		(string= word "static")
		(string= word "inline")))
	   (t (move-end-of-line nil)
	      (backward-word)
	      (setq newlist
		    (cons (cons
			   (concat type " " word)
			   (1+ (string-to-number (thing-at-point 'symbol)))) newlist))
	      (move-end-of-line nil)))
	  (forward-word)))
      (if (and (boundp 'speedbar-sort-tags)
               speedbar-sort-tags)
	  (sort newlist (lambda (a b) (string< (car a) (car b))))
	(reverse newlist)))))

(defun haxe-calculate-offset-from-vector (y x string)
  (let ((moved 0) current)
    (while (and (not (zerop x)) (not (zerop y)))
      (if (not (zerop y))
	  (when (position current "\r\n") (decf y))
	(return (+ moved x)))
      (incf moved))))

;; Commenting to pass compilation w/o warnings
;; (defun haxe-generate-import (for-type)
;;   (interactive (list (if for-type for-type (haxe-suggest-type))))
;;   (save-excursion ))

;; --------------- end my changes ---------------------------------------------

(defcustom haxe-mode-hook nil
  "*Hook called by `haxe-mode'."
  :type 'hook
  :group 'c)

(defun haxe-mode ()
  "Major mode for editing haXe code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `haxe-mode-hook'.

Key bindings:
\\{haxe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table haxe-mode-syntax-table)
  (setq major-mode 'haxe-mode
        mode-name "haXe"
        local-abbrev-table haxe-mode-abbrev-table
        abbrev-mode t)
  (use-local-map haxe-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars haxe-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'haxe-mode)
  ;; For some reason, comment-start-skip has to be set manually.
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  ;; --------------------------- my changes ---------------------------
  
  (c-set-offset 'substatement-open 0)
  (haxe-connect-to-compiler-server 1)
  (local-set-key "." haxe-completion-method)
  (local-set-key "(" 'haxe-hint-paren)
  (local-set-key (kbd "C-c h") 'haxe-electric-help)
  (setq flymake-log-level 0)
  (haxe-flymake-install)
  (haxe-identify-project-root)
  (setq compile-command
        (concat haxe-compiler " " (resolve-project-root) build-hxml))
  (flymake-mode)
  ;; (unless old-flymake-after-change-function
  ;;   (haxe-toggle-flymake-inbetween-saves))
  (ad-activate 'flymake-after-change-function)
  (when (fboundp 'auto-complete-mode)
    ;; TODO: Also need to disable the autocompletion on our side if
    ;; auto-complete is not installed
    (auto-complete-mode 1))
  (add-hook 'kill-buffer-hook 'haxe-kill-network-process)
  (haxe-try-set-ecb-outlines)
  ;; ---------------------------- end my changes ----------------------
  (run-hooks 'c-mode-common-hook 'haxe-mode-hook)
  (c-update-modeline))

(provide 'haxe-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-mode.el ends here.
