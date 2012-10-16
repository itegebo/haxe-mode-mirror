;;; Commentary:

;; ------------------------------------------------------------------------
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

;; This program is an addition to haxe-mode.el to add auto-completion support
;; to HaXe projects. It requires auto-complete library to function.
;; (http://cx4a.org/software/auto-complete/)

;; haxe-completion is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

;; (eval-when-compile (require 'cl))
(require 'cl)
(require 'haxe-log)
(require 'haxe-project)
(require 'haxe-help)
(require 'xml)

(defcustom haxe-completion-method #'haxe-complete-dot-ac
  "The function to use to perform after-dot completion. This can be either:
`haxe-complete-dot-ac' or `haxe-complete-dot-ido'. `haxe-complete-dot-ac' uses
auto-complete library to show the completion options, `haxe-complete-dot-ido' uses
ido to do the same."
  :type 'function :group 'haxe-mode)

(defcustom haxe-compiler "haxe"
  "The path to HaXe compiler"
  :type 'string :group 'haxe-mode)

(defcustom haxe-server-host "127.0.0.1"
  "The host to run HaXe compiler daemon"
  :type 'string :group 'haxe-mode)

(defcustom haxe-server-port 1257
  "The default port to connect to on HaXe compiler daemon"
  :type 'integer :group 'haxe-mode)

(defvar haxe-compiler-process "haxe-compiler"
  "The name given to the HaXe compiler process when started by automake
or auto-completion")

(defvar haxe-network-process nil
  "The reference to the network connection opened to HaXe complier")

(defvar haxe-network-process-buffer "haxe-network-process-buffer"
  "The buffer to hold the network process connecting to HaXe compiler server.
This is needed because otherwise the process get's lost somehow D:")

(defvar haxe-received-status 2
  "HaXe compiler will send large completion results in chunks, in order to
accumulate all received chunks we need sort of a state-machine. This variable
holds the status of receiving the info.
	0 - received first chunk,
	1 - received last chunk,
	2 - receiving junk (error messages etc).")

(defvar haxe-last-compiler-response nil
  "This variable is updated by the filter function that reads from the 
connection to HaXe compiler, it's content is the last response received")

(defvar haxe-eol "\n"
  "The string used as line separator when building commands to HaXe compiler")

(defvar haxe-response-terminator "</list>\n"
  "This variable is set according to the kind of completion we request
it may be \"</list>\n\" or \"</type>\n\" (first is for dot completion
the second is for paren hint")

(defvar haxe-ac-dot-sources
  '((init . haxe-ac-init)
    (requires . 0)
    (candidates . haxe-ac-candidates)
    (document . haxe-ac-documentation)
    (match . haxe-completion-filter)
    (prefix . haxe-ac-prefix-matcher)
    (symbol . "s"))
  "The source generator for autocompletion needed for interaction 
with auto-complete")

(defvar haxe-last-ac-candidates nil
  "The last time completion was called this variable was updated to
save the last result returnded from a request for completion sent to
HaXe compiler. This variable is set automatically, don't change it")

(defvar documentation-hash (make-hash-table :test #'equal)
  "We store here the documentation strings for the last completion requested")

(defvar haxe-last-ac-candidates-filtered nil
  "The list of completion candidates after the last filter was applied")

(defvar haxe-string-to-complete ""
  "This string is collected while requesting completion")

(defvar haxe-completion-pos -1
  "The position the completion started recording")

(defvar completion-requested nil
  "This variable is set to T when dot autocompletion starts")

(defvar haxe-folding-delimiters '(?\ ?\t ?\n)
  "Characters used to delimit the words when padding a region")

(defvar haxe-folding-terminators '(?\- ?\. ?\, ?\? ?\! ?\; ?\: ?\) ?\] ?\")
  "Characters that terminate words when padding a region")

(defvar haxe-folding-exceptions
  '((?\. . ?\)) (?\. . ?\") (?\. . ?\') (?\. . ?\]) (?\. . ?\.)
    (?\! . ?\)) (?\! . ?\") (?\! . ?\') (?\! . ?\]) (?\! . ?\.)
    (?\! . ?\!) (?\! . ?\?) (?\? . ?\)) (?\? . ?\") (?\? . ?\')
    (?\? . ?\]) (?\? . ?\.) (?\? . ?\!) (?\? . ?\?))
  "Character pairs that should not be split, when word-wrapping a region,
unless there is only one word in the line")

(defun haxe-connect-to-compiler-server (&optional wait)
  "Starts HaXe compilations server and connects to it.
If WAIT is NIL, will try to connect immediately, otherwise will
wait WAIT seconds.
This function is bound to \\[haxe-connect-to-compiler-server]"
  (interactive)
  (haxe-start-waiting-server)
  (unless wait (setq wait 0))
  (let ((old-proc (get-process haxe-compiler-process)))
    (if (and old-proc (equal (process-status old-proc) 'open))
        (setq haxe-network-process old-proc)
      (run-at-time
       wait nil
       #'(lambda ()
           (haxe-log 3 "Trying to connect to HaXe compiler on %s:%s"
                     haxe-server-host haxe-server-port)
           (setq haxe-network-process
                 (make-network-process
                  :name haxe-compiler-process
                  :family 'ipv4
                  :host haxe-server-host
                  :service haxe-server-port
                  :buffer haxe-network-process-buffer
                  :filter #'haxe-listen-filter))
           (haxe-log 3 "Connected to HaXe compiler"))))))

(defun haxe-package ()
  "Get the name of the package of the current file"
  ;; TODO This is a little too naive, we have to also check that the face of the
  ;; word package is also a proper face.
  (let ((bs (buffer-string)))
    (when (string-match "package\\s-\\(.*?\\);" bs)
      (match-string 1 bs))))

(defun haxe-conditional-comps ()
  "Reads conditional compilation settings from `build-hxml'"
  (let ((bs (buffer-string)))
       (when (string-match "hxc:\\s-\\(.*\\)" bs)
	 (match-string 1 bs))))

(defun haxe-build-cwd ()
  "Builds a part of command for HaXe compiler to change current directory to
the `project-root'."
  ;; It's not "/src", when we create the project, there's $source property
  ;; we then have to save that property somewhere and ONLY use "/src" if
  ;; we didn't have it.
  (list "--cwd"
	(concat (expand-file-name (resolve-project-root)) "/src")))

(defun haxe-read-hxml ()
  "Reads the contents of `project-build-command'
SEPARATOR is used to delimit lines read from the file"
  (with-temp-buffer
    (insert-file-contents
     (if (and build-hxml (resolve-project-root))
         (concat (file-name-as-directory (resolve-project-root)) build-hxml)
       (error "You need to specify `project-root' and `build-hxml'")))
    (delete-non-matching-lines "^-cp\\|^-lib\\|^-swf")
    (let (result pos)
      (dolist (i (delete-dups (split-string (buffer-string) haxe-eol)))
	(setq pos (position " " i))
	(if pos 
	    (setq result (cons (substring i 0 pos) result)
		  result (cons (substring i pos) result))
	  (setq result (cons i result))))
      (setq project-build-command (mapconcat #'identity result " "))
      result)))

(defun haxe-class-name (pkg)
  "Generates the fully qualified name of the HaXe class"
  (concat (when pkg (concat pkg "."))
          (file-name-nondirectory
           (file-name-sans-extension (buffer-name)))))

(defun haxe-ac-prefix-matcher ()
  "Loop back until we either find a dot to complete after, or
find nothing and return nil."
  (message "haxe-ac-prefix-matcher")
  (let ((start (point)) (w " ")
        current seen-white face)
    (catch 't
      (while (> start 0)
        (setf current (char-before start)
              (aref w 0) current)
        (cond
         ((string-match "\\w" w)
          (when seen-white (throw 't nil)))
         ((string-match "\\s-" w)
          (setq seen-white t))
         ((char-equal current ?.)
          (setq face (haxe-face-at-point start))
          (if (and (> start 0)
                   (not (char-equal (char-before (1- start)) ?.))
                   (or (null face)
                       (not (member face
                                    '(font-lock-string-face
                                      font-lock-comment-face
                                      font-lock-preprocessor-face)))))
              ;; Need to save here, otherwise the compiler
              ;; will get the old copy of the buffer, which doesn't have
              ;; the dot.
              (progn 
                (save-buffer)
                (throw 't (1+ start)))
            (throw 't nil)))
         (t (throw 't nil)))
        (decf start)) nil)))

;; (defun haxe-ac-prefix-matcher ()
;;   ;; Need to check if we aren't inside a for (i in x..y) loop
;;   (let ((dot-position (re-search-backward "\\.\\(\\s_\\|\\sw\\)*" nil t)))
;;     (message "haxe-ac-prefix-matcher searching for prefix... %s"
;;           (1+ dot-position))
;;     (save-buffer)
;;     (1+ dot-position)))

(defun haxe-ac-init ()
  "This function is called by `auto-complete' when it starts autocompleting"
  (if (and (char-equal (char-before) ?.)
           (not (char-equal (char-before (1- (point))) ?.))
           (not (member (haxe-face-at-point)
                        '(font-lock-string-face
                          font-lock-comment-face
                          font-lock-preprocessor-face))))
      (let ((old-proc (get-process haxe-compiler-process)))
        (when (or (not old-proc)
                  (not (equal (process-status old-proc) 'open)))
          (setq haxe-network-process nil)
          (haxe-connect-to-compiler-server)
          (sleep-for 1)
          (setq old-proc (get-process haxe-compiler-process)))
        (let ((ac-request
               (haxe-build-compile-string
                (haxe-package) (buffer-file-name))))
          (setq haxe-last-ac-candidates nil
                haxe-last-ac-candidates-filtered nil
                haxe-last-compiler-response nil
                haxe-received-status 2)
          (clrhash documentation-hash)
          (process-send-string old-proc ac-request)
          (process-send-string old-proc "\000")
          (process-send-eof old-proc)
          (haxe-log 3 "haxe-ac-init sent request: %s\n completing: %s"
                    ac-request
                    (substring (buffer-string)
                               (max (point-min) (- (point) 10))
                               (point))))
        (with-local-quit
          (with-timeout
              (5 (haxe-log 0 "Failing to fetch all completion options, giving up"))
            (while (not haxe-last-ac-candidates)
              (accept-process-output old-proc)
              (haxe-log 3 "statsus: %s"
                        (when haxe-last-compiler-response
                          (concat
                           (substring haxe-last-compiler-response
                                      0 (min (length haxe-last-compiler-response) 42)) "...")))
              (when (and haxe-last-compiler-response (= haxe-received-status 2))
                (if (string= haxe-response-terminator "</list>\n")
                    (haxe-parse-ac-response haxe-last-compiler-response)
                  (haxe-parse-hint-response haxe-last-compiler-response)))))))
    (setq completion-requested nil)
    haxe-last-ac-candidates))

(defun haxe-build-compile-string (pkg temp-file)
  "Builds `project-build-command'"
  (let ((conditionals (haxe-conditional-comps)))
    (concat haxe-eol
            (mapconcat #'identity (haxe-build-cwd) " ") haxe-eol
            (mapconcat #'identity (haxe-conditional-comps) " ")
            (if conditionals haxe-eol "")
            (mapconcat #'identity (haxe-read-hxml) haxe-eol) haxe-eol
            (concat "-main " (haxe-class-name pkg) haxe-eol)
            (concat "--display " temp-file "@"
                    (number-to-string (1- (point-in-bytes)))) haxe-eol)))

(defun point-in-bytes ()
  (let ((sub (substring (buffer-string) 0 (point))))
    (length (encode-coding-string sub 'utf-8))))

(defun haxe-ac-candidates ()
  "Requests autocompletion candidates and returns them"
  ;; (debug)
  ;; these do exist
  ;; (message "haxe-last-ac-candidates %s" haxe-last-ac-candidates)
  (message "haxe-last-ac-candidates-filtered %s" haxe-last-ac-candidates-filtered)
  haxe-last-ac-candidates-filtered)

(defun haxe-completion-filter (prefix candidates &rest rest)
  (message "haxe-completion-filter %s, rest: %s" prefix rest)
  (when (and (char-equal (char-before (- (point) (length prefix))) ?.)
             (not (char-equal (char-before (- (point) (1+ (length prefix)))) ?.))
             (not (member (haxe-face-at-point)
                          '(font-lock-string-face
                            font-lock-comment-face
                            font-lock-preprocessor-face))))
    (if (= 0 (length prefix))
        (setq haxe-last-ac-candidates-filtered haxe-last-ac-candidates)
      (setq haxe-last-ac-candidates-filtered
            (filter-candidates-exact haxe-last-ac-candidates))))
  ;; (let ((collected (substring (buffer-string) haxe-completion-pos (1- (point)))))
  ;;   (if (string= collected ".")
  ;;       (setq haxe-last-ac-candidates-filtered haxe-last-ac-candidates)
  ;;     (setq haxe-string-to-complete collected
  ;;           haxe-last-ac-candidates-filtered
  ;;           (filter-candidates-exact haxe-last-ac-candidates)
  ;;           ;; (remove-dupes
  ;;           ;;  (nconc (filter-candidates-exact haxe-last-ac-candidates)
  ;;           ;; 	  (filter-candidates-sturdy haxe-last-ac-candidates)
  ;;           ;; 	  (filter-candidates-fuzzy haxe-last-ac-candidates)))
  ;;           ))
  ;;   (message "haxe-completion-filter %s %s %s %s"
  ;;            prefix collected haxe-last-ac-candidates-filtered
  ;;            haxe-last-ac-candidates)
  ;;   haxe-last-ac-candidates-filtered)
  haxe-last-ac-candidates-filtered)

(defun haxe-remove-if! (predicate sequence)
  "Destructively removes conses that match PREDICATE from SEQUENCE.
Returns the list of removed conses."
  (let ((processed sequence)
        processed-head removed
        removed-tail cons-to-remove)
    (while processed
      (if (funcall predicate (car processed))
          (progn
            (setq cons-to-remove processed)
            (rplacd processed-head (cdr processed))
            (setq processed (cdr processed))
            (rplacd cons-to-remove nil)
            (if removed 
                (progn
                  (rplacd removed-tail cons-to-remove)
                  (setq removed-tail (cdr removed-tail)))
              (setq removed cons-to-remove
                    removed-tail cons-to-remove)))
        (setq processed-head processed
              processed (cdr processed))))
    removed))

(defun haxe-levenstain (in-a in-b &optional dist)
  "Finds Levenstain distance from IN-A to IN-B"
  (when (not dist) (setf dist 0))
  (let (temp-string replaced current-char)
    (cond
     ((string= in-a in-b) dist)
     ((< (length in-a) (length in-b))
      (setf temp-string (make-string (1- (length in-b)) ?\ ))
      (dotimes (i (length temp-string)
                  (haxe-levenstain in-a temp-string (1+ dist)))
        (setf current-char (aref in-b i))
        (cond
         ((and (not replaced) (< i (length in-a)))
          (if (char-equal current-char (aref in-a i))
              (setf (aref temp-string i) current-char)
            (setf replaced t)))
         ((not replaced) (setf replaced t))
         (t (setf (aref temp-string i) current-char)))))
     ((< (length in-b) (length in-a))
      (haxe-levenstain in-b in-a dist))
     (t (setf temp-string (make-string (length in-b) ?\ ))
        (dotimes (i (length temp-string)
                    (haxe-levenstain in-a temp-string (1+ dist)))
          (setf current-char (aref in-b i))
          (if (not replaced)
              (if (char-equal current-char (aref in-a i))
                  (setf (aref temp-string i) current-char)
                (setf replaced t
                      (aref temp-string i) (aref in-a i)))
            (setf (aref temp-string i) current-char)))))))

(defun fliter-candidates-levenstain (candidates)
  "Filters the candidates by establishing Levenstein distance from
the `haxe-string-to-complete' to the candidate"
  (let ((hash (make-hash-table))
        (max-distance 0)
        current result)
    (dolist (i candidates
               (dotimes (j max-distance result)
                 (setf current (gethash hash j))
                 (when current
                   (setf result (append result current)))))
      (setf current (haxe-levenstain haxe-string-to-complete i)
            max-distance (max max-distance current)
            (gethash hash current) (cons i (gethash hash current))))))

(defun filter-candidates-exact (candidates)
  "Filters CANDIDATES list by matching the exact beginning of every name
to `haxe-string-to-complete'"
  (let ((result
         (remove-if
          #'(lambda (x)
              (let ((mlen (length haxe-string-to-complete)))
                (or (< (length x) mlen)
                    (not (string= (substring x 0 mlen) haxe-string-to-complete)))))
          candidates)))
    (message "filter-candidates-exact result %s" result) result))

(defun filter-candidates-sturdy (candidates)
  "Filters CANDIDATES list by applying the following rule:

Characters in `haxe-string-to-complete' are taken to be the first
characters of the part constituting a word, such as for example:
\"do_me_a_favor\" or \"doMeAFavor\" corresponds to \"dmaf\" or \"DMAF\"
respectively."
  ;; TODO: consequent uppercase letters are treated incorrectly
  (remove-if
   #'(lambda (x)
       (let ((mlen (length haxe-string-to-complete))
             parts last)
         (dotimes (i (length x))
           (let ((current (aref x i)))
             (when (or (not last)
                       (and (char-equal last ?_) (not (char-equal current ?_)))
                       (and (not (char-equal (upcase last) last))
                            (char-equal (upcase current) current)))
               (setq parts (cons current parts)
                     last current))))
         (if (< mlen (length parts))
             (dotimes (i mlen)
               (setq last (aref haxe-string-to-complete i))
               (unless (char-equal last (aref parts i))
                 (return t))) nil))) candidates))

(defun filter-candidates-fuzzy (candidates)
  "Filters CANDIDATES list to see if all charactes of
`haxe-string-to-complete' are present in all list elements"
  (remove-if
   #'(lambda (x)
       (dotimes (i (length haxe-string-to-complete))
         (unless (position (aref haxe-string-to-complete i) x)
           (return t))))
   candidates))

(defun haxe-ac-documentation (symbol)
  "Requests documentation for SYMBOL from HaXe complier and returns it"
  (gethash symbol documentation-hash))

(defun haxe-complete-dot-ac ()
  "Calls HaXe compiler to get completion for properties.
This function is bound to \\[haxe-complete-dot-ac]"
  ;; There's one annoying thing about autocompletion, if you select it
  ;; from menu, it will delete the dot...
  (interactive)
  (insert ".")
  (let ((face (haxe-face-at-point)))
    (unless
        (or (not (char-equal (char-before (1- (point))) ?.))
            (not (member (haxe-face-at-point)
                         '(font-lock-string-face
                           font-lock-comment-face
                           font-lock-preprocessor-face))))
      (setq haxe-response-terminator "</list>\n"
            haxe-string-to-complete "."
            haxe-completion-pos (1- (point))
            haxe-last-ac-candidates nil
            haxe-last-ac-candidates-filtered nil
            completion-requested t)
      (save-buffer)
      ;; (when (fboundp 'auto-complete)
      ;;   (auto-complete (list haxe-ac-dot-sources)))
      )))

(defun hxc-parse-methods (raw)
  ;; TODO: dummy need to figure out what was this
  )

(defun hxc-modify-by-sig (sig selection)
  ;; TODO: what is this?
  )

(defun hxc-lookup-help (selection methodlist)
  ;; TODO: what is this?
  )

(defun hxc-msg (message)
  ;; TODO: what is this?
  )

(defun hxc-lookup-signature (selection methodlist)
  ;; TODO: what is this?
  )

(defun haxe-complete-dot-ido (raw)
  (let ((methodlist (hxc-parse-methods raw)))
    (when (length methodlist)
      (let ((selection
             (ido-completing-read
              "-> "
              (mapcar (lambda (el) (car el)) methodlist))))
        (forward-char)
        (when selection
          (let ((sig (hxc-lookup-signature selection methodlist))
                (help (hxc-lookup-help selection methodlist)))
            (message  sig)
            (hxc-msg (concat sig "\n\n" help ))
            (insert (hxc-modify-by-sig sig selection))))))))

(defun haxe-hint-paren ()
  "Calls HaXe compiler to get hint for function arguments.
This function is bound to \\[haxe-hint-paren]"
  (interactive)
  (insert "(")
  (let ((face (haxe-face-at-point))
        found)
    (unless (equal face 'font-lock-string-face)
      (save-excursion
        (while (and (not (eobp))
                    (position (char-before) " \t\r\n"))
          (backward-char))
        (unless (eobp)
          (backward-char)
          (when (equal (haxe-face-at-point) 'default)
            (setq found t))))
      (when found
        (setq haxe-response-terminator "</type>\n"
              completion-requested t)
        (haxe-ac-init)))))

(defun haxe-start-waiting-server ()
  "Starts HaXe `haxe-compiler' on `haxe-server-host':`haxe-server-port'
with \"--wait\" for the future requests made by autocompletion
or flymake.
This function is bound to \\[haxe-start-waiting-server]"
  (interactive)
  (unless (get-buffer-process "*haxe-waiting-server*")
    (shell-command
     (concat haxe-compiler " --wait "
             haxe-server-host ":"
             (number-to-string haxe-server-port) "&")
     "*haxe-waiting-server*")))

(defun haxe-parse-ac-response (xml)
  "Parses the completion options supplied by HaXe compiler.
XML has to contain child nodes named \"i\", their \"n\" attribute
is taken to be the name of the field to complete and their child node
\"d\" is taken to be the documentation of the field."
  (condition-case var
      (with-temp-buffer
        (let* ((root (progn (insert xml)
                            (xml-parse-region (point-min) (point-max))))
               (options (car root))
               (is (xml-get-children options 'i))
               completions
               docs)
          (dolist (i is completions)
            (setq completions
                  (cons (cdar (xml-node-attributes i)) completions)
                  docs
                  (cons (haxe-fold-string
                         (haxe-condence-white-string
                          (haxe-replace-all
                           (haxe-trim-string
                            (car (last (car (xml-get-children i 'd)))))
                           [?\t] [?\ ])) 42 3 2) docs))
            (puthash (car completions)
                     (concat (car completions) "\n" (car docs))
                     documentation-hash))
          (message "haxe-parse-ac-response dolist done %s" completions)
          ;; This is unsafe, in case we fail to parse, we kill flymake too...
          (setq completion-requested nil
                haxe-last-ac-candidates completions)))
    (error (haxe-log 0 "Error when parsing completion options %s, %s" var xml))))

(defun haxe-replace-all (source search-for replace-with)
  "Utility function for making multiple replacements in a string.
SOURCE is the string to replace in (not modified)
SEARCH-FOR is an array of characters to search for in SOURCE
REPLACE-WITH is an array of characters that have to be used as replacements
to the characters with the same subscripts in the SEARCH-FOR found in SOURCE.

For example (replace-all \"foo/bar/baz.tar.gz\" [?/ ?.] [?\\\\ ?_]) =>
\"foo\\bar\\baz_tar_gz\"
"
  (with-output-to-string
    (dotimes (i (length source))
      (let* ((current (aref source i))
             (pos (position current search-for)))
        (princ 
         (char-to-string
          (if pos (aref replace-with pos) current)))))))

(defun haxe-exception-p (first-char second-char exceptions)
  "Werifies whether the EXCEPTIONS contains a pair (FIRST-CHAR SECOND-CHAR)"
  (dolist (i exceptions)
    (when (and (char-equal (car i) first-char)
               (char-equal (cdr i) second-char))
      (return t))))

(defun haxe-read-word (input position delimiters ends exceptions)
  "Reads the first word from the INPUT, starting from position. The word
is a substring that may be terminated by any of the ENDS characters, or
before any of DELIMITERS characters. However, if the last character of
the word is the `car' of any of EXCEPTION pairs and the character that follows
that character is that same pairs `cdr', then the word is not terminated
and the process is repeated until the next DELIMITER or END is encountered."
  (let (word char)
    (catch 't
      (while (< position (length input))
        (setq char (aref input position))
        (cond
         ((member char delimiters)
          (if (and (< position (1- (length input)))
                   (haxe-exception-p char (aref input (1+ position)) exceptions))
              (setq word (cons char word))
            (throw 't t)))
         ((member char ends)
          (setq word (cons char word))
          (unless (and (< position (1- (length input)))
                       (haxe-exception-p char (aref input (1+ position)) exceptions))
            (throw 't t)))
         (t (setq word (cons char word))))
        (incf position)))
    (coerce (reverse word) 'string)))

(defun haxe-fold-string-words
  (input max-length &optional pad-left pad-right delimiters ends exceptions)
  "Creates a block of text which has no more than MAX-LENGTH characters
in one line, is padded by PAD-LEFT characters on the left and PAD-RIGHT
characters on the right. The text will break words only when the word is
longer then MAX-LENGTH.
DELIMITERS are the characters which cannot be part of the word.
ENDS are the characters that end a word.
EXCEPTIONS are the pairs of characters (an assoc list) that should never be
split, unless it is the only word on the line."
  (let* ((delimiters (or delimiters haxe-folding-delimiters))
         (ends (or ends haxe-folding-terminators))
         (exceptions
          (or exceptions haxe-folding-exceptions))
         (tab-delimiter-p (member ?\t delimiters))
         (pad-left (or pad-left 0))
         (pad-right (or pad-right 0))
         (pos 0) (line-built 0) word line-has-words next-char)
    (when (> (+ pad-right pad-left) max-length)
      (error "The sum of paddings must be smaller then the line length."))
    (with-output-to-string
      (while (< pos (length input))
        (dotimes (i pad-left)
          (princ " ")
          (incf line-built))
        (setq word (haxe-read-word input pos delimiters ends exceptions))
        (if (<= (+ line-built (length word)) (- max-length pad-right))
            (progn
              (princ word)
              (setq line-has-words t)
              (incf line-built (length word))
              (incf pos (length word))
              (when (and (< pos (1- (length input)))
                         (member (aref input pos) delimiters))
                (setq next-char (aref input pos))
                (cond
                 ((char-equal ?\t next-char)
                  (incf pos tab-width)
                  (incf line-built tab-width))
                 ((char-equal ?\n next-char)
                  (incf pos)
                  (dotimes (i (- max-length line-built))
                    (princ " "))
                  (setq line-built 0 line-has-words nil)
                  (terpri))
                 (t (incf pos)
                    (incf line-built)))
                (princ (char-to-string next-char))))
          (progn
            (if line-has-words
                (dotimes (i (- max-length line-built))
                    (princ " "))
              (progn
                (princ (subseq word 0 (- max-length pad-right line-built)))
                (incf pos (- max-length pad-right line-built))
                (dotimes (i pad-right)
                  (princ " "))))
            (terpri)
            (setq line-built 0 line-has-words nil)))))))

(defun haxe-pad-region (start end width &optional prefix pad-left pad-right)
  "Creates a column from the seclected region between START and END of the
width WIDTH.
PREFIX argument is populated when this function is called interactively.
With default prefix argument, the column will be padded by 1 character on the
right and on the left. If you provide numberical argument other than default,
then you will be prompted to provide the padding for left and right sides.
Non-interactive callers must not provide PREFIX argument if they wish to
specify paddings other then 0.

See also `haxe-folding-delimiters', `haxe-folding-terminators',
`haxe-folding-exceptions' and `haxe-fold-string-words'"
  (interactive "r\nnHow wide should the created columnbe? \nP")
  (if prefix
      (if (equal prefix 4)
          (setq pad-left (read-string "Columns to pad on the left: " prefix)
                pad-right (read-string "Columns to pad on the right: " prefix))
        (setq pad-left prefix pad-right prefix))
    (setq pad-left (or pad-left 0) pad-right (or pad-right 0)))
  (let ((input (haxe-fold-string-words
                (buffer-substring start end) width pad-left pad-right)))
    (kill-region start end)
    (insert input)))

(defun haxe-fold-string (input max-length &optional pad-left pad-right)
  "Folds string producing lines of maximum MAX-LENGTH length"
  (with-output-to-string
    (unless pad-left (setq pad-left 0))
    (unless pad-right (setq pad-right 0))
    (dotimes (i pad-left) (princ " "))
    (let ((offset 0) last-space current last-return)
      (dotimes (i (length input))
        (setq current (aref input i))
        (if (= offset max-length)
            (progn
              (setq last-return t offset 0)
              (unless (position current "\t\r\n ")
                (princ (char-to-string current)))
              (dotimes (i pad-right) (princ " "))
              (princ "\n")
              (dotimes (i pad-left) (princ " ")))
          (cond
           ((position current "\r\n")
            (unless (or last-space last-return)
              (princ " ")
              (incf offset)
              (setq last-return nil)))
           ((and (position current "\t ") last-return))
           (t (princ (char-to-string current))
              (incf offset)
              (setq last-return nil))))
        (setq last-space (position current " \t")))
      (unless last-return (dotimes (i pad-right) (princ " "))))))

(defun haxe-condence-white-string (input)
  "Replaces subsequent white space characters with a single whitespace character"
  (with-output-to-string
    (let (last-space current)
      (dotimes (i (length input))
        (setq current (aref input i))
        (unless (and last-space (position current " \t"))
          (princ (char-to-string current)))
        (setq last-space (position current " \t"))))))

(defun haxe-trim-string (input &rest characters)
  "Removes blanks and CHARACTERS from INPUT on its left and on its right"
  (if input
      (let ((i 0) (e (- (length input) 2))
            (mask (concat " \t\r\n" (or characters "")))
            start-found end-found c ce)
        (catch 't
          (while (>= e i)
            (setq c (aref input i) ce (aref input e))
            (when (and (not start-found) (not (position c mask)))
              (setq start-found i))
            (when (and (not end-found) (not (position ce mask)))
              (setq end-found e))
            (when (and start-found end-found)
              (throw 't (substring input start-found (1+ end-found))))
            (incf i) (decf e)) "")) ""))

(defun haxe-parse-hint-response (xml)
  "Parses the function hint supplied by HaXe compiler."
  (condition-case var
      (let* ((root (with-temp-buffer
                     (insert xml)
                     (xml-parse-region (point-min) (point-max))))
             (options (car root))
             (signature
              (replace-regexp-in-string
               "&lt;" "<"
               (replace-regexp-in-string
                "&gt;" ">"
                (xml-node-children options)))))
        (message "haxe-parse-hint-response %s" signature))
    (setq completion-requested nil)
    (error (haxe-log 0 "Error when parsing completion options %s, %s" var xml))))

(provide 'haxe-completion)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-completion.el ends here.
