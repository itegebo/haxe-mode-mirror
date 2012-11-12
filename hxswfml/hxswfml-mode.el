;;; hxswfml-mode.el --- Mode for HaXe SWFMill files
;; Copyright (C) 2012 Oleg Sivokon

;; Author: Oleg Sivokon <olegsivokon@gmail.com>
;; Created: 08 November 2012
;; Keywords: languages, hxswfml, haxe, xml

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides major mode `hxswfml-mode'
;; for editing HaXe SWFMill files.

;;; Code:

(defcustom hxswfml-compiler "hxswfml"
  "The location of HXSwfML executable"
  :type 'string
  :group 'hxswfml-mode)

(defcustom hxswfml-genclass-pattern 'pascal
  "The way to create class names when generating library files
from assets.
  `pascal' means that the names will start with uppercase and
      use uppercase letter for each subsequent word.
  `underscore' means that the parts of the class name are
      delimited by underscores and converted to lowercase.
  `underscore-upper' similar to underscore, but the name is 
      converted to all upercase.
  `underscore-mixed' keeps the case the same as it was in
      original file names.
The default is `pascal'.")

(defcustom hxswfml-genclass-prefix nil
  "This variable specifies how to prefix generated class
names when automatically generating them. Possible values are:
  Any string - that string is prepended to the generated name.
  A function, this function is called with the file name
     relative to the directory, where the generation started.
If this variable is `nil' or `hxswfml-genclass-prefix-function'
Then it behaves like `hxswfml-genclass-prefix-function' would,
i.e. sub-directory names are taken to be parts of the package
names. For conversion rules see documentation for
`hxswfml-genclass-prefix-function'.")

(defcustom hxswfml-genclass-prefix-substitution "_"
  "The character used to replace invalid characters in
generated prefices for class names"
  :type 'string
  :group 'hxswfml-mode)

(defcustom hxswfml-assets-extensions
  '((bitmapdata . "\\.\\(png\\|jpe?g\\|gif\\)$")
    (sound . "\\.mp3$")
    (font . "\\.ttf$")
    (bytearray . "[^.]+\\..+")
    (ignore . "~$\\|\\(^\\.\\(svn\\|git\\)$\\)"))
  "The list of tags and the file extension for generating
library files."
  :type 'list
  :group 'hxswfml-mode)

(defcustom hxswfml-default-library-file "lib.xml"
  "The default name for library when generating resources
library from directory, saved in the same directory as the
resources."
  :type 'string
  :group 'hxswfml-mode)

(defcustom hxswfml-default-superclasses
  '((bitmapdata . "flash.display.BitmapData")
    (sound . "flash.media.Sound")
    (font . "flash.text.Font")
    (bytearray . "flash.utils.ByteArray"))
  "The superclasses for classes being generated, see also
`hxswfml-assets-extensions'."
  :type 'list
  :group 'hxswfml-mode)

(defvar hxswfml-install-dir
  (file-name-directory load-file-name)
  "The directory whre HaXe SWFMill mode is installed")

(defvar hxswfml-genclass-prefix-function
  #'hxswfml--genclass-prefix-function
  "The default function for processing directory names and
creating suffices based on them")

(defvar hxswfml-cli-options
  '(xml2swf xml2lib xml2swc xml2abc abc2swf abc2swc
            abc2xml abc2hxm xml2hxm ttf2swf ttf2hx
            ttf2path flv2swf)
  "Commands hxswfml understands")

(defvar hxswfml-class-template nil
  "This variable is caching the contents of the class template
when generating library classes. You shouldn't need to change
its value.")

(defvar hxswfml-genclass-trie nil
  "This variable is set to the trie for replacing in template
for generating classes. You shouldn't need to set it yourself.")

(defclass hxswfml-trie ()
  ((trie :initarg :trie
         :initform nil
         :type (or null hash-table)
         :documentation "The actual trie data.")
   (keys :initarg :keys
         :initfrom nil
         :type (or null hash-table)
         :documentation "The kyes (hash-tables) pointing to the 
trie leafs which have values")))

(defun hxswfml--genclass-prefix-function (dir)
  "Converts the directory name into a package name using these
rules:
  - split the directory name on slashes ignoring empty results.
  - process every part as follows:
    * if it starts with non-letter, replace the first character
      with underscore.
    * for every subsequent character, if it is not a letter or
      a digit or a full stop, replace it with underscore.
    * if the full stop is the last character, it is replaced
      with underscore.
You may change what character is used for replacing invalid
characters in the prefix by customizing
`hxswfml-genclass-prefix-substitution' variable"
  (let ((first-letter " ")
        result)
    (dolist (part (split-string dir "/" t)
                  (mapconcat #'identity (reverse result) "."))
      (setf (aref first-letter 0) (aref part 0))
      (unless (string-match "[[:alpha:]]" first-letter)
        (setf (aref part 0) hxswfml-genclass-prefix-substitution))
      (when (> (length part) 1)
        (dotimes (i (1- (length part)))
          (setf (aref first-letter 0) (aref part (1+ i)))
          (unless (string-match "\\w\\|\\." first-letter)
            (setf (aref part (1+ i))
                  (aref hxswfml-genclass-prefix-substitution 0))))
        (when (char-equal (aref part (1- (length part))) ?\.)
          (setf (aref part (1- (length part)))
                (aref hxswfml-genclass-prefix-substitution 0))))
      (push part result))))

(defun hxswfml--int-suffix (word)
  "Returns position before the last digit in the WORD."
  (let ((pos (1- (length word))))
    (while (and
            (> pos -1)
            (member
             (aref word pos)
             '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
      (decf pos))
    (1+ pos)))

(defun hxswfml--ensure-unique-name (name hash)
  "Ensure that the NAME is unique in HASH, i.e. generate a new
name, if one is in hash and return it. HASH is updated with the
new name."
  (if (gethash name hash)
      (let ((prefix
             (substring name 0 (hxswfml--int-suffix name)))
            (suffix 0) result)
        (when (> (length result) (length prefix))
          (setq suffix
                (string-to-number
                 (substring result (length prefix)))))
        (while (gethash (format "%s%d" prefix (incf suffix)) names))
        (puthash (setq result (format "%s%d" prefix (incf suffix)))
                 t hash)
        result)
    (puthash name t hash) name))

(defun hxswfml--pascal-classname (file &optional names)
  "Translates the FILE into PascalCase while also removing
non-alphanumeric characters. Unless the first character is
not a letter, the `hxswfml-genclass-prefix-substitution'
is prepended to the name."
  (let ((mask " ")
        (result
         (mapconcat #'capitalize
                    (split-string file "[^[:alnum:]]") "")))
    (setf (aref mask 0) (aref result 0))
    (unless (string-match "[[:alpha:]]" mask)
      (setq result
            (concat hxswfml-genclass-prefix-substitution result)))
    (when names
      (setq result (hxswfml--ensure-unique-name result names)))
    result))

(defun hxswfml--underscore-classname (file &optional names kind)
  "Replaces all non-aphanumeric characters in FILE by
`hxswfml-genclass-prefix-substitution'. Depending on KIND
will also either upcase, downcase or leave the case of the
remaining characters unchanged. Possible values for KIND are
`mixed' and `upper'. Everything else is treated as downcase.
If first characters is not a letter, then
`hxswfml-genclass-prefix-substitution' is prepended to the name.
If NAMES (a hash-map) already contains the name this function
would have generated otherwise, will append an integer, while
firstly searching for an integral part at the end of the generated
name."
  (let ((mask " ")
        (result
         (mapconcat
          #'identity
          (mapcar 
           (cond
            ((eq kind 'mixed) #'identity)
            ((eq kind 'uper) #'upcase)
            (t #'downcase))
           (split-string file "[^[:alnum:]]"))
          hxswfml-genclass-prefix-substitution)))
    (setf (aref mask 0) (aref result 0))
    (unless (string-match "[[:alpha:]]" mask)
      (setq result
            (concat hxswfml-genclass-prefix-substitution result)))
    (when names
      (setq result (hxswfml--ensure-unique-name result names)))
    result))

(defun hxswfml--insert-directory (dir &optional names prefix genclass)
  "Inserts all resources found in DIR directory into
current buffer."
  (let ((ignored (cdr (assoc 'ignore hxswfml-assets-extensions)))
        (prefix
         (or prefix
             (file-name-base (directory-file-name dir)))))
    (dolist (file (directory-files dir))
      (unless (or (string= file ".")
                  (string= file "..")
                  (and ignored (string-match ignored file)))
        (if (file-directory-p (expand-file-name file dir))
            (hxswfml--insert-directory
             (expand-file-name file dir) names
             (concat prefix "/" file) genclass)
          (hxswfml--insert-resource file prefix names genclass))))))

(defun hxswfml--insert-resource (file dir &optional names genclass)
  "Inserts a tag for file, if it matches any of descriptors in
`hxswfml-assets-extensions'"
  (let ((prefix-generator
         (or hxswfml-genclass-prefix
             hxswfml-genclass-prefix-function))
        (name-generator hxswfml-genclass-pattern)
        (extensions hxswfml-assets-extensions)
        prefix name kind)
    (setq prefix
          (cond
           ((stringp prefix-generator)
            prefix-generator)
           ((fboundp prefix-generator)
            (funcall prefix-generator dir))
           (t nil))
          name
          (cond
           ((eq name-generator 'underscore)
            (hxswfml--underscore-classname
             (file-name-sans-extension file) names))
           ((eq name-generator 'underscore-mixed)
            (hxswfml--underscore-classname
             (file-name-sans-extension file) names 'mixed))
           ((eq name-generator 'underscore-upper)
            (hxswfml--underscore-classname
             (file-name-sans-extension file) names 'upper))
           (t (hxswfml--pascal-classname
               (file-name-sans-extension file) names))))
    (catch 't
      (while extensions
        (setq kind (car extensions)
              extensions (cdr extensions))
        (unless (eq (car kind) 'ignore)
          (when (string-match (cdr kind) file)
            (when genclass
              (hxswfml-class-from-template 
               genclass (concat dir file) name (car kind)))
            (insert "<" (symbol-name (car kind))
                    " file=\"" (if dir (concat dir "/") "")
                    file "\" class=\"" 
                    (if prefix (concat prefix ".") "")
                    name "\"/>\n")
            (throw 't nil)))))))

(defun hxswfml--generate-from-directory (dir target genclass)
  "Does the job of `hxswfml-generate-from-directory'"
  (with-temp-file target
    (insert "<lib>\n")
    (hxswfml--insert-directory
     dir (make-hash-table :test #'equal) nil genclass)
    (insert "</lib>")))

(defun hxswfml--file-package (name separator)
  "Returns the directory part of the NAME with the directory
separator characters replaced by SEPARATOR character."
  (hxswfml-replace-char
   (substring
    name 0
    (or (position ?\/ name :from-end t) 0)) ?\/ separator))

(defun hxswfml--ensure-directory (dir)
  (unless (and (file-exists-p dir) (file-directory-p dir))
    ;; if it was a file, where we wanted to create a directory
    ;; then let it throw.
    (make-directory dir t)) dir)

(defun hxswfml-replace-char (string char-a char-b)
  "Because there's no function in eLisp to do this"
  (loop for i from 0 below (length string)
        for c = (aref string i)
        do (when (char-equal c char-a)
             (aset string i char-b))
        finally (return string)))

(defun hxswfml-replace-string (string string-a string-b)
  "Because there's no function in eLisp to do this"
  (loop for i from 0 upto
        (- (length string) (length string-a))
        for c = (aref string i)
        with alen = (length string-a)
        with result = nil
        with last = 0
        do (loop for j from i below (+ i alen)
                 do (unless
                        (char-equal
                         (aref string-a (- j i))
                         (aref string j))
                      (return))
                 finally
                 (setq result
                       (cons (substring string last (- j alen)) result)
                       i (1- j) last j))
        finally
        (return
         (if result 
             (mapconcat
              #'identity
              (reverse (cons (substring string last) result)) string-b)
           string))))

(defun hxswfml-replace-trie (string trie)
  "Searches for all matches in STRING from TRIE and replaces them"
  (loop for i from 0 below (length string)
        for c = (aref string i)
        for branch = (gethash c trie)
        with result = nil
        with last-pos = 0
        with slen = (length string)
        do (when branch 
             (loop for j from (1+ i) upto slen
                   ;; can do upto here because will certainly
                   ;; exit sooner then the one after last
                   ;; character
                   for cj = (aref string j)
                   for rep-potential = (gethash t branch)
                   with replacement = nil
                   with rep-pos = j
                   do (progn
                        (when rep-potential
                          (setq replacement rep-potential rep-pos j))
                        (setq branch (gethash cj branch))
                        (when (or (not branch) (= (1+ j) slen))
                          (when (= (1+ j) slen)
                            (setq rep-potential (gethash t branch)
                                  replacement (or rep-potential replacement)
                                  rep-pos (1+ j) j (1+ j)))
                          (when replacement
                            (setq result
                                  (cons
                                   (concat
                                    (substring string last-pos i)
                                    replacement)
                                   result)
                                  i (1- rep-pos) last-pos j))
                          (return)))))
        finally
        (return
         (reduce #'concat
                 (reverse (cons (substring string last-pos) result))))))

(defun hxswfml-build-trie (alist)
  "Builds a trie (a list, containing number of hash-maps, each hash-map
uses single character for a key, except for `t' symbol, which, if present
as a key is the key for the value one has to substitute with."
  (loop for (key . value) in alist
        with trie-data = (make-hash-table)
        with trie-keys = (make-hash-table :test #'equal)
        for leaf =
        (reduce (lambda (branch c)
                  (or (gethash c branch)
                      (puthash c (make-hash-table) branch)))
                key :initial-value trie-data)
        do (progn
             (puthash t value leaf)
             (puthash key leaf trie-keys))
        finally
        (return (make-instance 'hxswfml-trie
                               :trie trie-data
                               :keys trie-keys))))

(defun hxswfml-update-trie (trie alist)
  "Replaces all TRIE's keys with new values from ALIST"
  (loop for (key . value) in alist
        with trie-keys = (oref trie keys)
        for table = (gethash key trie-keys)
        do (if table
               (puthash t value table)
             (error "Key `%s' is not in the trie" key))
        finally (return trie)))

;; TODO: This may be useful as a separate package.
(defun hxswfml-class-from-template (save-in file class-name tag)
  "Generates the HaXe class with the name CLASS-NAME in directory
SAVE-IN followed by the package name of the class, inferred from the
FILE name (the prefix before the last slash).
What super-class is chosen for this class is decided based on the
value of TAG and it's corresponding value from
`hxswfml-default-superclasses'"
  (unless hxswfml-class-template
    (with-temp-buffer
      (insert-file-contents-literally
       (expand-file-name "templates/lib.tpl" hxswfml-install-dir))
      (setq hxswfml-class-template (buffer-string))))
  (let* ((package (hxswfml--file-package file ?\.))
         (superclass (cdr (assoc tag hxswfml-default-superclasses)))
         (dir-with-package
          (concat save-in
                  (hxswfml-replace-char
                   (format "%s" package) ?\. ?\/))))
    (if hxswfml-genclass-trie
        (hxswfml-update-trie
         hxswfml-genclass-trie
         `(("$package" . ,package)
           ("$super_class" . ,superclass)
           ("$class" . ,class-name)))
      (setq hxswfml-genclass-trie
            (hxswfml-build-trie
             `(("$package" . ,package)
               ("$super_class" . ,superclass)
               ("$class" . ,class-name)))))
    (with-temp-file
        (expand-file-name
         (concat class-name ".hx")
         (hxswfml--ensure-directory dir-with-package))
      (message "Generating outline: <%s/%s>"
               dir-with-package (concat class-name ".hx"))
      (insert (hxswfml-replace-trie
               hxswfml-class-template
               (oref hxswfml-genclass-trie trie))))))

;;;###autoload
(defun hxswfml-generate-from-directory
  (dir &optional lib-file generate-classes)
  "Creates a library file TARGET by recursively searching
directory DIR and appending known types of assets to the
generated file.
The assets are identified by extension, the assoc list
specifying which asset extension match which tags can be
customized through `hxswfml-assets-extensions' variable.
If GENERATE-CLASSES is `t', then the classes are generated
for each resource mentioned in the generated library file.
All class-files are thus saved in sub-directory of parent
directory of DIR named \"generated\".
However, if it is a directory, then the classes are generated
in that directory.
The file name of the library being generated can be customized
by setting `hxswfml-default-library-file' variable.
The superclasses for generated classes can be customized through
setting `hxswfml-default-superclasses' variable."
  (interactive 
   (let* ((dir-i
           (read-directory-name
            "Select the directory containing assets: "))
          (lib-file-i
           (completing-read
            "Library XML will be generated in: "
            (list (concat (file-name-directory
                           (directory-file-name dir-i))
                          hxswfml-default-library-file))
            nil t (concat (file-name-directory
                           (directory-file-name dir-i))
                          hxswfml-default-library-file)))
          genclass-i)
     (when (yes-or-no-p "Generate outlines? ")
       (setq genclass-i
             (read-directory-name
              "Directory to generate outlines: "
              (concat (file-name-directory
                       (directory-file-name dir-i))
                      "generated/"))))
     (list dir-i lib-file-i genclass-i)))
  (let ((target
         (concat
          (file-name-directory
           (file-name-nondirectory dir))
          hxswfml-default-library-file)))
    (catch 't
      (when (file-exists-p target)
        (unless
            (yes-or-no-p
             (format "File <%s> already exists, overwrite? "
                     target))
          (throw 't nil)))
      (message "Generating classes in <%s>" generate-classes)
      (hxswfml--generate-from-directory
       dir target generate-classes)
      (find-file target)
      (hxswfml-mode))))

(defun hxswfml-validate-files ()
  "Searches for file paths in the current buffer and tries to
resolve them relatively to the file displayed in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((last-point (point)) token found-file)
      (while (< (point) (point-max))
        (setq last-point (forward-word)
              token (buffer-substring-no-properties
                     (max (point-min) (- (point) 5))
                     (point)))
        (when (string-match "\\<file$" token)
          (unless (char-equal
                   (or (char-after) ?x) ?=)
            (forward-whitespace 1))
          (when (char-equal (or (char-after) ?x) ?=)
            (forward-char)
            (unless (or
                     (char-equal
                      (or (char-after) ?x) ?\")
                     (char-equal
                      (or (char-after) ?x) ?\'))
              (forward-whitespace 1))
            (setq token
                  (buffer-substring-no-properties
                   (point) (progn (forward-sexp) (point))))
            (message "found file: <%s>" token)))))))

;;;###autoload
(define-derived-mode hxswfml-mode nxml-mode "HaXe SWFMill"
  "Major mode for editing HaXe SWFMill files."
  (add-to-list
   'rng-schema-locating-files
   (expand-file-name "hxswfml-schemas.xml" hxswfml-install-dir))
  (rng-auto-set-schema))

(provide 'hxswfml-mode)

;;; hxswfml-mode.el ends here
