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
If this variable is `nil' or `hxswf-genclass-prefix-function'
Then it behaves like `hxswf-genclass-prefix-function' would,
i.e. sub-directory names are taken to be parts of the package
names. For conversion rules see documentation for
`hxswf-genclass-prefix-function'.")

(defcustom hxswf-genclass-prefix-substitution "_"
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

(defun hxswfml-genclass-prefix-function (dir)
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
`hxswf-genclass-prefix-substitution' variable"
  (let ((first-letter " ")
        result)
    (dolist (part (split-string dir "/" t)
                  (mapconcat #'identity (reverse result) "."))
      (setf (aref first-letter 0) (aref part 0))
      (unless (string-match "[[:alpha:]]" first-letter)
        (setf (aref part 0) hxswf-genclass-prefix-substitution))
      (when (> (length part) 1)
        (dotimes (i (1- (length part)))
          (setf (aref first-letter 0) (aref part (1+ i)))
          (unless (string-match "\\w\\|\\." first-letter)
            (setf (aref part (1+ i))
                  hxswf-genclass-prefix-substitution)))
        (when (char-equal (aref part (1- (length part))) ?\.)
          (setf (aref part (1- (length part)))
                hxswf-genclass-prefix-substitution)))
      (push part result))))

(defun hxswfml--int-suffix (word)
  (let ((pos (1- (length word))))
    (while (and
            (> pos -1)
            (member
             (aref word pos)
             '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
      (decf pos))
    (1+ pos)))

(defun hxswfml--ensure-unique-name (name hash)
  (let ((prefix
         (substring name 0 (hxswfml--int-suffix name)))
        (suffix 0) result)
    (when (> (length result) (length prefix))
      (setq suffix
            (string-to-number
             (substring result (length prefix)))))
    (while (gethash (format "%s%d" prefix (incf suffix)) names))
    (puthash (setq result (format "%s%d" prefix (incf suffix))) t)
    result))

(defun hxswfml--pascal-classname (file &optional names)
  "Translates the FILE into PascalCase while also removing
non-alphanumeric characters. Unless the first character is
not a letter, the `hxswf-genclass-prefix-substitution'
is prepended to the name."
  (let ((mask " ")
        (result
         (mapconcat #'capitalize
                    (split-string file "[^[:alnum:]]") "")))
    (setf (aref mask 0) (aref result 0))
    (unless (string-match "[[:alpha:]]" mask)
      (setq result
            (concat hxswf-genclass-prefix-substitution result)))
    (when (and names (gethash result names))
      (setq result (hxswfml--ensure-unique-name result names)))
    result))

(defun hxswfml--underscore-classname (file &optional names kind)
  "Replaces all non-aphanumeric characters in FILE by
`hxswf-genclass-prefix-substitution'. Depending on KIND
will also either upcase, downcase or leave the case of the
remaining characters unchanged. Possible values for KIND are
`mixed' and `upper'. Everything else is treated as downcase.
If first characters is not a letter, then
`hxswf-genclass-prefix-substitution' is prepended to the name.
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
          hxswf-genclass-prefix-substitution)))
    (setf (aref mask 0) (aref result 0))
    (unless (string-match "[[:alpha:]]" mask)
      (setq result
            (concat hxswf-genclass-prefix-substitution result)))
    (when (and names (gethash result names))
      (setq result (hxswfml--ensure-unique-name result names)))
    result))

(defun hxswfml--insert-directory (dir &optional names)
  "Inserts all resources found in DIR directory into
current buffer."
  (let ((ignored (assoc hxswfml-assets-extensions 'ignore)))
    (dolist (file (directory-files dir))
      (unless (or (string= file ".")
                  (string= file "..")
                  (and ignored (string-match ignored file)))
        (if (file-directory-p file)
            (hxswfml--insert-directory file names)
          (hxswfml--insert-resource file dir names))))))

(defun hxswfml--insert-resource (file dir &optional names)
  "Inserts a tag for file, if it matches any of descriptors in
`hxswfml-assets-extensions'"
  (let ((prefix-generator
         (or hxswfml-genclass-prefix
             hxswf-genclass-prefix-function))
        (name-generator hxswfml-genclass-pattern)
        (extensions hxswfml-assets-extensions)
        prefix name kind)
    (setq prefix
          (cond
           ((stringp prefix-generator)
            prefix-generator)
           ((functionp prefix-generator)
            (funcall prefix-generator dir))
           (t ""))
          name
          (cond
           ((eq name-generator 'underscore)
            (hxswfml--underscore-classname file names))
           ((eq name-generator 'underscore-mixed)
            (hxswfml--underscore-classname file names 'mixed))
           ((eq name-generator 'underscore-upper)
            (hxswfml--underscore-classname file names 'upper))
           (t (hxswfml--pascal-classname file names))))
    (catch 't
      (while extensions
        (setq kind (car extensions)
              extensions (cdr extensions))
        (unless (eq (car kind) 'ignore)
          (when (string-match (cdr kind) file)
            (insert "<" (symbol-name (car kind)) "file=\""
                    file "\" class=\"" prefix
                    (if prefix "." "") name "\"/>\n")
            (throw 't nil)))))))

(defun hxswfml--generate-from-directory (dir target)
  "Does the job of `hxswfml-generate-from-directory'"
  (with-temp-file target
    (inert "<lib>")
    (hxswfml--insert-directory dir (make-hash-table))
    (inert "</lib>")))

;;;###autoload
(defun hxswfml-generate-from-directory (dir target)
  "Creates a library file TARGET by recursively searching
directory DIR and appending known types of assets to the
generated file.
The assets are identified by extension, the assoc list
specifying which asset extension match which tags can be
customized through `hxswfml-assets-extensions' variable."
  (interactive "DSelect the directory for generating the library: 
FWhere should I save the generated library? ")
  (catch 't
    (when (file-exists-p target)
      (unless (yes-or-no-p
               (format "File <%s> already exists, overwrite?" target)
               (throw 't nil))))
    (hxswfml--generate-from-directory dir target)))

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
   (expand-file-name "hxswfml-schemas.xml"
                     (file-name-directory load-file-name)))
  (rng-auto-set-schema))

(provide 'hxswfml-mode)

;;; hxswfml-mode.el ends here
