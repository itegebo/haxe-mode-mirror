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

;; haxe-utils is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(require 'cl)

;; Macros

(defmacro deflocal (var &rest body)
  "Declares a variable that must be local to the buffer, where it is declared."
  (let ((symb var)
        (val (car body))
        (doc (cadr body)))
    `(progn
       (defvar ,symb)
       (set (make-local-variable ',symb) ,val)
       (put ',symb 'variable-documentation ,doc))))

;; Defines (setf (local variable &optional buffer) value) overload
;; for `setf' Will set the variable defined locally in a buffer.
(gv-define-expander local
  (lambda (do &rest args)
    (let ((varname (first args))
          (buffer (or (second args) '(current-buffer)))
          (value (funcall do nil #'identity)))
      `(with-current-buffer ,buffer
         (if (boundp ',varname)
             (setq ,varname ,value)
           (error "Unbound buffer-local variable `%s'" ',varname))))))

(defmacro getlocal (var)
  (let ((local (list 'quote var)))
  `(if (boundp ,local) (symbol-value ,local)
     (error "Unbound buffer-local variable `%s'" ,local))))

(defmacro haxe-local-init* (varspecs)
  (declare (indent 3))
  (append '(progn)
          (loop for (key value) in varspecs by #'cdr
                collect
                (list 'set (list 'make-local-variable
                                 (list 'quote key)) value))))

(defun haxe-get-buffer-property (buffer property)
  "Pops to BUFFER, reads the value of the PROPERTY and returns it."
  (let ((result
         (with-current-buffer buffer
           (symbol-value property))))
    result))

(defun haxe-set-buffer-property (buffer &rest proplist)
  "Pops to BUFFER and sets properties in parallel, similar to `pset'."
  (let ((result
         (with-current-buffer buffer
           (loop for (property value) on proplist by #'cddr
                 do (set property value)
                 finally (return value)))))
    result))

(defmacro haxe-buffer-property (buffer property)
  `(haxe-get-buffer-property ,buffer ',property))
(defalias 'haxe-pbget #'haxe-buffer-property)

(defmacro haxe-buffer-pset-property (buffer &rest proplist)
  `(haxe-set-buffer-property
    ,buffer
    ,@(loop for (key value) on proplist by #'cddr
            nconc (list (list 'quote key) value))))
(defalias 'haxe-pbset #'haxe-buffer-pset-property)

(defmacro haxe-buffer-setf-property (buffer &rest proplist)
  `(with-current-buffer ,buffer
     ,@(list (append '(setf) proplist))))
(defalias 'haxe-pbsetf #'haxe-buffer-setf-property)

;; Strings

(defun haxe-string-between (string start end)
  (and (haxe-string-ends-with string end)
       (haxe-string-starts-with string start)))

(defun haxe-string-ends-with (string end)
  (and (>= (length string) (length end))
       (string= (substring string (- (length end))) end)))

(defun haxe-string-starts-with (string start)
  (and (>= (length string) (length start))
       (string= (substring string 0 (length start)) start)))

(defun haxe-split-string (string char &optional omit-nils)
  "Like `split-string' but simpler. CHAR is a character, not a regexp."
  (let ((start 0) (end 0) (len (length string))
        backref result current)
    (while (< end len)
      (setq current (aref string end))
      (when (char-equal current char)
        (when (or (not omit-nils) (/= start end))
          (if backref
              (setf (cdr result) (list (substring string start end))
                    result (cdr result))
            (setq backref (list (substring string start end))
                  result backref)))
        (setq start (1+ end)))
      (incf end))
    (when (or (not omit-nils) (/= start end))
      (if backref
          (setcdr result (list (substring string start end)))
        (setq backref (list (substring string start end)))))
    backref))

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

(defun haxe-condence-white-string (input)
  "Replaces subsequent white space characters with a single whitespace character"
  (with-output-to-string
    (let (last-space current)
      (dotimes (i (length input))
        (setq current (aref input i))
        (unless (and last-space (position current " \t"))
          (princ (char-to-string current)))
        (setq last-space (position current " \t"))))))

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

(defun haxe-replace-char (string char-a char-b)
  "Replaces destructively CHAR-A with CHAR-B in STRING.
 Because there's no function in eLisp to do this"
  (loop for i from 0 below (length string)
        for c = (aref string i)
        do (when (char-equal c char-a)
             (aset string i char-b))
        finally (return string)))

(defun haxe-replace-string (string string-a string-b)
  "Because there's no function in eLisp to do this."
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

;; TODO: this is a bad name, rename to something like replace-chars-in-string
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

;; Sequences

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

;; Datastructures

(defclass haxe-trie ()
  ((trie :initarg :trie
         :initform nil
         :type (or null hash-table)
         :documentation "The actual trie data.")
   (keys :initarg :keys
         :initfrom nil
         :type (or null hash-table)
         :documentation "The kyes (hash-tables) pointing to the 
trie leafs which have values"))
  "The class for manipulating prefix tries")

(defun haxe-update-trie (trie alist)
  "Replaces all TRIE's keys with new values from ALIST"
  (loop for (key . value) in alist
        with trie-keys = (oref trie keys)
        for table = (gethash key trie-keys)
        do (if table
               (puthash t value table)
             (error "Key `%s' is not in the trie" key))
        finally (return trie)))

(defun haxe-build-trie (alist)
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
        (return (make-instance 'haxe-trie
                               :trie trie-data
                               :keys trie-keys))))

(defun haxe-replace-trie (string trie)
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

(provide 'haxe-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-utils.el ends here.
