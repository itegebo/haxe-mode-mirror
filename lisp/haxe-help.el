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

;; This program is an addition to haxe-mode.el for displaying help on various
;; topics particular to haxe-mode

;; haxe-help is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(require 'cl)

(defcustom haxe-help-location
  (concat (file-name-directory load-file-name) "/doc")
  "The location of HaXe language basic help info pages"
  :type 'string :group 'haxe-mode)

(defcustom haxe-help-language "en"
  "This is the language code to use when querying haxe.org for
documentation."
  :type 'string :group 'haxe-mode)

(defcustom haxe-help-url "http://haxe.org/wiki/search"
  "The url to use to search for documentation on types, functions etc."
  :type 'string :group 'haxe-mode)

(defcustom haxe-help-url-builder 'haxe-google-help
  "The function to use when building arguments to query the
`haxe-help-url' for documentation on a topic such as type or function."
  :type 'function :group 'haxe-mode)

(defstruct haxe-help-item type string face)

(defun haxe-face-at-point (&optional position)
  "This is like `face-at-point' except we will only look for faces
which are relevant to haxe-mode. This will also look under overlays
created by minor modes like ispel or highlight current line.
If POSITION is nil, it is set to POINT."
  (interactive)
  (when (null position)
    (setq position (point)))
  (let ((props (text-properties-at position)))
    (catch 't
      (while props
        (when (eql (car props) 'face)
          (throw 't
                 (when (member (cadr props)
                               '(font-lock-string-face
                                 font-lock-keyword-face
                                 font-lock-variable-name-face
                                 font-lock-comment-face
                                 font-lock-preprocessor-face
                                 font-lock-type-face
                                 default))
                   (cadr props))))
        (setq props (cdr props))))))

(defun haxe-string-at-point ()
  (with-output-to-string
    (save-excursion
      (while (equal (haxe-face-at-point)
		    'font-lock-string-face)
	(backward-char))
      (forward-char)
      (while (equal (face-at-point)
		    'font-lock-string-face)
	(princ (char-to-string (char-after)))
	(forward-char)))))

(defun haxe-electric-help (thing &rest args)
  "Provides contextual help for the whatever word the point
is currently standing at.
This function is bound to \\[haxe-electric-help]"
  (interactive
   (list
    (read-string
     (format "Display help on (%s) "
	     (if (equal (haxe-face-at-point)
			'font-lock-string-face)
		 (truncate-string-to-width (haxe-string-at-point) 42)
	       (thing-at-point 'word))))))
  (let ((help-item (make-haxe-help-item)))
    (when (or (not thing) (string= thing ""))
      (setf (haxe-help-item-string help-item)
	    (if (equal (haxe-face-at-point)
		       'font-lock-string-face)
		(truncate-string-to-width (haxe-string-at-point) 42)
	      (thing-at-point 'word))
	    (haxe-help-item-face help-item) (haxe-face-at-point)
	    (haxe-help-item-type help-item)
	    (let ((thing-face (haxe-face-at-point)))
	      (cond
	       ((equal 'font-lock-keyword-face thing-face) 'keyword)
	       ((equal 'font-lock-constant-face thing-face) 'constant)
	       ((equal 'font-lock-variable-name-face thing-face) 'variable)
           ((equal 'font-lock-type-face thing-face) 'type)
	       ((equal 'default thing-face)
		(if (position (char-after) " \t\r\n") 'blank
		  (if (position (aref (thing-at-point 'word) 0) "0123456789")
		      'number
		    'operator)))
	       ((equal 'font-lock-string-face thing-face) 'string)
	       ((equal 'font-lock-comment-face thing-face) 'comment)
	       ((equal 'font-lock-preprocessor-face thing-face) 'preprocessor)))))
    (with-electric-help
     #'(lambda ()
	 (let ((title (format "Help for: %s\n\n" (haxe-help-item-string help-item))))
	   (insert title)
	   (add-text-properties
	    (point-min)
	    (+ (point-min) (length title))
	    '(face bold)))
	 (cond
	  ((equal (haxe-help-item-type help-item) 'number)
	   (haxe-display-number (haxe-help-item-string help-item)))
	  ((equal (haxe-help-item-type help-item) 'string)
	   (haxe-display-string (haxe-help-item-string help-item)))
	  ((equal (haxe-help-item-type help-item) 'keyword)
	   (haxe-display-basic-help (haxe-help-item-string help-item)))
      ((equal (haxe-help-item-type help-item) 'type)
	   (haxe-online-help (haxe-help-item-string help-item))))
	 (shrink-window-if-larger-than-buffer)
	 nil) (format "HaXe help: %s" (haxe-help-item-string help-item)) nil 15)
    (other-window 0)))

(defun haxe-number-binary (n)
  (with-output-to-string
    (while (< 0 n)
      (princ (logand n 1))
      (setq n (ash n -1)))))

(defun haxe-color-string (n)
  (with-output-to-string
    (princ "#")
    (dotimes (i 6)
      (princ (format "%X" (mod n 16)))
      (setq n (ash n -4)))))

(defun haxe-display-number (n)
  (let ((n (if (and (< 2 (length n))
		    (char-equal (aref n 0) ?0)
		    (char-equal (aref n 1) ?x))
	       (string-to-number (substring n 2) 16)
	     (string-to-number n))))
    (insert "Base 10: "
	    (number-to-string n)
	    "\nBase 16: "
	    (format "0x%X" n)
	    "\nBase  2: "
	    (haxe-number-binary n)
	    "\nColor  : ")
    (let ((last-length (1+ (length (buffer-string))))
	  (color (make-face 'color-face)))
      (insert "        ")
      (set-face-background 'color-face (haxe-color-string n))
      (add-text-properties
       last-length
       (+ last-length 8)
       '(face color-face)))
    (insert "\n")))

(defun haxe-string-to-charcodes (s)
  (with-output-to-string
    (princ (aref s 0))
    (setq s (substring s 1))
    (dotimes (i (length s))
      (princ ", ") (princ (aref s i)))))

(defun haxe-string-to-u-string (s)
  (with-output-to-string
    (dotimes (i (length s))
      (princ (format "\\u%04x" (aref s i))))))

(defun haxe-string-to-html-string (s)
  (with-output-to-string
    (dotimes (i (length s))
      (princ (format "&#%04x;" (aref s i))))))

(defun haxe-string-to-x-string (s)
  (with-output-to-string
    (let (current parts)
      (dotimes (i (length s))
	(if (> 0 (multibyte-char-to-unibyte (aref s i)))
	    (progn
	      (setq current (encode-coding-string
			     (char-to-string (aref s i)) 'utf-8))
	      (dotimes (j (length current))
		(princ (format "\\x%02x" (aref current j)))))
	  (princ (format "\\x%02x" (aref s i))))))))

(defun haxe-display-string (s)
  (let ((s (substring s 1 -1)))
    (insert "String       : " s "\n")
    (insert "Charcodes    : " (haxe-string-to-charcodes s) "\n")
    (insert "URI-encode   : " (url-hexify-string s) "\n")
    (insert "\\uXXX-encode : " (haxe-string-to-u-string s) "\n")
    (insert "\\xXX-encode  : " (haxe-string-to-x-string s) "\n")
    (insert "Base64-encode: " (base64-encode-string
			       (encode-coding-string s 'utf-8)) "\n")
    (insert "HTML-encode  : " (haxe-string-to-html-string s) "\n")))

;; Later, when the help has more nodes this will direct to different nodes
(defun haxe-display-basic-help (s)
  (info (expand-file-name (concat haxe-help-location "haxe.info"))
	(buffer-name)))

(defun haxe-wiki-help (url term language)
  ;; can't use url-build-query-string here because haxe.org
  ;; uses semicolns to delimit arguments instead of ampersands.
  (browse-url
   (url-encode-url (concat url "?s=" term ";lang=" language))))

(defun haxe-google-help (url term language)
  "Load custom Google search results, URL parameter is ignored."
  (browse-url
   (concat "http://www.google.com/cse?"
           (url-build-query-string
            `((hl ,language)
              (as_sitesearch "http://haxe.org")
              (q ,term) (cx "008143996983163686627:ohovhuovssi"))))))

(defun haxe-online-help (s)
  "Opens the default browser and sens the query to haxe.org"
  (funcall haxe-help-url-builder haxe-help-url s haxe-help-language))

(provide 'haxe-help)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-help.el ends here.
