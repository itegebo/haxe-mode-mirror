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

(defcustom haxe-help-location "~/programs/haxe-for-emacs/haxe-mode/bin/"
  "The location of HaXe language basic help info pages"
  :type 'string)

(defstruct haxe-help-item
  type
  string
  face)

(defun haxe-string-at-point ()
  (with-output-to-string
    (save-excursion
      (while (equal (face-at-point)
		    'font-lock-string-face)
	(backward-char))
      (forward-char)
      (while (equal (face-at-point)
		    'font-lock-string-face)
	(princ (char-to-string (char-after)))
	(forward-char)))))

(defun haxe-electric-help (thing &rest args)
  (interactive
   (list
    (read-string
     (format "Display help on (%s) "
	     (if (equal (face-at-point)
			'font-lock-string-face)
		 (truncate-string-to-width (haxe-string-at-point) 42)
	       (thing-at-point 'word))))))
  (let ((help-item (make-haxe-help-item)))
    (when (or (not thing) (string= thing ""))
      (setf (haxe-help-item-string help-item)
	    (if (equal (face-at-point)
		       'font-lock-string-face)
		(truncate-string-to-width (haxe-string-at-point) 42)
	      (thing-at-point 'word))
	    (haxe-help-item-face help-item) (face-at-point)
	    (haxe-help-item-type help-item)
	    (let ((thing-face (face-at-point)))
	      (cond
	       ((equal 'font-lock-keyword-face thing-face) 'keyword)
	       ((equal 'font-lock-constant-face thing-face) 'constant)
	       ((equal 'font-lock-variable-name-face thing-face) 'variable)
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
	   (haxe-display-basic-help (haxe-help-item-string help-item))))
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

(provide 'haxe-help)

;;; haxe-help.el ends here.