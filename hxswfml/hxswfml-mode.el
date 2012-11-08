;;; hxswfml-mode.el --- Mode for HaXe SWFMill files
;; Copyright (C) 2012 Oleg Sivokon

;; Author: Tony Graham <olegsivokon@gmail.com>
;; Created: 12 May 2012
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
