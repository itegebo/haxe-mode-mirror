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

;; This program is an addition to haxe-mode.el it defines logging
;; in haxe-mode
;; (http://cx4a.org/software/auto-complete/)

;; haxe-log is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(defcustom haxe-log-level 3
  "This variable controls verbosity of log messages written by `haxe-mode'
to `*Messages*' buffer"
  :type 'integer :group 'haxe-mode)

(defun haxe-log (level mask &rest args)
  "Sends messages to `*Messages*' buffer. If LEVEL is less or equal to
`haxe-log-level' the message is printed"
  (when (>= haxe-log-level level)
    (apply #'message (cons mask args))))

(provide 'haxe-log)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-log.el ends here.
