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

;; haxe-compiler-mode is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.0.0 - This is not usable yet.
;;

;;; Usage:
;;


;;; Code:

(require 'cl)
(require 'haxe-log)

(defmacro deflocal (var &rest body)
  (let ((symb var)
        (val (car body))
        (doc (cadr body)))
    `(progn
       (defvar ,symb ,val ,doc)
       (make-local-variable ',symb))))

(deflocal haxe-compiler-in-start 0
  "Position of the beginning of the last message sent to haxe compiler")

(deflocal haxe-compiler-in-end 0
  "Position of the end of the last message sent to haxe compiler")

(deflocal haxe-compiler-out-start 0
  "Position of the beginning of the last message received from haxe compiler")

(deflocal haxe-compiler-out-end 0
  "Position of the end of the last message received from haxe compiler")

(deflocal haxe-compiler "haxe"
  "The path to HaXe compiler")

(deflocal haxe-server-host "127.0.0.1"
  "The host to run HaXe compiler daemon")

(deflocal haxe-server-port 1257
  "The default port to connect to on HaXe compiler daemon")

(deflocal haxe-max-responses 100
  "The number of responses from the compiler server to keep in cache")

(deflocal haxe-last-compiler-response nil
  "This variable is updated by the filter function that reads from the 
connection to HaXe compiler, it's content is the last response received")

(deflocal haxe-server-responses 0
  "The number of responses received so far from haxe compilation server")

(deflocal haxe-received-status 2
  "HaXe compiler will send large completion results in chunks, in order to
accumulate all received chunks we need sort of a state-machine. This variable
holds the status of receiving the info.
	0 - received first chunk,
	1 - received last chunk,
	2 - receiving junk (error messages etc).")

(deflocal haxe-compiler-process "haxe-server"
  "The name given to the HaXe compiler process when started by automake
or auto-completion")

(deflocal haxe-network-process nil
  "The reference to the network connection opened to HaXe complier")

(defvar haxe-compiler-mode-hook nil
  "Hooks run by thaxe-compiler-mode when it starts")

(defvar haxe-compiler-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c \C-c" 'haxe-send-to-server)
    map)
  "Keymap for haxe-compiler major mode")


(defvar haxe-compilation-servers nil
  "The list of all currently active compilation servers
 (their buffers, actually)")

(defvar haxe-network-process-buffer "*haxe-network-process-buffer*"
  "The buffer to hold the network process connecting to HaXe compiler server.
This is needed because otherwise the process get's lost somehow D:")

(defvar haxe-eol "\n"
  "The string used as line separator when building commands to HaXe compiler")

(defvar haxe-response-terminator "</list>\n"
  "This variable is set according to the kind of completion we request
it may be \"</list>\n\" or \"</type>\n\" (first is for dot completion
the second is for paren hint")

;; TODO: this should return to haxe-completion.el once the `haxe-listen-filter'
;; starts making more sense.
(defvar haxe-completion-requested nil
  "This variable is set to T when dot autocompletion starts")

(defun haxe-listen-filter (proc input)
  "Is called by the running HaXe server to report events, if any."
  ;; We are only interested in recording the completion XMLs
  (block nil
    (cond
     ((null input)
      (haxe-log 3 "HaXe compiler sends no input")
      (when (= haxe-received-status 2)
	(setq haxe-last-compiler-response "No input"
	      haxe-completion-requested nil)
	(return-from nil)))
     ((and (= haxe-received-status 2) (not (null input))
	   input (char-equal (aref input 0) ?<))
      (if (or (and (string= (substring input 0 6) "<list>")
		   (string= haxe-response-terminator "</list>\n"))
	      (and (string= (substring input 0 6) "<type>")
		   (string= haxe-response-terminator "</type>\n")))
	  (setq haxe-received-status 0
		haxe-last-compiler-response input)
	(progn
	  (setq haxe-last-compiler-response "Wrong tag"
		haxe-completion-requested nil)
	  (haxe-log 3 "Received wrong result, expected %s, received %s"
		    (substring haxe-response-terminator 0 -1) input)
	  (return-from nil))))
     ((= haxe-received-status 1)
      (setq haxe-last-compiler-response (concat haxe-last-compiler-response input)))
     ((= haxe-received-status 2)
      (haxe-log 3 "Compiler had something to say:

'%s'

But chosen a bad time to do it" input)
      (setq haxe-last-compiler-response nil)
      (return-from nil)))
    
    (if (and (< haxe-received-status 2)
	     (string= (substring haxe-last-compiler-response
				 (- (length haxe-response-terminator)))
		      haxe-response-terminator))
	(setq haxe-received-status 2)
      (setq haxe-received-status 1))

    (haxe-log 3 "filter received: %s %s"
	      haxe-received-status
	      (string= (substring haxe-last-compiler-response
				  (- (length haxe-response-terminator)))
		       haxe-response-terminator))))

;;;###autoload
(defun haxe-connect-to-compiler-server (&optional wait)
  "Starts HaXe compilations server and connects to it.
If WAIT is NIL, will try to connect immediately, otherwise will
wait WAIT seconds.
This function is bound to \\[haxe-connect-to-compiler-server]"
  (interactive
   (let ((wait-i (read-number "Wait seconds before connecting: " 0)))
     (list wait-i)))
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

(defun haxe-send-to-server ()
  "Sends the substring `haxe-compiler-in-start' to
`haxe-compiler-in-end' from the current buffer to the compiler server"
  (interactive)
  (goto-char (point-max))
  (insert "\n")
  (process-send-region haxe-network-process
   (setq haxe-compiler-in-start (goto-char (point-max)))
   (setq haxe-compiler-in-end (point-max)))
  (process-send-eof haxe-network-process))

;;;###autoload
(defun haxe-start-waiting-server (&optional compiler host port)
  "Starts HaXe `haxe-compiler' on `haxe-server-host':`haxe-server-port'
with \"--wait\" for the future requests made by autocompletion
or flymake.
This function is bound to \\[haxe-start-waiting-server]"
  (interactive
   (let ((compiler-i
          (read-string "HaXe compiler: "
                       haxe-compiler t haxe-compiler))
         (host-i
          (read-string "HaXe server host: "
                       haxe-server-host t haxe-server-host))
         (port-i
          (read-number "HaXe server port: " haxe-server-port)))
     (list compiler-i host-i port-i)))
  (unless (called-interactively-p 'interactive)
    (unless compiler (setq compiler haxe-compiler))
    (unless host (setq compiler haxe-server-host))
    (unless port (setq compiler haxe-server-port)))
  (let ((new-buffer
         (get-buffer-create
          (generate-new-buffer-name
           " *haxe-waiting-server*"))))
    (start-process compiler new-buffer compiler
                   "--wait" 
                   (format "%s:%d" host port))
    (bury-buffer new-buffer)
    (switch-to-buffer
     (get-buffer-create "*haxe-interactive-server*"))
    (haxe-compiler-mode)
    (setq haxe-compiler compiler
          haxe-server-host host
          haxe-server-port port)))

(define-derived-mode haxe-compiler-mode fundamental-mode
  "Haxe Compiler interaction mode"
  "Major mode interacting with HaXe compiler server."
  (kill-all-local-variables)
  (erase-buffer)
  (setq major-mode 'haxe-compiler-mode)
  (setq mode-name "HaXe Interactive Compiler")
  (run-hooks 'haxe-compiler-mode-hook))

(provide 'haxe-compiler-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-compiler-mode.el ends here.
