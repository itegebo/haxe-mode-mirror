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
(require 'eieio)
(require 'haxe-log)

(defmacro deflocal (var &rest body)
  (let ((symb var)
        (val (car body))
        (doc (cadr body)))
    `(progn
       (set (make-local-variable ',symb) ,val)
       (put ',symb 'variable-documentation ,doc))))

(deflocal haxe-compiler-in-start 0
  "Position of the beginning of the last message sent to haxe compiler")

(deflocal haxe-compiler-in-end 0
  "Position of the end of the last message sent to haxe compiler")

(deflocal haxe-compiler-out-start 0
  "Position of the beginning of the last message received from haxe compiler")

(deflocal haxe-compiler-out-end 0
  "Position of the end of the last message received from haxe compiler")

(deflocal haxe-network-idle-timer nil
  "The idle timer that watches after the connection to the compilation
server and tries to bring it up, if it was broken")

(defvar haxe-compiler-default "haxe"
  "The default haxe compiler to use")

(defvar haxe-host-default "127.0.0.1"
  "The default host to connect to for haxe compiler")

(defvar haxe-port-default 1257
  "The default port on which to connect to haxe compiler")

(defvar haxe-max-responses 100
  "The number of responses from the compiler server to keep in cache")

(defconst haxe-compiler-process "haxe-server"
  "The name given to the HaXe compiler process when started by automake
or auto-completion")

(defconst haxe-times-to-reconnect 10
  "How many times should the idle timer try to reconnect before considering
the connection to be dead")

(defvar haxe-compiler-mode-hook nil
  "Hooks run by thaxe-compiler-mode when it starts")

(defvar haxe-compiler-mode-map nil
  "Keymap for haxe-compiler major mode")
(unless haxe-compiler-mode-map
  (setq haxe-compiler-mode-map
        (let ((map (make-keymap)))
          (define-key map "\C-c\C-c" #'haxe-send-to-server)
          (define-key map "\C-c\C-r" #'haxe-reconnect)
          map)))

(defvar haxe-connections nil
  "The list of all currently active compilation servers
 (the list of `haxe-connection')")

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

(defface haxe-compiler-in
  `((t (:foreground "pink")))
  "Face for highlighting the text sent to haxe compiler server."
  :group 'haxe-mode)

(defface haxe-compiler-out
  `((t (:foreground "mint")))
  "Face for highlighting the text received from haxe compiler server."
  :group 'haxe-mode)

(defface haxe-compiler-pending
  `((t (:foreground "white")))
  "Face for highlighting the text pending to be sent haxe compiler server."
  :group 'haxe-mode)

(defclass haxe-connection ()
  ((host :initarg :host
         :initform "127.0.0.1"
         :type string
         :documentation "Host this connection is working with.")
   (port :initarg :port
         :initform 1257
         :type integer
         :documentation "Port this connection is working with.")
   (compiler :initarg :compiler
             :initform "haxe"
             :type string
             :documentation "Path to the compiler executable.")
   (process :initarg :process
            :type process
            :documentation "The process of this connection.")
   (connection :initarg :connection
               :initfrom nil
               :type (or null process)
               :documentation
               "The connection created to this instance of server.")
   (reconnects :initfrom 0
               :type integer
               :documentation
               "How many times this connection has tried to reconnect.")
   (connection-state
    :initform 'off
    :type symbol
    :documentation "The state of this connection, possible values are:
- off (not connected)
- error (tried to connect and failed)
- open (connection established, idle)
- busy (connection established, communicating with the server).")
   (receive-state
    :initform 'nothing
    :type symbol
    :documentation "When receiving messages form server this slot can be:
- nothing (nothing received yet)
- junk (received input, but couldn't decipher it)
- incomplete (seems like receiving a valid message, but not all parts received yet)
- full (finished receiving message).")
   (message :initform nil
            :type (or null string)
            :documentation "The message received from server.")
   (request :initform nil
            :type (or null string)
            :documentation "The requiest sent to server.")
   (buffers :initarg :buffers
            :initform nil
            :type list
            :documentation "All buffers who share this connection.")
   (filter :initarg :filter
           :initform nil
           :type symbol
           :documentation "The filter function for this connection
 (used in `make-network-process').")
   (sentinel :initarg :sentinel
             :initform nil
             :type symbol
             :documentation "The sentinel function for this connection
 (used in `make-network-process')."))
  "This class describes the shared connection to the HaXe compiler server")

(defmacro haxe-with-connection (spec &rest body)
  (let ((con (car spec))
        (slots (cadr spec))
        (buffer (or (caddr spec) '(current-buffer))))
    `(let ((,con (haxe-connection-for-buffer ,buffer)))
       (with-slots ,slots ,con
         ,@body))))

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

(defun haxe-listen-filter (proc input)
  "Is called by the running HaXe server to report events, if any."
  ;; We are only interested in recording the completion XMLs
  (message "filter <%s>" input)
  (block nil
    (cond
     ((null input)
      (haxe-log 3 "HaXe compiler sends no input")
      (when (= haxe-received-status 2)
        (setq haxe-last-compiler-response "No input"
              haxe-completion-requested nil)
        (return-from nil)))
     ((and (= haxe-received-status 2)
           input
           (char-equal (aref input 0) ?<))
      (if (or (haxe-string-between input "<list>" "</list>\n")
           (haxe-string-between input "<type>" "</type>\n"))
          (setq haxe-received-status 0
                haxe-last-compiler-response input)
        (progn
          (setq haxe-last-compiler-response "Wrong tag"
                haxe-completion-requested nil)
          (haxe-log 3 "Received wrong result, expected %s, received %s"
                    (substring haxe-response-terminator 0 -1) input)
          (return-from nil))))
     ((= haxe-received-status 1)
      (setq haxe-last-compiler-response
            (concat haxe-last-compiler-response input)))
     ((= haxe-received-status 2)
      (haxe-log 3 "Compiler had something to say:

'%s'

But chosen a bad time to do it" input)
      (setq haxe-last-compiler-response nil)
      (return-from nil)))
    
    (if (and (< haxe-received-status 2)
             (haxe-string-ends-with input haxe-response-terminator))
        (setq haxe-received-status 2)
      (setq haxe-received-status 1))

    (haxe-log 3 "filter received: %s %s"
              haxe-received-status
              (haxe-string-ends-with input haxe-response-terminator))))

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

(defun haxe-network-process-sentinel (process input)
  (message "haxe-network-process-sentinel <%s>" input)
  (when (stringp input)
      (cond
       ((or
         (haxe-string-starts-with input "failed with code")
         (haxe-string-starts-with input "connection broken by"))
        (setq haxe-network-status 'error))
       ((string= input "open")
        (setq haxe-network-status 'open
              haxe-restarting-server t
              haxe-reconnected 0))
       (t (setq haxe-network-status 'open
                haxe-restarting-server t
                haxe-reconnected 0)
        (haxe-append-server-response input)))))
(make-local-variable 'haxe-network-process-sentinel)

(defun haxe-network-tick ()
  (when (and (< haxe-reconnected haxe-times-to-reconnect)
             (eql haxe-network-status 'error))
    (incf haxe-reconnected)
    (setq haxe-restarting-server t)
    (haxe-start-waiting-server)))

(defun haxe-connection-for-buffer (buffer)
  (unless (bufferp buffer) (setq buffer (get-buffer buffer)))
  (loop for connection in haxe-connections
        for found = 
        (loop for buf in (oref connection buffers)
              do (when (eql (if (bufferp buffer) buffer
                              (get-buffer buffer)) buf)
                   (return connection)))
        do (when found (return found))))

(defun haxe-reconnect ()
  (interactive)
  (setq haxe-reconnected 0
        haxe-restarting-server t)
  (haxe-start-waiting-server))

;;;###autoload
(defun haxe-connect-to-compiler-server ()
  "Starts HaXe compilations server and connects to it.
This function is bound to \\[haxe-connect-to-compiler-server]"
  (interactive)
  (haxe-with-connection
   (con (connection
         filter sentinel host port
         connection-state reconnects))
   (unless (and connection (equal (process-status connection) 'open))
     (haxe-log 3 "Trying to connect to HaXe compiler on %s:%s" host port)
     (while (and
             (not (eql connection-state 'open))
             (< reconnects haxe-times-to-reconnect))
       (when connection (delete-process connection))
       (setq connection
             (make-network-process
              :name haxe-compiler-process
              :family 'ipv4
              :host host
              :nowait t
              :service port
              ;; :buffer haxe-network-process-buffer
              ;; :sentinel #'haxe-network-process-sentinel
              ;; :filter #'haxe-listen-filter
              :filter filter
              :sentinel sentinel))
       (incf reconnects)
       (message "I'm spinning %s" reconnects)
       (sleep-for 1))
     (haxe-log 3 (if (eql connection 'open)
                     "Connected to HaXe compiler"
                   "Connection to HaXe compiler failed permanently."))
     (setq haxe-reconnected 0
           haxe-network-idle-timer
           (run-with-idle-timer 2 t #'haxe-network-tick)))))

(defun haxe-send-to-server ()
  "Sends the substring `haxe-compiler-in-start' to
`haxe-compiler-in-end' from the current buffer to the compiler server"
  (interactive)
  (let ((message
         (concat
          (mapconcat #'identity
                     (haxe-split-string
                      (buffer-substring-no-properties
                       haxe-compiler-in-start
                       (setq haxe-compiler-in-end
                             (goto-char (point-max)))) ?\n t)
                     haxe-eol)
          (format "%s\000" haxe-eol))))
    (insert "\n")
    (goto-char (point-max))
    (haxe-log 0 "sending message <%s>" message)
    (process-send-string haxe-network-process message)
    (setq haxe-compiler-in-end (point-max)
          haxe-compiler-in-start haxe-compiler-in-end)))

(defun haxe-append-server-response (response)
  "Appends HaXe server response to the current buffer."
  (goto-char (point-max))
  (insert response)
  (goto-char (point-max))
  (setq haxe-compiler-in-start (point)
        haxe-compiler-in-end (point)))

;;;###autoload
(defun haxe-start-waiting-server (&optional restart compiler host port)
  "Starts HaXe `haxe-compiler' on `haxe-server-host':`haxe-server-port'
with \"--wait\" for the future requests made by autocompletion
or flymake.
RESTART is only used when called non-interactively, this instructs to
reuse the old buffer and its connection rather then create a new one.
This function is bound to \\[haxe-start-waiting-server]"
  (interactive
   (let ((compiler-i
          (read-string "HaXe compiler: "
                       haxe-compiler-default t
                       haxe-compiler-default))
         (host-i
          (read-string "HaXe server host: "
                       haxe-host-default t haxe-host-default))
         (port-i
          (read-number "HaXe server port: " haxe-port-default)))
     (list nil compiler-i host-i port-i)))
  (unless (called-interactively-p 'interactive)
    (let ((con (haxe-connection-for-buffer (current-buffer))))
      (unless compiler
        (setq compiler
              (or (oref compiler con) haxe-compiler-default)))
      (unless host
        (setq host (or (oref host con) haxe-host-default)))
      (unless port
        (setq host (or (oref port con) haxe-port-default)))))
  ;; TODO: must remember to kill process before restarting.
  (let* ((new-buffer
          (get-buffer-create
           (generate-new-buffer-name
            " *haxe-waiting-server*")))
         (proc (start-process compiler new-buffer compiler
                              "--wait" 
                              (format "%s:%d" host port))))
    (bury-buffer new-buffer)
    (if (not restart)
        (let ((buff
               (switch-to-buffer
                (get-buffer-create "*haxe-interactive-server*"))))
          (haxe-compiler-mode)
          (push (make-instance 'haxe-connection
                               :compiler compiler
                               :host host
                               :port port
                               :process proc
                               :buffers (list buff))
                haxe-connections))
      (with-slots (compiler host port process)
          (haxe-connection-for-buffer (current-buffer))
        (setq compiler compiler
              host host
              port port
              process proc)))
    (haxe-connect-to-compiler-server)))

(defun haxe-server-cleanup-hook ()
  (haxe-with-connection
   (con (buffers process connection))
   (message "Do you know who con is? %s" con)
   (setq buffers
         (remove-if (lambda (x) (eql x (current-buffer)))
                    buffers))
   (unless buffers
     (delete-process process)
     (when connection
       (delete-process connection))
     (setq haxe-connections
           (remove-if (lambda (x) (eql x con))
                      haxe-connections)))))

(define-derived-mode haxe-compiler-mode fundamental-mode
  "Haxe Compiler interaction mode"
  "Major mode interacting with HaXe compiler server.
This mode uses its own keymap:
\\{haxe-compiler-mode-map}"
  (kill-all-local-variables)
  (erase-buffer)
  (setq major-mode 'haxe-compiler-mode)
  (use-local-map haxe-compiler-mode-map)
  (setq mode-name "HaXe Interactive Compiler")
  (run-hooks 'haxe-compiler-mode-hook)
  (add-hook 'kill-buffer-hook #'haxe-server-cleanup-hook nil t))

(provide 'haxe-compiler-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; haxe-compiler-mode.el ends here.
