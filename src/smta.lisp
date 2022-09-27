;;;  smta.lisp --- main module

;;;  Copyright (C) 2004 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: Simple Mail Transport Agent
;;;  $Id$

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;;  Commentary:
;;;
;;; Tested only on SBCL, it may require some effort to port to a
;;; different CL, due to the amount of extensions used.
;;;
;;; The main entry point is RUN-PROGRAM, which will fork a spool
;;; monitor and start listening to SMTP connections.  For testing
;;; purposes the TEST macro will execute anything you like with "safe"
;;; defaults for some configuration variables.
;;;

#+cmu (ext:file-comment "$Id$")

(in-package #:smta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration parameters

(defvar *smta-load-path* (make-pathname :defaults *load-pathname* :name nil :type nil))

(defvar *configuration-file*  (merge-pathnames "smta.conf"  (uiop/os:getcwd))
  "Pathname of the configuration file.")

(defvar *alias-file* (merge-pathnames "aliases" (uiop/os:getcwd))
  "Aliases file pathname.  Aliases are read from this.")

(defvar *recipient-types* '()
  "List of recipient types and therefore delivery strategies
supported by this server.")

(defvar *debug-features* '(:accept :delivery)
  "List of debugging features symbols.  According to these
features certain debugging informations are produced by the
program.  Currently supported features:

  :CONFIG	verbose read of configuration file
  :ALIAS	alias table read and expansion
  :CONNECT	report connections
  :PROTOCOL	report SMTP protocol exchanges
  :DELIVERY	report deliveries when done
  :LOCKING	about locking and unlocking of messages
  :ACCEPT	report accepted messages")

(defvar *log-file* #P"/var/log/smta.log"
  "Pathname of the log file where debug messages are written.")

(defvar *max-processes* 20
  "Maximum number of processes this server is allowed to fork.
This limits the number of SMTP connections as well, because each
connection is served by a different process.")

(defvar *spooler-run-frequency* (* 5 60)
  "Time between runs of the spooler, in seconds.")

(defvar *initial-banner*  nil
  "Any initial blurb after the connection.")

;; not used yet -wcp2/9/04.
(defvar *max-smtp-line-length* 512
  "Maximum length of a single SMTP command line.  RFC821 says it
should be 512.")

(defvar *max-message-size* (* 10 1024 1024)
  "Maximum size of a message.  Beyond this we simply hang up.")

(defvar *mailboxes-directory* #P"/var/mail/"
  "Pathname of the directory containing the users' mailboxes.")

(defvar *spool-directory* #P"/var/spool/mqueue/"
  "Pathname of the directory used to store messages in transit.")

(defvar *smtp-port* '("smtp" . 25)
  "SMTP port name and/or number.")

(defvar *smtp-incoming-port* *smtp-port*
  "Port the server should listen to for SMTP connections.  If
it's a string it's a service name.  If it's a number must be a
port number.  If it's a pair the first element must be a string
for the service name and the second the backup port number if the
service name is not known.  This is normally set to '(\"smtp\" .
25).")

(defvar *allowed-hosts* t
  "List of hosts, domains, IP addresses, or IP masks, allowed to
contact this server.  If it's T, anybody is allowed.  Each item
can be a string for a hostname (case is insignificant), or a
string starting with a dot for a domain name, or a vector for a
specific IP address, or a pair of two vectors (an IP address and
a mask) to indicate a range of IP addresses.")

(defvar *barred-hosts* nil
  "List of hosts, domains, IP addresses, or IP masks, NOT allowed
to contact this server.  If it's NIL, nobody is barred.  Each
item can be a string for a hostname (case is insignificant), or a
string starting with a dot for a domain name, or a vector for a
specific IP address, or a pair of two vectors (an IP address and
a mask) to indicate a range of IP addresses.")

(defvar *relay-hosts*
  '(#(127 0 0 1)
    ;; this is just an example that works in my network
    (#(10 0 0 0) . #(255 255 255 0)))
  "List of hosts, domains, IP addresses, or IP masks, this server
is supposed to relay mail for (forward mail on behalf of some
other host).  If it's T, relay for anybody.  Each item can be a
string for a hostname (case is insignificant), or a string
starting with a dot for a domain name, or a vector for a specific
IP address, or a pair of two vectors (an IP address and a mask)
to indicate a range of IP addresses.")

(defvar *read-timeout* (* 5 60)
  "Timeout in seconds waiting for each command from the client.
RFC2821 recommends a minimum of 5 minutes.  After this waiting
time we simply hang up.")

(defvar *connect-hook* '()
  "List of procedures to execute upon connection from a new
client.  Each hook procedure is passed a socket as single
argument.  The procedures are allowed to signal a
REJECT-CONNECTION condition if the connection should be
rejected.")

(defvar *accept-before-hook* '()
  "List of procedures to execute upon receipt of a new message,
just before accepting it. Each hook procedure is passed a MESSAGE
object as single argument.  The procedures are allowed to signal
a REJECT-MESSAGE condition if the message should be rejected.")

(defvar *accept-after-hook* '()
  "List of procedures to execute upon receipt of a new message,
just after spooling it. Each hook procedure is passed a MESSAGE
object as single argument.")

(defvar *delivery-before-hook* '()
  "List of procedures to execute prior delivery of a message.
Each hook procedure is passed a MESSAGE object as single
argument.")

(defvar *delivery-after-hook* '()
  "List of procedures to execute after delivery of a message.
Each hook procedure is passed a MESSAGE object as single
argument.")

(defvar *smtp-routing-table*
  '((:local-domain :direct)
    ("localhost" :direct)
    (".playboy.com" :reject)
    (:any "gateway"))
  "Routing table for SMTP delivery.  This is an alist with the
first element a pattern for the destination address and the
second a delivery route.  The first element can be:

  :LOCAL-DOMAIN		matches anything in the domain of this server
  :ANY			matches any address (same as :ALL)
  \"host.domain\"	matches a certain host
  \".domain\"		matches a domain (not the leading dot)

The second element can be:

  :DIRECT		connect directly to the address
  :REJECT		don't deliver
  \"some.other.host\"	use a relay host")

(defvar *smtp-send-strategy*
  '(("gateway" :hold)
    (:any :immediate))
  "How SMTP deliveries should be performed.")

(defvar *max-smtp-delivery-attempts* 5
  "Maximum number of delivery atempts via SMTP.  This is when we
play as client.")

(defvar *services-file* #P"/etc/services"
  "Pathname of the /etc/services file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; https://www.sbcl.org/manual/#Idiosyncrasies ;;;;;
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))


;; デバッガフックを設定
;; (setf sb-ext:*invoke-debugger-hook*  
;;       (lambda (condition hook) 
;;         (declare (ignore conditoin hook))
;;         ;; デバッガが呼ばれたら、単にプログラムを終了する
;;         (format t "[smta:hook debugger calling...]~%")
;;         (sb-ext:quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +whitespace-chars+ '(#\space #\return #\newline #\tab))

(defvar *child-processes* '()
  "List of running child processes we have to keep count of.")

(defvar *aliases* (make-hash-table :test 'equalp)
  "Hash table containing all the alias definitions.")


(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(deftype ip-address ()
  '(vector integer 4))

(defun probe-program (program)
  ;; may check if it's executable -wcp31/8/04.
  (probe-file program))

(defun week-day->string (day)
  "Return the weekday string corresponding to DAY number."
  (elt #("Monday"
	 "Tuesday"
	 "Wednesday"
	 "Thursday"
	 "Friday"
	 "Saturday"
	 "Sunday") day))

(defun month->string (month)
  "Return the month string corresponding to MONTH number."
  (elt #("January"
	 "February"
	 "March"
	 "April"
	 "May"
	 "June"
	 "July"
	 "August"
	 "September"
	 "October"
	 "November"
	 "December") (1- month)))

(defun time-string (&optional (epoch (get-universal-time)))
  "Return a string describing the current time in human readable form."
  (multiple-value-bind (ss mm hh day month year week-day dst tz) (decode-universal-time epoch)
    (declare (ignore dst tz))
    (format nil "~A, ~A ~A ~A ~2D:~2,'0D:~2,'0D" (week-day->string week-day)
	    day (month->string month) year hh mm ss)))

(defun time-RFC822-string (&optional (epoch (get-universal-time)))
  "Return a string describing the current time according to
the RFC822."
  (multiple-value-bind (ss mm hh day month year week-day dst tz) (decode-universal-time epoch)
    (declare (ignore dst))
    (format nil "~A, ~A ~A ~2,'0D ~2,'0D:~2,'0D:~2,'0D ~:[-~;+~]~2,'0D~2,'0D"
	    (subseq (week-day->string week-day) 0 3)
	    day (subseq (month->string month) 0 3) (mod year 100) hh mm ss
	    (plusp tz) (abs (truncate tz)) (mod (* 60 tz) 60))))

(defun position-any (bag sequence &rest position-args)
  "Find any element of bag in sequence and return its position.
Accept any argument accepted by the POSITION function."
  (apply #'position-if #'(lambda (element)
			   (find element bag)) sequence position-args))

(defun find-any (bag sequence &rest find-args)
  "Find any element of bag in sequence.  Accept any argument
accepted by the FIND function."
  (apply #'find-if #'(lambda (element)
			   (find element bag)) sequence find-args))

(defun split-at (bag sequence)
  "Split SEQUENCE at occurence of any element from BAG.
Contiguous occurences of elements from BAG are condired atomic;
so no empty string is returned."
  (let ((len (length sequence)))
    (labels ((split-from (start)
	       (unless (>= start len)
		 (let ((sep (position-any bag sequence :start start)))
		   (cond ((not sep)
			  (list (subseq sequence start)))
			 ((> sep start)
			  (cons (subseq sequence start sep)
				(split-from (1+ sep))))
			 (:else
			  (split-from (1+ start))))))))
      (split-from 0))))

(defun find-service (service-name &optional (protocol "tcp"))
  "Return the port number associated to a particular service.
See /etc/services."
  (with-open-file (stream *services-file*)
    (loop
       for line = (read-line stream nil)
       while line
       do (let ((good-stuff (aif (position #\# line)
				 (subseq line 0 it)
				 line)))
	    (awhen (split-at +whitespace-chars+ good-stuff)
	      (destructuring-bind (name port &rest aliases) it
		(when (or (string= service-name name)
			  (member service-name aliases :test #'string=))
		  (destructuring-bind (port-num proto) (split-string-at-char port #\/)
		    (when (string-equal proto protocol)
		      (return (string->integer port-num)))))))))))

(defun port-number-back (port-desc)
  "Return the port number, either looking it up in the
/etc/services or simply returning the default value.  PORT-DESC
can be a number (just return that), a string (look it up) or a
pair (name and default number)."
  (etypecase port-desc
    (string (find-service port-desc))
    (integer port-desc)
    (list (or (find-service (car port-desc))
	      (cadr port-desc)))))

(defun port-number (port-desc)
  "Return the port number, either looking it up in the
/etc/services or simply returning the default value.  PORT-DESC
can be a number (just return that), a string (look it up) or a
pair (name and default number)."
    (car
      (cdr port-desc)))

(defun make-server-socket (port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (format t "[make-server-socket] ~a ~%" port)
    ;;(socket-bind socket (vector 0 0 0 0) (port-number port))
    (socket-bind socket #(0 0 0 0) port)
    (socket-listen socket 5)
    socket))

(defun make-client-socket (host port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-connect socket (if (stringp host)
			       (host-ent-address (get-host-by-name host))
			       host)
		    (port-number port))
    socket))

(defmacro with-open-socket ((socket value) &body body)
  `(let ((,socket ,value))
     (unwind-protect (progn ,@body)
       (socket-close ,socket))))

(defmacro do-connections ((connection port) &body body)
  (let ((server-socket (gensym)))
    `(with-open-socket (,server-socket (make-server-socket ,port))
       (loop
	  (with-open-socket (,connection (socket-accept ,server-socket))
	    ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recipient types

(defstruct recipient
  (address nil :type string :read-only t))

(defmacro define-recipient-type (name &rest slots)
  `(progn
     (defstruct (,name (:conc-name recipient-) (:include recipient))
       ,@slots)
     (push ',name *recipient-types*)))

(define-recipient-type mbox-recipient)
(define-recipient-type file-recipient)
(define-recipient-type pipe-recipient
    user)
(define-recipient-type smtp-recipient
    (attempts '() :type list))
(define-recipient-type uucp-recipient)

(defgeneric address-type-is (type address)
  (:documentation
   "Return a true if ADDRESS is syntactically compatible with type,
otherwise NIL."))

(defgeneric deliver-to-recipient (message recipient)
  (:documentation
   "Try to deliver message to recipient.  Return one of the
following values:

  :LATER	delivery can't be tried yet
  :RETRY	if failed but re-triable
  :FAILED	if failed and can't be tried again
  :DELIVERED	if succesfully delivered"))

(defmacro with-process ((variable (fmt &rest args) &rest rest) &body body)
  `(let ((,variable (sb-ext:run-program "/bin/sh" (list "-c" (format nil ,fmt ,@args)) ,@rest)))
     (unwind-protect
	  (progn ,@body)
       (sb-ext:process-close ,variable))))

(defun string-made-of (bag string)
  (loop
     for c across string
     unless (find c bag :test #'char-equal)
     return nil
     finally (return t)))

(defmethod address-type-is ((type (eql 'mbox-recipient)) address)
  (string-made-of "abcdefghijklmnopqrstuvwxyz-_.0123456789" address))

(defmethod address-type-is ((type (eql 'file-recipient)) address)
  (string-starts-with "/" address))

(defmethod address-type-is ((type (eql 'pipe-recipient)) address)
  (string-starts-with "|" address))

(defmethod address-type-is ((type (eql 'smtp-recipient)) address)
  (find #\@ address))

(defmethod address-type-is ((type (eql 'uucp-recipient)) address)
  (declare (ignore type address))
  ;; yet to be done -wcp2/9/04.
  nil)

(defun user-exists-p (user)
  ;; yet to be done -wcp1/9/04.
  (declare (ignore user))
  t)

(defun trim-space (string)
  (string-trim +whitespace-chars+ string))

(defun make-recipient-from-address (address)
  (dolist (type *recipient-types*)
    (awhen (address-type-is type address)
      (return (funcall (intern (concatenate 'string "MAKE-" (symbol-name type))
			       #.(package-name *package*)) :address address)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We don't keep the body in message object, because it may be huge.
;; Reading it everytime we need to deal with it, doesn't make sense.
(defclass message ()
  ((id :initarg :id
       :reader message-id
       :type string
       :initform (new-message-id)
       :documentation
       "unique identification of the message within the spool area")
   (client-identity :initarg :client-identity
		    :reader message-client-identity
		    :type string
		    :documentation
		    "declared by the client in the HELO command")
   (client-ip-address :initarg :client-ip-address
		      :reader message-client-ip-address
		      :type ip-address
		      :documentation
		      "from the client's connection socket")
   (sender :initarg :sender
	   :reader message-sender
	   :type string
	   :documentation
	   "from the MAIL command")
   (recipients :initarg :recipients
	       :accessor message-recipients
	       :type (list recipient)
	       :documentation
	       "from the RCPT commands")
   (time-received :initarg :time-received
		  :reader message-time-received
		  :type integer
		  :initform (get-universal-time))
   (headers :initarg :headers
	    :accessor message-headers
	    :type list
	    :documentation
	    "extracted from the message body")))

(defgeneric message-pathname (message &optional type)
  (:documentation
   "Return the pathname for a message in spool area."))

(defmethod message-pathname ((id string) &optional type)
  (make-pathname :defaults *spool-directory* :name id :type type))

(defmethod message-pathname ((id symbol) &optional type)
  (make-pathname :defaults *spool-directory* :name id :type type))

(defmethod message-pathname ((msg message) &optional type)
  (message-pathname (message-id msg) type))

;; this is really a nonsense -wcp31/8/04.
(defmethod message-pathname ((path pathname) &optional type)
  (make-pathname :defaults path :type type))

(defmacro write-slots (slots object stream)
  `(with-slots ,slots ,object
     ,@(mapcar #'(lambda (slot)
		   `(format ,stream ,(format nil "(~A ~~S)~%" slot) ,slot))
	       slots)))

(defmacro with-message-body ((stream message &rest open-options) &body body)
  `(with-open-file (,stream (message-pathname ,message "letter") ,@open-options)
    ,@body))

(defgeneric save-message (message))

(defmethod save-message ((msg message))
  (with-open-file (stream (message-pathname msg "envelope")
			  :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (write-slots (id client-identity client-ip-address sender recipients time-received headers)
		 msg stream)))

(defgeneric load-message (message)
  (:documentation
   "Load from file the MESSAGE."))

(defmethod load-message ((msg-path pathname))
  (let ((msg (make-instance 'message :id nil))
	;; make sure the symbols we read from file are interned in
	;; this package
	(*package* (find-package #.(package-name *package*))))
    (with-open-file (stream msg-path)
      (loop
	 for form = (read stream nil)
	 while form
	 do (setf (slot-value msg (car form)) (cadr form))))
    msg))

(defmethod load-message ((id string))
  (load-message (message-pathname id "envelope")))

(defgeneric lock-message (message)
  (:documentation
   "Try to lock message.  Return true if lock succeeded,
   otherwise false."))

(defmethod lock-message ((msg message))
  (lock-message (message-id msg)))

(defmethod lock-message ((id string))
  (lock-message (message-pathname id "lock")))

(defmethod lock-message ((path pathname))
  (let ((lock-pathname (message-pathname path "lock")))
    (handler-case
	(with-open-file (stream lock-pathname
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (princ (sb-posix:getpid) stream)
	  (dprint :locking "~A locked" (pathname-name path))
	  t)
      (file-error (condition)
	(declare (ignore condition))
	nil))))

(defun message-locked-p (message)
  (probe-file (message-pathname message "lock")))

(defgeneric unlock-message (message)
  (:documentation
   "Unlock message.  No check is made that the lock belonged to
us."))

(defmethod unlock-message ((msg message))
  (unlock-message (message-id msg)))

(defmethod unlock-message ((id string))
  (delete-file (message-pathname id "lock"))
  (dprint :locking "~A unlocked" id))

(defmacro with-locked-message (message &body body)
  "Execute BODY if we can get a lock on MESSAGE.  Remove lock at
the end."
  `(when (lock-message ,message)
     (unwind-protect (progn ,@body)
       (unlock-message ,message))))

(defun message-size (msg)
  (with-message-body (stream msg)
    (file-length stream)))

(defun bytes-string (bytes)
  (loop
     for unit in '("bytes" "KB" "MB" "GB")
     while (> bytes (* 2 1024))
     do (progn (setf bytes (round bytes 1024)))
     finally (return (format nil "~A ~A" bytes unit))))

(defun print-message (msg &optional (stream *standard-output*))
  (pprint-logical-block (stream '())
    (format stream "~&~A " (message-id msg))
    (pprint-indent :current 0 stream)
    (format stream "from ~A " (message-sender msg))
    (pprint-newline :fill stream)
    (format stream "to ")
    (pprint-logical-block (stream (message-recipients msg))
      (loop
	 (princ (recipient-address (pprint-pop)) stream)
	 (pprint-exit-if-list-exhausted)
	 (princ #\space stream)
	 (pprint-newline :fill stream)))
    (pprint-newline :mandatory stream)
    (princ (time-string (message-time-received msg)) stream)
    (pprint-newline :mandatory stream)
    (format stream "~A~@[ running~]" (bytes-string (message-size msg)) (message-locked-p msg)))
  (terpri stream))
  

(defgeneric delete-message (message)
  (:documentation
   "Delete message from spool."))

(defmethod delete-message ((msg message))
  (delete-message (message-id msg)))

(defmethod delete-message ((id string))
  (delete-file (message-pathname id "letter"))
  (delete-file (message-pathname id "envelope")))

(defmacro with-new-message ((message &rest initargs) &body body)
  "Take care to create a new MESSAGE object using INITARGS and
execute BODY so that at the end the message will be automatically
unlocked.  Please note that new messages are automatically
locked."
  (let ((condition (gensym)))
    `(let ((,message (make-instance 'message ,@initargs)))
       (unwind-protect
	    (handler-case (progn ,@body)
	      ;; if the body signal any type of conditions we must
	      ;; remove the half backed message
	      (condition (,condition)
		(delete-message ,message)
		(signal ,condition)))
	 (unlock-message ,message)))))

(defun message-sender-host (msg)
  (declare (type message msg))
  (let ((ip (message-client-ip-address msg)))
    (or (get-host-true-name ip)
	(format nil "~S" ip))))

(defun new-message-id ()
  "Return a new unique message id."
  ;; BEWARE: to make sure the id is kept unique we write a lock file
  ;; in the spool area
  (loop
     for id = (format nil "~36R" (random (expt 36 10)))
     until (lock-message id)
     finally (return id)))

(defun write-message (msg &optional (stream *standard-output*) crlf)
  (flet ((eol ()
	   (when crlf
	     (write-char #\return stream))
	   (write-char #\newline stream)))
    (format stream "From ~A ~A" (message-sender msg) (time-rfc822-string))
    (eol)
    (dolist (hdr (message-headers msg))
      (format stream "~A: ~A" (string-capitalize (car hdr)) (cdr hdr))
      (eol))
    (eol)
    (with-message-body (in msg)
      (loop
	 for line = (read-line in nil)
	 while line
	 do (progn
	      (write-string line stream)
	      (eol))))))

(defmethod deliver-to-recipient ((msg message) (recipient mbox-recipient))
  (handler-case
      (with-open-file (out (make-pathname :defaults *mailboxes-directory*
					  :name (recipient-address recipient))
			   :direction :output :if-exists :append
			   :if-does-not-exist :error)
	(write-message msg out)
	;; messages in mailboxes are separated by the From line, which is
	;; usually preceded by a blank line
	(terpri out)
	:delivered)
    (condition (c)
      (declare (ignore c))
      :failed)))

(defmethod deliver-to-recipient ((msg message) (recipient file-recipient))
  (handler-case
      (with-open-file (out (recipient-address recipient) :direction :output :if-exists :append
			   :if-does-not-exist :create)
	(write-message msg out)
	;; messages in mailboxes are separated by the From line, which is
	;; usually preceded by a blank line
	(terpri out)
	:delivered)
    (condition (c)
      (declare (ignore c))
      :failed)))

(defmethod deliver-to-recipient ((msg message) (recipient pipe-recipient))
  (handler-case
      (with-process (process ("~A" (subseq (recipient-address recipient) 1))
			     :pty nil :wait nil :search t :input :stream :output nil)
	(with-open-stream (stream (sb-ext:process-input process))
	  (write-message msg stream))
	:delivered)
    (condition (c)
      (declare (ignore c))
      :failed)))

(defmacro with-socket-connection ((stream server &optional port) &body body)
  (let ((socket (gensym)))
    `(with-open-socket (,socket (make-client-socket ,server ,(or port '(port *smtp-port*))))
       (with-open-stream (,stream (socket-make-stream ,socket :input t :output t
						      :buffering :line :element-type 'character))
	 ,@body))))

(defun crlf (stream)
  "Output a carriage return and a line feed on STREAM."
  (write-char #\return stream)
  (write-char #\newline stream))

(defun smtp-read-line (stream)
  "Read a line from SMTP peer (either server or client).  Strip
the optional #\return character at the end of the line.  Give up
after *READ-TIMEOUT* seconds."
  (sb-ext:with-timeout *read-timeout*
    (let* ((line (read-line stream))
	   (length (length line)))
      (if (and (plusp length)
	       (char= (elt line (1- length))
		      #\return))
	  (subseq line 0 (1- length))
	  line))))

(defun string->integer (string)
  "Convert a string into an integer.  If string doesn't represent
a proper integer, the result is undefined."
  (loop
     with value = 0
     for c across string
     do (setf value (+ (* value 10) (- (char-code c) (char-code #\0))))
     finally (return value)))

(defun smtp-receive-answer (stream)
  (let* ((code)
	 (description
	  (with-output-to-string (out)
	    (loop
	       for line = (smtp-read-line stream)
	       do (progn
		    (setf code (string->integer (subseq line 0 3)))
		    (write-line (subseq line 4) out))
	       while (char= #\- (elt line 3))))))
    (dprint :protocol "<< ~A ~A" code description)
    (values code description)))

(defun smtp-expect-answer (expected stream command)
  (multiple-value-bind (code description) (smtp-receive-answer stream)
      (unless (= code expected)
	(signal 'unexpected-smtp-answer
		:command command
		:expected expected
		:value code
		:description description))))

(defun send-smtp-command (stream expected-result-code fmt &rest args)
  (let ((line (apply #'format nil fmt args)))
    (write-string line stream)
    (crlf stream)
    (dprint :protocol ">> ~A" line)
    (smtp-expect-answer expected-result-code stream line)))

(defmacro with-smtp-connection ((stream server) &body body)
  `(with-socket-connection (,stream ,server)
     (unwind-protect
	  (progn
	    (smtp-expect-answer 220 stream "initial connection")
	    (send-smtp-command ,stream 250 "HELO ~A" (host-name))
		 ,@body)
       (send-smtp-command ,stream 221 "QUIT"))))

(defun get-host-from-address (address-string)
  (subseq address-string (1+ (position #\@ address-string :from-end t))))

(defmethod deliver-to-recipient ((msg message) (recipient smtp-recipient))
  (let ((server (get-host-from-address (recipient-address recipient))))
    (handler-case
	(with-smtp-connection (stream server)
	  (send-smtp-command stream 250 "MAIL from:<~A>" (message-sender msg))
	  (send-smtp-command stream 250 "RCPT to:<~A>" (recipient-address recipient))
	  (send-smtp-command stream 354 "DATA")
	  (write-message msg stream t)
	  (send-smtp-command stream 250 ".")
	  :delivered)
      (smtp-condition (condition)
	(push (cons (get-universal-time) (format nil "~A" condition))
	      (recipient-attempts recipient))
	(if (> (length (recipient-attempts recipient)) *max-smtp-delivery-attempts*)
	    :failed
	    :retry)))))

(defun attempt-immediate-delivery (message)
  (when (< (length *child-processes*) *max-processes*)
    (push (within-subprocess
	    (deliver-message message)) *child-processes*)))

(defun string-starts-with (prefix string &optional (compare #'string=))
  (let ((prefix-length (length prefix)))
    (and (>= (length string) prefix-length)
	 (funcall compare prefix string :end2 prefix-length))))

(defun string-ends-with (suffix string &optional (compare #'string=))
  (let ((suffix-length (length suffix))
	(string-length (length string)))
    (and (>= string-length suffix-length)
	 (funcall compare suffix string :start2 (- string-length suffix-length)))))

(defun char-whitespace-p (char)
  (member char +whitespace-chars+))

(defun extract-header (line)
  "Extract header/value pair from a line string.  If line is not
conforming to a header syntax, simply return NIL."
  (let ((colon-position (position #\: line)))
    (when (and colon-position
	       (not (find-if #'(lambda (c)
				 (not (find c "abcdefghijklmnopqrstuvwxyz-0123456789" :test #'char-equal)))
			     line :end colon-position)))
      (values (intern (string-upcase (subseq line 0 colon-position)) :keyword)
	      (trim-space (subseq line (1+ colon-position)))))))

(defun receive-message-body (msg in)
  (let ((bytes-read 0))
    (flet ((eom-p (line)
	     (string= line "."))
	   (continuation-p (line)
	     (and (plusp (length line))
		  (char-whitespace-p (first-char line))))
	   (from-p (line)
	     (string-starts-with "From " line #'string-equal))
	   (rl ()
	     (let ((line (smtp-read-line in)))
	       (incf bytes-read (length line))
	       (when (> bytes-read *max-message-size*)
		 (signal 'message-too-big))
	       line)))
      ;; the first line can be the "From " (without colon)
      (let ((current-line (rl))
	    (headers '()))
	(when (from-p current-line)
	  ;; Simply ditch it.  Apparently we don't have any use for
	  ;; the From line.
	  (setf current-line (rl)))
	;; read the headers
	(do ((line current-line (rl)))
	    ((eom-p line) line)
	  (if (and headers
		   (continuation-p line))
	      ;; if it's a continuation, append to the previous
	      ;; header
	      (setf (cdar headers) (concatenate 'string (cdar headers) line))
	      (multiple-value-bind (header value) (extract-header line)
		(if header
		    (push (cons header value) headers)
		    ;; if it's not a header we proceed with the body
		    (progn
		      (setf current-line line)
		      (setf (message-headers msg) (nreverse headers))
		      (return))))))
	;; read the body and write it to a file
	(with-message-body (out msg :direction :output :if-exists :error)
	  (do ((line current-line (rl)))
	      ((eom-p line))
	    (write-line line out
			;; skip initial dot if any
			:start (if (and (plusp (length line))
					(char= #\. (first-char line)))
				   1 0))))
	msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition smtp-condition (error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "SMTP condition.")))
  (:documentation
    "Base class for all exceptions raised by this module."))

(define-condition smtp-eof (smtp-condition)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "SMTP EOF.~%The controlled process may have died."))))

(define-condition smtp-timeout (smtp-condition)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "SMTP timeout."))))

(define-condition message-too-big (smtp-condition)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "message too big; max=~A." (bytes-string *max-message-size*)))))

(define-condition reject-condition ()
  ((reason :initarg :reason
	   :reader reject-reason
	   :type string))
  (:report (lambda (condition stream)
	     (format stream "Rejected because ~A." (reject-reason condition)))))

(define-condition reject-connection (reject-condition)
  ())

(define-condition reject-message (reject-condition)
  ())

(define-condition unexpected-smtp-answer (smtp-condition)
  ((command :initarg :command
	    :reader smtp-command)
   (value :initarg :value
	  :reader smtp-value)
   (expected :initarg :expected
	     :reader smtp-expected)
   (description :initarg :description
		:reader smtp-description))
  (:report (lambda (condition stream)
	     (format stream "Unexpected SMTP answer ~A for command ~S while waiting for ~A (reason ~A)."
		     (smtp-value condition) (smtp-command condition) (smtp-expected condition) (smtp-description condition)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *commands-help*
  '((:about "~
	This is Smta $Revision$
	Topics:
		HELO	EHLO	MAIL	RCPT	DATA
		RSET	NOOP	QUIT	HELP	VRFY
		EXPN	VERB	ETRN	DSN	AUTH
		STARTTLS
	For more info use \"HELP <topic>\".
	To report bugs in the implementation send email to
		smta-bugs@nowhere.org.
	For local information send email to Postmaster at your site.")
    (:help "~
	HELP [ <topic> ]
		The HELP command gives help info.")
    (:helo "~
	HELO <hostname>
		Introduce yourself.")
    (:ehlo "~
	EHLO <hostname>
		Introduce yourself, and request extended SMTP mode.
	Possible replies include:
		EXPN		Expand the mailing list		[RFC821]
		HELP		Supply helpful information	[RFC821]
		8BITMIME	Use 8-bit data			[RFC1652]
		SIZE		Message size declaration	[RFC1870]
		VERB		Verbose				[Allman]
		CHUNKING	Chunking			[RFC1830]
		BINARYMIME	Binary MIME			[RFC1830]
		PIPELINING	Command Pipelining		[RFC1854]
		DSN		Delivery Status Notification	[RFC1891]
		ETRN		Remote Message Queue Starting	[RFC1985]
		STARTTLS	Secure SMTP			[RFC2487]
		AUTH		Authentication			[RFC2554]
		ENHANCEDSTATUSCODES	Enhanced status codes	[RFC2034]
		DELIVERBY	Deliver By			[RFC2852]")
    (:mail "~
	MAIL FROM: <sender> [ <parameters> ]
		Specifies the sender.  Parameters are ESMTP extensions.
		See \"HELP DSN\" for details.")
    (:rcpt "~
	RCPT TO: <recipient> [ <parameters> ]
		Specifies the recipient.  Can be used any number of times.
		Parameters are ESMTP extensions.  See \"HELP DSN\" for details.")
    (:data "~
	DATA
		Following text is collected as the message.
		End with a single dot.")
    (:rset "~
	RSET
		Resets the system.")
    (:quit "~
	QUIT
		Exit Smta (SMTP).")
    (:auth "~
	AUTH mechanism [initial-response]
		Start authentication.")
    (:starttls "~
	STARTTLS
		Start TLS negotiation.")
    (:verb "~
	VERB
		Go into verbose mode.")
    (:vrfy "~
	VRFY <recipient>
		Verify an address.  If you want to see what it aliases
		to, use EXPN instead.")
    (:expn "~
	EXPN <recipient>
		Expand an address.  If the address indicates a mailing
		list, return the contents of that list.")
    (:noop "~
	NOOP
		Do nothing.")
    (:status"~
	STATUS
		Report the spool queue status.")
    (:etrn "~
	ETRN [ <hostname> | @<domain> | #<queue-name> ]
		Run the queue for the specified <hostname>, or
		all hosts within a given <domain>, or a specially-named
		<queue-name> (implementation-specific).")
    (:dsn "~
	MAIL FROM: <sender> [ RET={ FULL | HDRS} ] [ ENVID=<envid> ]
	RCPT TO: <recipient> [ NOTIFY={NEVER,SUCCESS,FAILURE,DELAY} ]
			     [ ORCPT=<recipient> ]
		SMTP Delivery Status Notifications.
	Descriptions:
		RET	Return either the full message or only headers.
		ENVID	Sender's \"envelope identifier\" for tracking.
		NOTIFY	When to send a DSN. Multiple options are OK, comma-
			delimited. NEVER must appear by itself.
		ORCPT	Original recipient."))
  "AList of help string for each SMTP command.")

(defvar *supported-smtp-commands* '(:helo :mail :rcpt :data :quit :help :rset :noop :status :etrn)
  "List of supported SMTP commands.  You can selectively remove
those you don't like or think they represent a security risk.
It's a bad idea to remove :HELO, :MAIL, or such.")

;; currently not used -wcp2/9/04.
(defvar *extended-capabilities* '()
  ;; '(:enhancedstatuscodes :pipelining :8bitmime :size :dsn :etrn :deliverby :help)
  "List of supported ESMTP capabilities.")

(defun first-char (string)
  (elt string 0))

(defun last-char (string)
  (elt string (1- (length string))))

(defun run-hooks (hooks &rest args)
  (dolist (proc hooks)
    (apply proc args)))

(defun dprint (debug-feature fmt &rest args)
  "Output formatted message to *LOG-FILE* if DEBUG-FEATURE is
among the selected ones in *DEBUG-FEATURES*.  The log file is
always opened and closed at each message."
  ;; we should make sure that messages are not garbled due to the race
  ;; condition between processes -wcp28/8/04.
  (when (member debug-feature *debug-features*)
    (with-open-file (out *log-file* :direction :output
			 :if-exists :append :if-does-not-exist :create)
      (multiple-value-bind (ss mm hh day month year week-day dst tz) (get-decoded-time)
	(declare (ignore year week-day dst tz))
	(format out "~A ~2D ~2D:~2,'0D:~2,'0D [~A] " (subseq (month->string month) 0 3)
		day hh mm ss (sb-posix:getpid)))
      (apply #'format out fmt args)
      (terpri out))))

(defun print-help (stream what)
  (let ((doc-string (if what
			(assoc what *commands-help* :test #'string-equal)
			(assoc :about *commands-help*))))
    (if doc-string
	(smtp-reply stream 214 "~A" (cadr doc-string))
	(smtp-reply stream 504 "HELP topic ~S unknown" what))))

(defun run-in-subprocess (proc)
  "Execute PROC in a separate process.  This will fork a new
kernel process."
  (let ((pid (sb-posix:fork)))
    (cond ((zerop pid)
	   ;; child process
	   (unwind-protect
		(funcall proc)
	     ;; whatever happens, just make sure the process dies
	     (sb-ext:quit)))
	  ((> pid 0)
	   pid)
	  (:else (error "can't fork any more.")))))

(defmacro until (condition &body body)
  "Execute BODY until CONDITION is true."
  `(do () (,condition) ,@body))

(defmacro within-subprocess (&body body)
  "Execute BODY within a forked subprocess.  See
RUN-IN-SUBPROCESS."
  `(run-in-subprocess #'(lambda () ,@body)))

(defun get-host-true-name (host)
  (if (stringp host)
      (awhen (get-host-by-name host)
	(host-ent-name it))
      (awhen (get-host-by-address host)
	(host-ent-name it))))

(defun get-host-ip-address (host)
  (if (stringp host)
      (awhen (get-host-by-name host)
	(host-ent-address it))
      host))

(defun remove-child-from-pool (pid)
  (setf *child-processes* (remove pid *child-processes*)))

(defun match-host (host patterns)
  (labels ((match-name (name pattern)
	     (or (string-equal name pattern)
		 (and (char= (first-char pattern) #\.)
		      (> (length name) (length pattern))
		      (string-equal (subseq name (- (length name) (length pattern))) pattern))))
	   (match-ip-address (address pattern)
	     (if (vectorp pattern)
		 (equalp address pattern)
		 (equalp (map 'vector #'logand address (cdr pattern))
			 (map 'vector #'logand (car pattern) (cdr pattern))))))
    (let ((host-name (get-host-true-name host))
	  (host-ip-address (get-host-ip-address host)))
      (loop
	 for pattern in patterns
	 when (if (stringp pattern)
		  (match-name host-name pattern)
		  (match-ip-address host-ip-address pattern))
	 return t
	 finally (return nil)))))

(defun read-smtp-command (stream)
  (let* ((line (smtp-read-line stream))
	 (cmd-end (position #\space line))
	 (cmd-string (if cmd-end
			 (subseq line 0 cmd-end)
			 line))
	 (cmd-data (when cmd-end
		     (subseq line (1+ cmd-end)))))
    (dprint :protocol "<< ~A" line)
    (if (member cmd-string *supported-smtp-commands* :test #'string-equal)
	(values (intern (string-upcase cmd-string) :keyword) cmd-data)
	;; this will trigger an error
	(values nil line))))

(defun host-name ()
  "Return the name of this host inclusive of domain name."
  (sb-unix:unix-gethostname))

(defun version ()
  "Return the current program version."
  "$Revision$")

(defun split-string-at-char (string separator &key escape skip-empty)
  "Split STRING at SEPARATORs and return a list of the substrings.  If
SKIP-EMPTY is true then filter out the empty substrings.  If ESCAPE is
not nil then split at SEPARATOR only if it's not preceded by ESCAPE."
  (declare (type string string) (type character separator))
  (labels ((next-separator (beg)
             (let ((pos (position separator string :start beg)))
               (if (and escape
                        pos
                        (plusp pos)
                        (char= escape (char string (1- pos))))
                   (next-separator (1+ pos))
                   pos)))
           (parse (beg)
             (cond ((< beg (length string))
                    (let* ((end (next-separator beg))
                           (substring (subseq string beg end)))
                      (cond ((and skip-empty (string= "" substring))
                             (parse (1+ end)))
                            ((not end)
                             (list substring))
                            (:else
			     (cons substring (parse (1+ end)))))))
                   (skip-empty
		    '())
                   (:else
		    (list "")))))
    (parse 0)))

(defun split-lines (string)
  "Split lines at newline characters."
  (split-string-at-char string #\newline))

(defun smtp-reply (stream code fmt &rest args)
  "Send a reply of code CODE to STREAM.  FMT and ARGS are passed
to FORMAT.  Multi-line replies are formatted according to RFC821."
  (let ((answer (apply #'format nil fmt args)))
    (loop
       for lines on (split-lines answer)
       do (progn
	    (format stream "~A~C~A" code (if (cdr lines) #\- #\space) (car lines))
	    (dprint :protocol ">> ~A~C~A" code (if (cdr lines) #\- #\space) (car lines))
	    (crlf stream)))))

(defun greeting-message (stream)
  (smtp-reply stream 220 "~A SMTP smta ~A; ~A ~%"
	      (host-name) (version) (time-string)))

(defun greeting-message-bak (stream)
  (smtp-reply stream 220 "~A SMTP smta ~A; ~A~@
Hi there.  This is an experimental SMTP server written in Common Lisp.~@
If something goes wrong with this session you are pretty much~@
on your own.  Don't say I didn't tell you."
	      (host-name) (version) (time-string)))

(defun answer-helo (stream client-ip-address extended)
  (smtp-reply stream 250 "~A Hello ~A ~A, nice to meet you~:[~;~{~&~A~}~]~@[~%~A~]"
	      (host-name) (get-host-true-name client-ip-address) client-ip-address
	      extended *extended-capabilities* *initial-banner*))

(defun local-address-p (address)
  "Return true if the e-mail ADDRESS is local to this machine."
  (string-made-of "abcdefghijklmnopqrstuvwxyz0123456789" address))

(defun normalise-message-headers (message)
  (let ((headers (message-headers message)))
    (macrolet ((default (hdr value)
		 `(unless (assoc ,hdr headers)
		    (push (cons ,hdr ,value) headers))))
      (default :date (time-rfc822-string))
      (default :to "undisclosed-recipients:;")
      (default :from (message-sender message))
      (default :message-id (format nil "<~A@~A>" (message-id message) (host-name)))
      ;; every SMTP server should add a received line
      (push (cons :received (format nil "from ~A (~A ~A)
	by ~A (smta $Revision$)
	with SMTP id ~A;
	~A
	(envelope-from ~A)" (message-client-identity message)
	(message-sender-host message)
	(message-client-ip-address message)
	(host-name)
	(message-id message)
	(time-rfc822-string)
	(message-sender message))) headers)
      (setf (message-headers message) headers))))

(defun bounce-message (addresses message)
  ;; yet to be done -wcp30/8/04.
  (declare (ignore addresses message))
  nil)

(defgeneric deliver-message (message)
  (:documentation
   "Try to deliver message to its recipients."))

(defmethod deliver-message ((id string))
  (let ((msg (load-message id)))
    (with-locked-message msg
      (deliver-message msg))))

;; Should implement some clever strategy like sorting recipients by
;; host and connect once for each host or mailbox -wcp26/8/04.

(defmethod deliver-message ((msg message))
  (loop
     with failed
     for recipient in (message-recipients msg)
     do (ecase (deliver-to-recipient msg recipient)
	  (:delivered
	   (dprint :delivery "~A delivered to ~A" (message-id msg) (recipient-address recipient)))
	  (:retry
	   (push recipient failed))
	  ;; this looks like a repeat; maybe it should go -wcp1/9/04.
	  (:later
	   (push recipient failed))
	  (:failed
	   (bounce-message msg recipient)))
     finally
       (setf (message-recipients msg) failed)))

(defun find-user-id (user-name)
  (flet ((empty-p (line)
	   (or (< (length line) 1)
	       (char= #\# (elt line 0)))))
    (with-open-file (stream #P"/etc/passwd")
      (loop
	 for line = (read-line stream nil)
	 while line
	 unless (empty-p line)
	 do (destructuring-bind (name passwd uid &rest rest) (split-string-at-char line #\:)
	      (declare (ignore passwd rest))
	      (when (string= user-name name)
		(return (string->integer uid))))
	 finally (return nil)))))

(defun user-home-directory (user)
  (pathname (sb-unix:uid-homedir (if (stringp user)
				     (find-user-id user)
				     user))))

(defstruct (file-stat (:conc-name fs-))
  device
  inode
  mode
  links
  uid
  gid
  rdev
  size
  atime
  mtime
  ctime)

(defun stat-file (pathname)
  (multiple-value-bind (exist-p device inode mode links uid gid rdev size atime mtime ctime)
      (sb-unix:unix-stat (namestring pathname))
    (if exist-p
	(make-file-stat :device device
			:inode inode
			:mode mode
			:links links
			:uid uid
			:gid gid
			:rdev rdev
			:size size
			:atime atime
			:mtime mtime
			:ctime ctime))))

(defun dot-forward-content (user)
  (let* ((id (find-user-id user))
	 (home-dir (user-home-directory id))
	 (path-name (make-pathname :defaults home-dir :name ".forward"))
	 (stat (stat-file path-name)))
    (when (and (probe-file path-name)
	       ;; should be owned by the user
	       (eq (fs-uid stat) id)
	       ;; and not writable by anybody else
	       (zerop (logand #o022 (fs-mode stat))))
      (flet ((clean (line)
	       ;; the .forward lines may be sorrounded by double
	       ;; quotes ("like this"); remove them
	       (let* ((str (trim-space line))
		      (len (length str)))
		 (if (and (plusp len)
			  (char= #\" (elt str 0)))
		     (subseq str 1 (if (and (> len 1)
					    (char= #\" (elt str (1- len))))
				       (1- len)
				       len))
		     str))))
	(with-open-file (stream path-name)
	  (loop
	     for line = (read-line stream nil)
	     while line
	     collect (clean line)))))))

(defun expand-alias (address)
  ;; the seen argument is to keep track of loops
  (labels ((expand (address seen)
	     (cond ((member address seen :test #'string=)
		    ;; it's a loop!
		    nil)
		   (:else
		    (let ((expansion (gethash address *aliases*)))
		      (cond (expansion
			     (mapcan #'(lambda (name)
					 (expand name (cons address seen)))
				     expansion))
			    ((local-address-p address)
			     (aif (dot-forward-content address)
				  (mapcan #'(lambda (name)
					      (expand name (cons address seen)))
					  it)
				  (list address)))
			    (:else
			     (list address))))))))
    (expand address '())))

(defgeneric recipient= (recipient1 recipient2)
  (:documentation
   "Return true if recipients are equal."))

;; fall back method
(defmethod recipient= ((rcpt1 recipient) (rcpt2 recipient))
  (string= (recipient-address rcpt1) (recipient-address rcpt2)))

(defmethod recipient= ((rcpt1 smtp-recipient) (rcpt2 smtp-recipient))
  (let* ((addr1 (recipient-address rcpt1))
	 (addr2 (recipient-address rcpt2))
	 (at-position (position #\@ addr1)))
    (and (= (length addr1) (length addr2))
	 (char= #\@ (elt addr2 at-position))
	 ;; domain names are case insensitive, whereas user names
	 ;; normally aren't, so we compare case sensitive on the
	 ;; former but not on the latter
	 (string= addr1 addr2 :end1 at-position :end2 at-position)
	 (string-equal addr1 addr2 :start1 at-position :start2 at-position))))

(defun uniq (list &optional (compare #'eql))
  (loop
     for item in list
     unless (member item unique-items :test compare)
     collect item into unique-items
     finally (return unique-items)))

(defun expand-aliases (addresses)
  (loop
     for addr in addresses
     append (expand-alias addr) into new-addresses
     finally (return (uniq new-addresses #'string=))))

(defun spool-message (message)
  "Save message in spool, for later processing."
  (normalise-message-headers message)
  (save-message message))

(defun strip-angle-brackets (string)
  "Remove, if present, the angle brackets around an e-mail
address."
  (let* ((length (length string))
	 (start (if (and (plusp length)
			 (char= #\< (elt string 0)))
		    1 0))
	 (end (if (and (plusp length)
		       (char= #\> (elt string (1- length))))
		  (1- length)
		  length)))
    (subseq string start end)))

(defun canonify-sender (sender)
  (strip-angle-brackets (trim-space sender)))

(defun canonify-recipient (recipient)
  (strip-angle-brackets (trim-space recipient)))

(defun valid-sender-p (sender)
  (declare (ignore sender))
  ;; yet to be done -wcp31/8/04.
  t)

(defun check-recipient-syntax (address)
  ;; There isn't very much to do here.  Very few character are
  ;; forbidden and to be open to new delivery strategies we could
  ;; simply allow everything in.
  (declare (ignore address))
  t)

(defun smtp-repl (stream client-ip-address)
  ;;(greeting-message stream)
  (let (extended-smtp
	client-identity
	sender
	recipients)
    (flet ((reset-state ()
	     (setf sender nil
		   recipients '()
		   extended-smtp nil)))
      (reset-state)
      (loop
	 (multiple-value-bind (command data) (read-smtp-command stream)
	   (case command
	     (:quit
	      (smtp-reply stream 221 "~A closing connection" (host-name))
	      (return))
	     (:help
	      (print-help stream data))
	     (:helo
	      ;; never understood the purpose of this HELO command;
	      ;; maybe we should verify the client -wcp1/9/04.
	      (setf extended-smtp nil)
	      (setf client-identity (trim-space data))
	      (answer-helo stream client-ip-address nil))
	     (:ehlo
	      (setf extended-smtp t)
	      (setf client-identity (trim-space data))
	      (answer-helo stream client-ip-address nil))
	     (:mail
	      (if sender
		  (smtp-reply stream 503 "Sender already specified")
		  (if (string-starts-with "from:" data #'string-equal)
		      (destructuring-bind (from &optional esmtp-parameters)
			  (split-string-at-char (subseq data 5) #\space :skip-empty t)
			(declare (ignore esmtp-parameters))
			(let ((sender-address (canonify-sender from)))
			  (if (valid-sender-p sender-address)
			      (progn
				(setf sender sender-address)
				(smtp-reply stream 250 "~A... Sender OK" sender-address))
			      (progn
				(smtp-reply stream 450 "invalid sender ~S" sender-address)))))
		      (smtp-reply stream 501 "Syntax error in parameters scanning \"~A\"" data))))
	     (:rcpt
	      (if sender
		  (if (string-starts-with "to:" data #'string-equal)
		      (destructuring-bind (to &optional esmtp-parameters)
			  (split-string-at-char (subseq data 3) #\space :skip-empty t)
			(declare (ignore esmtp-parameters))
			(let ((address (canonify-recipient to)))
			  (if (check-recipient-syntax address)
			      (progn
				(push address recipients)
				(smtp-reply stream 250 "~A... Recipient OK" address))
			      (smtp-reply 501 "invlid recipient syntax ~S" address))))
		      (smtp-reply stream 501 "Syntax error in parameters scanning \"~A\"" data))
		  (smtp-reply stream 503 "Need MAIL before RCPT")))
	     (:data
	      (cond ((not sender)
		     (smtp-reply stream 503 "Need MAIL command"))
		    ((null recipients)
		     (smtp-reply stream 503 "Need RCPT (recipient)"))
		    (:else
                     (progn
       (format t "aaaaa~%")
		     (with-new-message (msg :client-identity client-identity
					    :client-ip-address client-ip-address
					    :sender sender
					    :recipients (mapcar #'make-recipient-from-address
								(expand-aliases recipients)))
		       (smtp-reply stream 354 "Enter mail, end with \".\" on a line by itself")
		       (receive-message-body msg stream)
		       (handler-case (run-hooks *accept-before-hook* msg)
			 (reject-message (condition)
			   (smtp-reply stream 450 "~A Message rejected (~A)"
				       (message-id msg) (reject-reason condition))
			   (dprint :accept "message ~A rejected because ~A"
				   (message-id msg) (reject-reason condition))
			   (delete-message msg))
			 (:no-error (x)
			   (declare (ignore x))
			   (spool-message msg)
			   (smtp-reply stream 250 "~A Message accepted for delivery" (message-id msg))
			   (dprint :accept "accepted message ~A" (message-id msg))
			   (run-hooks *accept-after-hook* msg)
			   (attempt-immediate-delivery msg)))
		       (reset-state))))))
	     (:rset
	      (reset-state)
	      (smtp-reply stream 250 "Reset state"))
	     (:noop
	      (smtp-reply stream 250 "OK"))
	     (:status
	      (smtp-reply stream 250 (with-output-to-string (stream)
				       (print-spool-status stream))))
	     (:etrn
	      (run-spool data))
	     (otherwise
	      (smtp-reply stream 500 "Command unrecognised: \"~A\"" data))))))))

(defmacro test (&body body)
  ;; rebind some special variables to harmless values
  `(let* ((test-dir (append (pathname-directory *smta-load-path*) '("test")))
	  (*mailboxes-directory*
	   (make-pathname :defaults *smta-load-path*
			  :directory (append test-dir '("mbox"))))
	  (*spool-directory*
	   (make-pathname :defaults *smta-load-path*
			  :directory (append test-dir '("spool"))))
	  (*log-file*
	   (make-pathname :defaults *smta-load-path*
			  :directory test-dir
			  :name "log"))
	  (*debug-features* '(:config :alias :connect :protocol :delivery :accept :locking))
	  (*spooler-run-frequency* 10)
	  (*smtp-incoming-port* 25000))
     (ensure-directories-exist *mailboxes-directory*)
     (ensure-directories-exist *spool-directory*)
     ,@body))

(defun authorised-client-p (socket)
  (or (eq *allowed-hosts* t)
      (match-host (socket-peername socket) *allowed-hosts*)))

(defun smtp-client-barred-repl (stream client)
  (smtp-reply stream 554 "Sorry mate, ~A is not authorised to talk to this server" client)
  (loop
     (multiple-value-bind (command data) (read-smtp-command stream)
       (declare (ignore data))
       (case command
	 (:quit
	  (smtp-reply stream 221 "~A closing connection" (host-name))
	  (return))
	 (otherwise
	  (smtp-reply stream 503 "bad sequence of commands"))))))

(defun serve-connection (connection)
  (run-hooks *connect-hook* connection)
  (with-open-stream (stream (socket-make-stream connection :input t :output t
						:buffering :line :element-type 'character))
    (if (authorised-client-p connection)
	(smtp-repl stream (socket-peername connection))
	(smtp-client-barred-repl stream (socket-peername connection)))))

(defun bury-dead-child ()
  (remove-child-from-pool (sb-posix:wait)))

(defun sigchld-handler (&rest args)
  (declare (ignore args))
  (bury-dead-child))

(defun parse-alias-definition (string)
  (let ((colon-position (position #\: string)))
    (if colon-position
	(values (trim-space (subseq string 0 colon-position))
		(mapcar #'trim-space
			(split-string-at-char (subseq string (1+ colon-position)) #\,))))))

(defun read-aliases-file ()
  (with-open-file (stream *alias-file*)
    (loop
       with current-alias = "\\"
       for line = (read-line stream nil)
       while line
       do (let ((trimmed-line (string-left-trim +whitespace-chars+ line)))
	    (cond ((zerop (length trimmed-line))
		   ;; skip empty the lines
		   nil)
		  ((char= #\# (first-char trimmed-line))
		   ;; skip comment lines
		   nil)
		  ((char= #\\ (last-char current-alias))
		   (setf current-alias
			 (concatenate 'string (subseq current-alias 0 (1- (length current-alias))) line)))
		  ((char-whitespace-p (last-char line))
		   (setf current-alias
			 (concatenate 'string current-alias line)))
		  (:else
		   (multiple-value-bind (name expansion) (parse-alias-definition current-alias)
		     (when name
		       (dprint :alias "alias ~S = ~S" name expansion)
		       (setf (gethash name *aliases*) expansion)))
		   (setf current-alias line)))))))

(defun read-configuration-file ()
  "Read and execute the configuration file *CONFIGURATION-FILE*.
The configuration file may contain any valid Lisp statement but
it's mainly used to set the configuration variables."
  (dprint :config "Start reading configuration file ~A" *configuration-file*)
  (when (probe-file *configuration-file*)
    ;; in the configuration file we must have visibility of this
    ;; package
    (let ((*package* (find-package #.(package-name *package*)))
	  (loud (member :config *debug-features*)))
      (load *configuration-file*
	    :verbose loud
	    :print loud)))
  (dprint :config "End of configuration file ~A" *configuration-file*))

(defmacro add-hook (hook procedure &optional append)
  (if append
      `(unless (member #',procedure ,hook)
	 (setf ,hook (append ,hook (list #',procedure))))
      `(pushnew #',procedure ,hook)))

(defun list-spooled-messages ()
  (directory (message-pathname :wild "envelope")))

(defmacro do-spool-messages (msg &body body)
  (let ((path (gensym)))
    `(dolist (,path (list-spooled-messages))
       (let ((,msg (load-message ,path)))
	 ,@body))))

(defun print-spool-status (&optional (stream *standard-output*))
  (format stream "SMTA $Revision$ running on ~A:~%" (host-name))
  (let ((n 0))
    (do-spool-messages msg
      (incf n)
      (print-message msg stream))
    (format stream "total ~A message~:P~%" n)))

(defun run-spool ()
  (do-spool-messages msg
    (with-locked-message msg
      (deliver-message msg)
      (if (message-recipients msg)
	  (save-message msg)
	  (progn
	    (dprint :delivery "message ~A completely delivered" (message-id msg))
	    (delete-message msg))))))

(defvar *spooler-process* nil
  "Process id of the spooler.")

(defun start-spooler ()
  "Start the spooler daemon.  It returns it's process id."
  (setf *spooler-process*
	(within-subprocess
	  (loop
	     (run-spool)
	     (sleep *spooler-run-frequency*)))))

(defun start-smtp-server ()
  "Run the SMTP server.  Id doesn't return."
  (do-connections (socket (cdr *smtp-incoming-port*))
    (dprint :connect "Received connection from ~A" (socket-peername socket))
      (push (within-subprocess
        (serve-connection socket)) *child-processes*)
        (when (>= (length *child-processes*) *max-processes*)
          (sb-posix:wait))))

(defun run-program ()
  "Main program entry point.  It start all the server modules."
  (read-configuration-file)
  (read-aliases-file)
  (start-spooler)
  (start-smtp-server))


(defun start-echo-server (port)
  "Listening on a port for a message, and print the received message."
  (usocket:with-socket-listener (socket "127.0.0.1" port)
    (usocket:wait-for-input socket)
    (usocket:with-connected-socket (connection (usocket:socket-accept socket))
      (loop for line = (read-line (usocket:socket-stream connection))
        do 
        (progn
          (format (usocket:socket-stream connection) "~a~%" line)
          (force-output (usocket:socket-stream connection)))
        (if (equal (string-right-trim '(#\Return #\Newline) line) "quit") (return NIL))))))
