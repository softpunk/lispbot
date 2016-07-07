(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
(with-open-file
	(*standard-output* "/dev/null" :direction :output :if-exists :supersede)
		(ql:quickload '(alexandria cl-ppcre iterate anaphora cl-irc cl-json trivial-shell)))

(defpackage lispbot
 (:use cl alexandria iterate cl-ppcre anaphora cl-irc cl-json trivial-shell))
(in-package lispbot)

(setf *read-eval* nil)
(make-random-state)

(defvar *cute* '(
   "(✿◠‿◠)っ~~ ♥ ~a"
   "⊂◉‿◉つ ❤ ~a"
   "( ´・‿-) ~~ ♥ ~a"
   "(っ⌒‿⌒)っ~~ ♥ ~a"
   "ʕ´•ᴥ•`ʔσ” BEARHUG ~a"
   "~a (´ε｀ )♡"
   "(⊃｡•́‿•̀｡)⊃ U GONNA GET HUGGED ~a"
   "( ＾◡＾)っ~~ ❤ ~a"))

(defvar *connection* nil)
(defvar *nickname* "lispbot")
(defvar *members* '("Arathnim"))
(defvar *commands* '(cute join-channel send))
(defvar *ignored* '("nv" "trinary" "tca" "shinrei"))
(defvar src nil)
(defvar dest nil)

(defmacro is-auth (exp)
   `(when (auth src) ,exp))

(defun eval-command (form)
	(if (and (listp form) (member (car form) *commands*)) 
		 (apply (car form) (cdr form))
		 (is-auth (ignore-errors (eval form)))))

(defun cute (nick)
	(format nil (nth (random (length *cute*)) *cute*) (format nil "~a" nick)))

(defun join-channel (channel)
	(progn (join *connection* (string-downcase (string channel)))
		    nil))

(defun read-command (str)
	(ignore-errors (read-from-string (clean str))))

(defun auth (dst)
   (member dst *members* :test #'equalp))

(defun catstr (&rest rest)
   (format nil "~{~a~}" rest))

(defun wrap-to (length string)
   (format nil (catstr "~{~<~%~1," length ":;~A~> ~}")
      (split " " string)))

(defun send (msg dst)
   (iter (for x in (split "\\n" (wrap-to 100 msg)))
      (privmsg *connection* dst x)
      (sleep 0.4)))

(defun ud (term)
   (let ((json (decode-json-from-string (shell-command (format nil "curl 'api.urbandictionary.com/v0/define?term=~a'" term)))))
		(print json)
      (cdr (fifth (second (third json)))))) ;; second is where the nth should go

(defun clean (str)
   (string-trim '(#\Space #\Tab #\Newline) str))

(defun msg-hook (message)
   (let ((dest* 
            (if (string-equal (first (arguments message)) *nickname*)
                (source message)
                (first (arguments message))))
         (src* (source message))
         (data (car (last (arguments message)))))
			(setf dest dest* src src*)
			(let ((parsed-command (read-command data)))
					(when (and parsed-command (listp parsed-command) (not (member src *ignored* :test #'equalp)))
				   (let ((res (eval-command parsed-command)))
					      (when res
									(when (not (stringp res)) (setf res (format nil "~a" res)))
						 		 	(send (format nil "~a" res) dest)))))
   (finish-output)))

(defun notice-hook (m)
   (print m)
   (finish-output))

;; irc.rizon.net
(setf *connection* (connect :nickname *nickname* :server "irc.rizon.net"))
(add-hook *connection* 'irc-privmsg-message #'msg-hook)
(add-hook *connection* 'irc-notice-message #'notice-hook)

(read-message-loop *connection*)
(iter
 (print "errored out, starting loop again")
 (sleep 2)
 (read-message-loop *connection*))
