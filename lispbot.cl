(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
(ql:quickload '(alexandria cl-store cl-ppcre iterate anaphora cl-irc) :silent t)

(defpackage lispbot
 (:use cl alexandria cl-store iterate cl-ppcre anaphora cl-irc))
(in-package lispbot)

(setf *read-eval* nil)
(make-random-state)

(defconstant *cute1* '(
   "(✿◠‿◠)っ~~ ♥ ~a"
   "⊂◉‿◉つ ❤ ~a"
   "( ´・‿-) ~~ ♥ ~a"
   "(っ⌒‿⌒)っ~~ ♥ ~a"
   "ʕ´•ᴥ•`ʔσ” BEARHUG ~a"
   "~a (´ε｀ )♡"
   "(⊃｡•́‿•̀｡)⊃ U GONNA GET HUGGED ~a"
   "( ＾◡＾)っ~~ ❤ ~a"))

(defconstant *cute2* '(
   "~a ~~(=^･ω･^)ヾ(^^ ) ~a"
   "~a (◎｀・ω・´)人(´・ω・｀*) ~a"
   "~a (*´・ω・)ノ(-ω-｀*) ~a"
   "~a (ɔ ˘⌣˘)˘⌣˘ c) ~a"
   "~a (◦˘ З(◦’ںˉ◦)♡ ~a"))

(defvar *connection* nil)
(defvar *nickname* "lispbot")
(defvar *ignored* '("nv" "trinary" "tca" "shinrei"))
(defvar *channels* nil)
(defvar *channel-users* (make-hash-table :test #'equalp))
(defvar *user-cookies* (make-hash-table :test #'equalp))
(defvar *users* nil)
(defvar src nil)
(defvar dest nil)

(defun clean (str)
   (string-trim '(#\Space #\Tab #\Newline) str))

(defun join-channel (channel)
   (progn (join *connection* channel)
          nil))

(defun cat (list)
   (format nil "~{~a~}" list))

(defmacro cats (&rest rest)
	`(cat (list ,@rest)))

(defun wrap-to (length string)
   (format nil (cats "~{~<~%~1," length ":;~A~> ~}")
      (split " " string)))

(defun stringify (s)
   (if (symbolp s)
       (string s)
       s))

(defun sym-downcase (s) (string-downcase (string s)))

(defun send (msg dst)
   (iter (for x in (split "\\n" (wrap-to 100 msg)))
      (privmsg *connection* dst x)
      (sleep 0.4)))

(defmacro strip-to-capture-group (regex str)
   (with-gensyms (a) `(register-groups-bind (,a) (,regex ,str) ,a)))

(defconstant single-char-trigger ",")
(defconstant inline-char-trigger ",,")

(defun parse-command (str)
   (or (when inline-char-trigger
         (let ((l (split inline-char-trigger str))) 
          (when (not (eql (length l) 1)) (second l))))
       (when inline-char-trigger
         (strip-to-capture-group (format nil "^ *~a(.*)" single-char-trigger) str))
       (strip-to-capture-group (format nil "^~a:(.*)" *nickname*) str)))

;; TODO replace with list of functions, send the result given by the first that matches
(defparameter commands (make-hash-table :test #'equalp))

(defmacro defcommand (name ll &rest body)
   `(setf (gethash ,(string-downcase name) commands) (lambda ,ll ,@body)))

(defun cute (nick)
   (let* ((n (eql 1 (random 2))) (cuteset (if n *cute1* *cute2*)) (length (length (if n *cute1* *cute2*)))
          (f (nth (random length) cuteset))) 
          (if n 
              (format nil f nick)
              (format nil f nick src))))

(defcommand cute (args)
   (if (not (first args))
       "I don't know who you want me to cute!" 
       (cute (stringify (first args)))))

(defcommand sentient? (args)
   (declare (ignore args))
   (whichever "nil" "not yet"))

(defparameter english-list "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")
(defparameter fspaces "~{~a~^ ~}")

(defcommand cookies (args)
	(declare (ignore args))
	(let ((cookie-data (gethash src *user-cookies*)))
			(if cookie-data
			    (cats "You have " (format nil english-list
					 (iter (for (flavor number) in cookie-data)
							 (collect (format nil "~r ~a cookie~p" number flavor number)))) "!")
				 "You don't have any cookies ;_;")))

(defparameter english-number-list (iter (for x from 0 to 100) (collect (format nil "~r" x))))

(defun number-designator? (n)
	(or (when (numberp n) n)
		 (position n english-number-list :test #'equalp)
		 (when (equalp n "a") 1)))

(defun is-cookie? (s)
	(or (equalp s "cookie") (equalp s "cookies")))

(defmacro nth-from-end (n seq)
	`(nth (- (length ,seq) (+ ,n 1)) ,seq))

(defun transfer-cookies (nick number type)
	(format nil "cookies transferred ~s" (list nick number type)))

;; nick-first and nick-last formats
;; give NUMBER/a TYPE cookie(s) to NICK 
;; give NICK NUMBER/a TYPE cookie(s)
(defcommand give (args)
	(setf args (mapcar #'stringify args))
	(if (is-cookie? (nth-from-end 2 args)) 
		 (if (equalp (nth-from-end 1 args) "to")
			  (aif (number-designator? (first args))
					 (if (member (nth-from-end 0 args) *users* :test #'equalp)
						  (let ((flavor (subseq args 1 (- (length args) 3))))
								  (if flavor
									   (transfer-cookies (nth-from-end 0 args) it (format nil fspaces flavor))
										"What kind of cookies?"))
		  				  "Who?")
				    "How many cookies?")
			  "What do you want to do with those cookies?")
		 (if (is-cookie? (nth-from-end 0 args))
			  (aif (number-designator? (second args))
					 (if (member (first args) *users* :test #'equalp)
						  (let ((flavor (subseq args 2 (- (length args) 1))))
								  (if flavor
										(transfer-cookies (first args) it (format nil fspaces flavor))
										"What kind of cookies?"))
						  "Who?")
					 "How many cookies?")
			  "What do you want to give?")))

;; what would be nice:
;; (make-numbered-verb-command give cookie transfer-cookies)

;; this is a hack
(defun lispify (str)
   (let ((r nil)) 
         (setf (readtable-case *readtable*) :preserve)
         (setf r 
				(multiple-value-bind (res err)
					(ignore-errors
						(if (eql (char str 0) #\()
						    (read-from-string str)
						    (read-from-string (concatenate 'string "(" str ")"))))
					(if (not (numberp err)) "read error!" res)))
         (setf (readtable-case *readtable*) :upcase)
         r))

(defun command-handler (str)
   (when (not (equalp str ""))
         (let ((form (lispify str)))
					(if (not (listp form))
						 form
						 (let ((command (gethash (string-downcase (car form)) commands)))
								 (if (not command)
                             (send 
										(format nil "I don't know the term '~a'" 
											(string-downcase (car form))) 
										dest)
                             (send (funcall command (cdr form)) dest)))))))

(defun msg-hook (message)
   (let ((dest* (if (string-equal (first (arguments message)) *nickname*)
                    (source message)
                    (first (arguments message))))
         (src* (source message))
         (data (car (last (arguments message)))))
         (setf dest dest* src src*)
         (awhen (and (not (member src *ignored* :test #'equalp)) 
                     (or (unless (eql (char dest 0) #\#) data) (parse-command data)))
                (command-handler (clean it)))
   (finish-output)))

(defun notice-hook (m)
   (print m)
   (finish-output))

(defun strip-sigil (s)
   (strip-to-capture-group "[+%@~]?(.+)" s))

(defun namereply-hook (m)
   (let ((nicks (mapcar #'strip-sigil (split " " (fourth (arguments m)))))) 
			(appendf (gethash (third (arguments m)) *channel-users*))
			(setf *users* (union *users* nicks :test #'equalp))))

(defun set-server (name)
   (setf *connection* (connect :nickname *nickname* :server name))
   (add-hook *connection* 'irc-privmsg-message #'msg-hook)
   (add-hook *connection* 'irc-rpl_namreply-message #'namereply-hook)
   (add-hook *connection* 'irc-notice-message #'notice-hook))

(defun start-irc (server &optional nick)
   (when nick (setf *nickname* nick))
   (set-server server)
   (sb-thread:make-thread 'main-irc-loop :name "irc"))

(defun main-irc-loop ()
   (read-message-loop *connection*)
   (iter
      (print "errored out, starting loop again")
      (sleep 2)
      (read-message-loop *connection*)))

(start-irc "irc.rectilinear.xyz" "lbt")
(sleep 2)
(send "identify SHA-8192" "nickserv")
(setf (gethash "ara" *user-cookies*) '(("snickerdoodle" 1) ("sugar" 2) ("peanut butter" 3)))
