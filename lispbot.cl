(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(ql:quickload '(alexandria iterate parmesan cl-ppcre cl-store anaphora cl-irc trivial-shell cl-json))
(defpackage lispbot
 (:use cl alexandria iterate parmesan cl-ppcre cl-store anaphora cl-irc cl-json trivial-shell))
(in-package lispbot)

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
(defvar src nil)
(defvar dest nil)
(defvar *env* (make-hash-table :test #'equalp))

(defparser parse-sym (aif (many+ (choice letter sym)) (intern it)))
(defparser parse-int (aif (many+ digit) (parse-integer it)))
(defparser quoted    (between "\"" (many (choice (if (par "\\\"") "\"") (none-of "\""))) "\""))
(defparser term      (between (many whitespace) (choice quoted parse-sym parse-int sexp) (many whitespace)))
(defparser sexp      (between (many whitespace) (between "(" (sep-many term) ")") (many whitespace)))

(defun defcommand (name type arg-count handler)
	(setf (gethash name *env*) (list type arg-count handler)))

(defun eval-command (form)
	(if (listp form) 
		 (let ((def (gethash (string (car form)) *env*)))
				(if def
					(case (car def)
						(function 
							(if (eql (second def) (length (cdr form)))
								 (apply (third def) (mapcar #'eval-command (cdr form)))
								 (format nil "bad number of arguments to ~a, expected ~a" (car form) (second def))))
						(special
							(apply (third def) (cdr form))))
					(format nil "error: unknown symbol ~a" (car form))))
		 (if (symbolp form)
			  (aif (gethash (string form) *env*) it (format nil "error: unknown symbol ~a" form))
			  form)))

(defmacro defunc (name ll &rest body)
	`(defcommand ',(string name) 'function ',(length ll)
		(lambda ,ll ,@body)))

(defmacro defspec (name ll &rest body)
	`(defcommand ',(string name) 'special ',(length ll)
		(lambda ,ll ,@body)))

(defunc test (x)
	(format nil "it works! [~a]" x))

(defunc + (x y) (+ x y))
(defunc - (x y) (- x y))
(defunc * (x y) (* x y))
(defunc / (x y) (/ x y))

(defunc eql (x y) (equalp x y))

(defunc src () src)
(defunc dest () dest)

(defspec if (pred a &optional b)
	(if a b))

(defunc string (str) 
	(string-downcase (string str)))

(defspec cute (nick)
	(print nick)
	(format nil (nth (random (length *cute*)) *cute*) (string nick)))

(defspec join (channel)
	(progn (join *connection* (string-downcase (string channel)))
		    nil))

(defun read-command (str)
	(parse str (sexp)))

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

(defun save-data ()
   (store *members* "members-db")
   "data saved")

(defun load-data ()
   (setf *members* (restore "members-db"))
   "data loaded")

(defun ud (term)
   (let ((json (decode-json-from-string (shell-command (format nil "curl 'api.urbandictionary.com/v0/define?term=~a'" term)))))
      (cdr (fifth (second (third json)))))) ;; second is where the nth should go

(defmacro is-auth (exp)
   `(if (auth src) ,exp))

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
   	   (if (read-command data)
				 (if (gethash (string (car (read-command data))) *env*)
					(let ((res (eval-command (read-command data))))
					 (print (read-command data))
					 (if (not (equalp res ""))
						  (send (format nil "~a" res) dest)))))
   (finish-output)))

(defun notice-hook (m)
   (print m)
   (finish-output))

(setf *connection* (connect :nickname *nickname* :server "irc.rizon.net"))
(add-hook *connection* 'irc-privmsg-message #'msg-hook)
(add-hook *connection* 'irc-notice-message #'notice-hook)

(read-message-loop *connection*)
(iter
   (print "errored out, starting loop again")
   (sleep 2)
   (read-message-loop *connection*))
