(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))
(setf *print-case* :downcase)
(ql:quickload '(alexandria cl-ppcre iterate anaphora cl-irc) :silent t)

(defpackage lispbot
 (:use cl alexandria iterate cl-ppcre anaphora cl-irc))
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
(defvar src nil)
(defvar dest nil)

(defun clean (str)
   (string-trim '(#\Space #\Tab #\Newline) str))

(defun cute (nick)
   (let* ((n (eql 1 (random 2))) (cuteset (if n *cute1* *cute2*)) (length (length (if n *cute1* *cute2*)))
          (f (nth (random length) cuteset))) 
          (if n 
              (format nil f (format nil "~a" nick))
              (format nil f (format nil "~a" nick) (format nil "~a" src)))))

(defun join-channel (channel)
   (progn (join *connection* channel)
          nil))

(defun catstr (&rest rest)
   (format nil "~{~a~}" rest))

(defun wrap-to (length string)
   (format nil (catstr "~{~<~%~1," length ":;~A~> ~}")
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

(defcommand cute (args)
   (if (not (first args))
       "I don't know who you want me to cute!" 
       (cute (stringify (first args)))))

(defcommand sentient? (args)
   (declare (ignore args))
   (whichever "nil" "not yet"))

;; this is a hack
(defun lispify (str)
   (let ((r nil)) 
         (setf (readtable-case *readtable*) :preserve)
         (setf r (if (eql (char str 0) #\()
                     (read-from-string str)
                     (read-from-string (concatenate 'string "(" str ")"))))
         (setf (readtable-case *readtable*) :upcase)
         r))

(defun command-handler (str)
   (when (not (equalp str ""))
         (let* ((form (lispify str)) (command (gethash (string-downcase (car form)) commands)))
                (if (not command)
                    (send (format nil "I don't know the term '~a'" (string-downcase (car form))) dest)
                    (send (funcall command (cdr form)) dest)))))

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
   (appendf (gethash (third (arguments m)) *channel-users*) 
            (mapcar #'strip-sigil (split " " (fourth (arguments m))))))

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

