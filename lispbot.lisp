(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
;(declaim #+sbcl(sb-ext:muffle-conditions warning))

;; TODO
;; [x] switch over to destructuring-match for give and award
;; [ ] make a more general inventory system
;; [ ] note/definition thing
;; [ ] better auth methods
;; [ ] number designator 'all'
;; [ ] better server handling
;; [ ] sandbox'd evaluation
;; [ ] improved help, esp. for each command
;; [ ] documentation scraping
;; [ ] /me irc and parsing

(ql:quickload '(alexandria cl-store iterate cl-ppcre anaphora cl-irc destructuring-match) :silent t)

(defpackage lispbot
 (:use cl alexandria cl-store iterate cl-ppcre anaphora cl-irc destr-match)
 (:shadowing-import-from destr-match switch))
(in-package lispbot)

(setf *read-eval* nil)
(setf *random-state* (make-random-state t))

(defvar *connection* nil)
(defvar *nickname* "lispbot")
(defvar *owners* '("Arathnim" "ara"))
(defvar *ignored* nil)
(defvar *channels* nil)
(defvar *channel-users* (make-hash-table :test #'equalp))
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

(defun sym->upcase (s) (intern (string-upcase (string s))))

(defun sym-downcase (s) (string-downcase (string s)))

(defun send (msg dst)
   (iter (for x in (split "\\n" (wrap-to 100 msg)))
      (privmsg *connection* dst x)
      (sleep 0.4)))

(defmacro strip-to-capture-group (regex str)
   (with-gensyms (a) `(register-groups-bind (,a) (,regex ,str) ,a)))

(defparameter single-char-trigger ",")
(defparameter inline-char-trigger ",,")

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
   `(setf (gethash ,(string-downcase name) commands) 
          (lambda ,ll (block ,(intern (string-upcase name)) ,@body))))

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
                             (send (format nil "I don't know the term '~a'" 
                                      (string-downcase (car form))) 
                                   dest)
                             (let ((r (ignore-errors (apply command (cdr form))))) 
                                (send (or r "wut?") dest))))))))

(defun dummy-handler (str)
   (when (not (equalp str ""))
         (let ((form (lispify str)))
               (if (not (listp form))
                   form
                   (let ((command (gethash (string-downcase (car form)) commands)))
                         (if (not command)
                             (format nil "I don't know the term '~a'" 
                                (string-downcase (car form)))
                             (let ((r (ignore-errors (apply command (cdr form))))) 
                                (or r "wut?"))))))))

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

(defun strip-sigil (s)
   (strip-to-capture-group "[+%@~]?(.+)" s))

(defun namereply-hook (m)
   (let ((nicks (mapcar #'strip-sigil (split " " (fourth (arguments m)))))) 
         (appendf (gethash (third (arguments m)) *channel-users*))
         (setf *users* (union *users* nicks :test #'equalp))))

(defun set-server (name)
   (setf *connection* (connect :nickname *nickname* :server name))
   (add-hook *connection* 'irc-privmsg-message #'msg-hook)
   (add-hook *connection* 'irc-join-message 
      (lambda (m) (setf *users* (union *users* (list (source m)) :test #'equalp))))
   (add-hook *connection* 'irc-rpl_namreply-message #'namereply-hook))

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

(load "commands.lisp")
