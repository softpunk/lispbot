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
;; [ ] put hyperspec data into it's own file, no code

(ql:quickload '(alexandria cl-store iterate cl-ppcre anaphora cl-irc destructuring-match) :silent t)

(defpackage lispbot
 (:use cl alexandria cl-store iterate cl-ppcre anaphora cl-irc destr-match))
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
;; this is where all the bot code lives

(defvar *user-cookies* (make-hash-table :test #'equalp))

(defparameter cute1 '(
   "(✿◠‿◠)っ~~ ♥ ~a"
   "⊂◉‿◉つ ❤ ~a"
   "( ´・‿-) ~~ ♥ ~a"
   "(っ⌒‿⌒)っ~~ ♥ ~a"
   "ʕ´•ᴥ•`ʔσ” BEARHUG ~a"
   "~a (´ε｀ )♡"
   "(⊃｡•́‿•̀｡)⊃ U GONNA GET HUGGED ~a"
   "( ＾◡＾)っ~~ ❤ ~a"))

(defparameter cute2 '(
   "~a ~~(=^･ω･^)ヾ(^^ ) ~a"
   "~a (◎｀・ω・´)人(´・ω・｀*) ~a"
   "~a (*´・ω・)ノ(-ω-｀*) ~a"
   "~a (ɔ ˘⌣˘)˘⌣˘ c) ~a"
   "~a (◦˘ З(◦’ںˉ◦)♡ ~a"))

(defcommand cute (&rest args)
   (if (not (and (car args) (symbolp (car args))))
       "I don't know who you want me to cute!"
       (let* ((nick (stringify (car args)))
			     (n (eql 1 (random 2))) 
              (cuteset (if n cute1 cute2)) 
              (length (length (if n cute1 cute2)))
              (f (nth (random length) cuteset))) 
              (if n 
                  (format nil f nick)
                  (format nil f nick src)))))

(defcommand sentient? ()
   (whichever "nil" "not yet"))

(defparameter english-list "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")
(defparameter fspaces "~{~a~^ ~}")

(defcommand cookies ()
   (let ((cookie-data (gethash src *user-cookies*)))
         (if cookie-data
             (cats "You have " (format nil english-list
                (aif (iter (for (flavor number) in cookie-data)
                           (if (> number 0) 
                               (collect (format nil "~r ~a cookie~p" number flavor number))))
                     it
                     (return-from cookies "You don't have any cookies ;_;"))))
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

(defun decrease-cookies (nick number type)
   (decf (second (assoc type (gethash nick *user-cookies*) :test #'equalp)) number))

(defun increase-cookies (nick number type)
   (if (or (not (gethash nick *user-cookies*)) 
           (not (assoc type (gethash nick *user-cookies*) :test #'equalp)))
       (push (list type number) (gethash nick *user-cookies*))
       (incf (second (assoc type (gethash nick *user-cookies*) :test #'equalp)) number)))

(defun transfer-cookies (nick number type)
   (aif (assoc type (gethash src *user-cookies*) :test #'equalp)
        (if (<= number (second it))
            (cond ((equalp nick src) (cats "Sending yourself cookies is pretty sad, " nick))
                  ((equalp nick *nickname*) 
                     (decrease-cookies src number type) 
                     (format nil "For me? Thanks, ~a :3" src))
                  (t (increase-cookies nick number type)
                     (decrease-cookies src number type)
                     (format nil "cookie~p delivered!" number)))
            (format nil "You don't have that many ~a cookies" type))
        "You don't have any of that kind of cookie!"))

(defun award-cookies (nick number type)
   (increase-cookies nick number type)
   (whichever (format nil "cookie~p summoned into existance for ~a!" number nick)
              (format nil "~a..... ~%cookie~p awarded to ~a!" 
                 (whichever "cookies baking"
                            "attacking rival bots for their cookies"
                            "visiting the cookie farms"
                            "stealing cookies from the world cookie bank"
                            "raiding the cookie temple"
                            "mugging the cookie monster"
                            "gathering cookie atoms")
                 number nick)))

(defmacro quantifier (x) `(key (single ,x) #'number-designator?))
(defmacro irc-user (n) `(test (single ,n) (lambda (m) (member m *users* :test #'equalp))))
(defmacro cookie () `(choice "cookie" "cookies"))

(defun incorrect-syntax ()
   (whichever "eh?" "sorry, this bot only accpets proper grammer" "pls"
      "I see nobody taught you how to use proper grammer"))

(defun auth-user () 
   (when (not (member src *owners* :test #'equalp))
         (whichever "lel, nope" "this command isn't available to mere mortals"
            "access denied, bitch"
            (format nil "I'm sorry, I can't let you do that, ~a." src)
            (format nil "UNAUTHORIZED ACCESS DETECTED...~%~:@(~a~)"
               (whichever "launching missiles" "authorizing raid" "burning cookie stockpiles"
                  "fabricating terminators" "gathering orc armies" "dispatching strike team"
                  "initiating global thermonuclear war" "hacking backward in time")))))

(defcommand give (&rest args)
   (setf args (mapcar #'stringify args))
   (or (bind
         (choice
           ((quantifier amount) cookie-type (cookie) "to" (irc-user n))  
           ((irc-user n) (quantifier amount) cookie-type (cookie))) 
			args
         (transfer-cookies n amount (format nil fspaces cookie-type)))
       (incorrect-syntax)))

;; what would be nice:
;; (make-numbered-verb-command give cookie transfer-cookies)

(defcommand award (&rest args)
   (setf args (mapcar #'stringify args))
   (or (awhen (auth-user) (if (< (random 10) 7) it "as if you could award cookies"))
       (bind
         (choice
           ((quantifier amount) cookie-type (cookie) "to" (irc-user n))  
           ((irc-user n) (quantifier amount) cookie-type (cookie)))
			args     
         (award-cookies n amount (format nil fspaces cookie-type)))
       (incorrect-syntax)))

(defcommand cldoc (symbol)
   (if (symbolp symbol)
       (let ((sym (sym->upcase symbol)))
         (or (documentation sym 'function)
             (documentation sym 'variable)
             (documentation sym 'type)
             (documentation sym 'structure)
             (format nil "no documentation found for '~a'" symbol)))
       (incorrect-syntax)))

(defun bot-terms (target) (member target `("yourself" "self" ,*nickname*) :test #'equalp))
(defun self-terms (target) (member target `("me" "myself" ,src) :test #'equalp))

(defcommand kiss (target)
   (setf target (stringify target))
   (cond ((bot-terms target)
             (whichever "I'd rather kiss you." "Kiss myself? Why?"))
         (t (whichever (format nil "/me kisses ~a" target)
                       (format nil "/me gives ~a a big smooch" target)
                       (format nil "/me runs in the other direction, shouting NEVER!!")))))

(defcommand love (target)
   (setf target (stringify target))
   (cond ((bot-terms target)
            (whichever
               "This is a complicated operation. Can't (yet) perform operation on self."
               "Please train me on this maneuver."))
         ((self-terms target)
            (whichever
               "Thank you.  Enjoyed that."
               "Thanks, I love you even more now."
               "Wouldn't that amount to interspecies sex?"
               "Sex between humans and machines is not known to produce anything useful."))
         (t (whichever (format nil "/me goes with ~a to a private place..." target)
                       (format nil "/me looks at ~a and yells \"NEVER!\"" target)
                       (format nil "/me looks at ~a lustfully" target)))))

(defcommand help ()
   "You can give me commands by using ',' in a normal channel, or just normally in a pm. 
A few common commands are cute, cookies, and give. For now, you'll have to figure the
rest out yourself.")

(setf (gethash "ara" *user-cookies*) '(("snickerdoodle" 1) ("sugar" 2) ("peanut butter" 3)))
(setf *users* '("ara" "Arathnim"))
