;; this is where all thebot code lives

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

(defun cute (nick)
   (let* ((n (eql 1 (random 2))) 
          (cuteset (if n cute1 cute2)) 
          (length (length (if n cute1 cute2)))
          (f (nth (random length) cuteset))) 
          (if n 
              (format nil f nick)
              (format nil f nick src))))

(defcommand cute (&rest args)
   (if (not (and (car args) (symbolp (car args))))
       "I don't know who you want me to cute!" 
       (cute (stringify (first args)))))

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
                            "raiding the cookie temple with Indy"
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
   (or (destructuring-match :mode string args
         (choice
           ((quantifier amount) cookie-type (cookie) "to" (irc-user n))  
           ((irc-user n) (quantifier amount) cookie-type (cookie)))     
         (transfer-cookies n amount (format nil fspaces cookie-type)))
       (incorrect-syntax)))

;; what would be nice:
;; (make-numbered-verb-command give cookie transfer-cookies)

(defcommand award (&rest args)
   (setf args (mapcar #'stringify args))
   (or (awhen (auth-user) (if (< (random 10) 7) it "as if you could award cookies"))
       (destructuring-match :mode string args
         (choice
           ((quantifier amount) cookie-type (cookie) "to" (irc-user n))  
           ((irc-user n) (quantifier amount) cookie-type (cookie)))     
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

(load "hyperspec.lisp")
