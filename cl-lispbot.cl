(proclaim '(optimize (speed 0) (safety 3) (debug 3) (space 0)))
;;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(ql:quickload '(alexandria iterate cl-ppcre cl-store anaphora cl-irc trivial-shell cl-json))
(defpackage cl-ircbot
   (:use cl alexandria iterate cl-ppcre cl-store anaphora cl-irc cl-json trivial-shell))
(in-package cl-ircbot)

(make-random-state)

(defvar *connection* nil)
(defvar *nickname* "lispbot")
(defvar *members* '("Arathnim"))
(defvar *commands* nil)
(defvar matched-symbols nil)

(defun single-quote-reader (stream char)
   (declare (ignore char))
   `(gethash ',(read stream t nil t) matched-symbols))

(set-macro-character #\% #'single-quote-reader)

(defun defcommand (head &rest forms)
   (push (list head forms) *commands*)
   "command form added")

(defun quote-list (lst)
   (mapcar (lambda (x) (car `(',x))) lst))

(defmacro defcom (head &rest forms)
   `(defcommand ,head ,@(quote-list forms)))

;; WARNING! psychotic code - approach with caution

(defun mushy-eval (command-str &optional source destination)
   (setf command-str (string-trim '(#\Space #\Tab #\Newline) command-str))
   (let* ((command (find-head command-str)) 
          (forms (cadr command))
          (form nil) (matches nil) (symbols nil)
          (matched-symbols (make-hash-table :test 'eq)))
      (block outer 
         (loop for f in forms do
            (multiple-value-bind (a b) 
               (scan-to-strings (convert-form f) command-str)
                  (if a (progn 
                     (setf matches b form f symbols (collect-symbols (car f)))
                     (return-from outer nil))))))
      (if (not form) (return-from mushy-eval 
         "Can't match your command form."))
      (if (and symbols (not (equalp matches #()))) 
         (loop for s in symbols for m across matches do
         (setf (gethash s matched-symbols) m)))
      (let ((src source) (dest destination))
         (declare (special src))
         (declare (special dest))
         (declare (special matched-symbols))
         (eval (cadr form)))))

(defun convert-head (head)
   (format nil "^~a" head))

(defun find-head (command)
   (loop for c in *commands* do
      (if (all-matches (convert-head (car c)) command)
         (return-from find-head c))))

(defun convert-form (form)
   (setf form (car form))
   (format nil "^~a$"
      (build-regex form)))

(defun build-regex (elt)
   (cond ((stringp elt) elt)
         ((symbolp elt) "(.+?)")
         ((eq (car elt) 'optional) 
            (format nil "~{(?:~a)?~}" (mapcar #'build-regex (cdr elt))))
         ((eq (car elt) 'switch) 
            (format nil "(?:~{~a~^|~})" (mapcar #'build-regex (cdr elt))))
         ((listp elt) (format nil "~{~a~}" (mapcar #'build-regex elt)))
         (t (error "Invalid element in command declaration:~a" elt))))

(defun collect-symbols (form)
   (let ((res nil))
      (cond ((listp form) 
         (setf res (alexandria:flatten (mapcar #'collect-symbols form))))
            ((and (symbolp form) (not (eq form 'optional)) (not (eq form 'switch))) 
               (setf res (push form res)))
            (t nil)) res))

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
   (store *harm* "harm")
   (store *members* "members-db")
   "data saved")

(defun load-data ()
   (setf *harm* (restore "harm"))
   (setf *members* (restore "members-db"))
   "data loaded")

(defun ud (term)
   (let ((json (decode-json-from-string (shell-command (format nil "curl 'api.urbandictionary.com/v0/define?term=~a'" term)))))
      (cdr (fifth (second (third json)))))) ;; second is where the nth should go

(defmacro is-auth (exp)
   `(if (auth src) ,exp))

(defun clean (str)
   (string-trim '(#\Space #\Tab #\Newline) str))

(defcom "\\.bots"
   ((".*") (send "Reporting in! [Common Lisp]" dest)))

(defun harmful (&rest harmful-things) (iter (for x in harmful-things) (push x *harm*)) "harmful software added.")
(defvar *harm* '("GCC" "make" "autoconf" "automake" "C" "XML" "F#" "C#" "Java" "clojure" "JS" "C++" "web" "harmful"))
(defvar *cute* '(
   "(✿◠‿◠)っ~~ ♥ ~a"
   "⊂◉‿◉つ ❤ ~a"
   "( ´・‿-) ~~ ♥ ~a"
   "(っ⌒‿⌒)っ~~ ♥ ~a"
   "ʕ´•ᴥ•`ʔσ” BEARHUG ~a"
   "~a (´ε｀ )♡"
   "(⊃｡•́‿•̀｡)⊃ U GONNA GET HUGGED ~a"
   "( ＾◡＾)っ~~ ❤ ~a"))

(defcom "%"
   ((".neko") (send (nth (random 3) '("mew ฅ^•ﻌ•^ฅ" "meow (^・ω・^ )" "nyaa (=^･^=)")) dest))
   ((".help") 
      (send "I keep track of ideas and evaluate lisp expressions!  Use 'whoami' to see if you're authorized to evaluate things. Other commands include eval, authorize, whoami, and add-harmful.
Type (new <character name>) to start a new character on the D&D module." src))
   ((".whoami") (send (if (auth src) "You're authorized!" "You're unauthorized, and likely to be eaten by a grue.") dest))
   ((".exit") (is-auth (sb-ext:exit)))
   ((".eval " exp) (is-auth (send (ignore-errors (write-to-string (eval (read-from-string %exp)))) dest)))
   ((".% " exp) (is-auth (send (ignore-errors (write-to-string (eval (read-from-string %exp)))) dest)))
   ((".authorize " nick) (is-auth (push %nick *members*)))
   ((".cute " nick) (send (format nil (nth (random (length *cute*)) *cute*) %nick) dest)))

(defcom "%add-harmful" (("%add-harmful " software) (harmful %software)))
(defcom "%harmful" ((".*") (send (format nil "~a considered harmful" (nth (random (length *harm*)) *harm*)) dest)))

(defcom "%ud"
   ((".ud " term) (send (ud %term) dest)))

(defcom "(ld)|(cd)" 
   (("(ld)") (send "This isn't a shell, dummy!" dest)))

(defcom "#!"
   (("#! " command) (is-auth (send (clean (shell-command %command)) dest))))

(defcom "\\(" 
   (("\\(" exp "\\)") 
      (progn 
         (mushy-eval (format nil "%~a" %exp) src dest)
         (mushy-eval (format nil "$~a" %exp) src dest))))

(defcom "{" 
   (("{" exp "}")
         (mushy-eval (format nil "(eval (~a))" %exp) src dest)))

(defun msg-hook (message)
   (let ((dest 
            (if (string-equal (first (arguments message)) *nickname*)
               (source message)
               (first (arguments message))))
         (src (source message))
         (data (car (last (arguments message)))))
   (mushy-eval data src dest)
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
