(asdf:defsystem "lispbot"
	:serial t
	:version "0.1"
	:author "Dylan Ball <arathnim@gmail.com>"
	:maintainer "Dylan Ball <arathnim@gmail.com>"
	:description "cute irc bot"
	:depends-on (alexandria cl-store cl-ppcre iterate anaphora cl-irc destructuring-match)
	:components ((:file "lispbot")))
