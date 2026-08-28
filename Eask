;; -*- mode: eask; lexical-binding: t -*-

(package "slackit"
         "0.1.0"
         "Appkit-based Slack client")

(website-url "https://github.com/emacs-slack/emacs-slackit")
(keywords "comm")
(package-file "slackit.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "29.1")
(depends-on "appkit" :file "../appkit.el")
(depends-on "browser-session" :file "../browser-session")
(depends-on "plz" "0.8")
(depends-on "websocket" "1.16")

(script "test" "eask test ert --quiet test/*.el")
(script "test-local"
        "eask -f install-file ../appkit.el && eask -f install-file ../browser-session && eask recompile && eask test ert --quiet test/*.el")
