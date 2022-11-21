(add-to-load-path (dirname (current-filename)))
(use-modules
 (yasnippet-to-tempel-converter)
 (srfi srfi-64))

(define-syntax test-with-proc
  (syntax-rules ()
    ((test-with-proc testing-proc proc
                     (test-name0 in0 expected-out0)
                     (test-name in expected-out) ...)
     (begin
       (testing-proc test-name0 (proc in0) expected-out0)
       (testing-proc test-name (proc in) expected-out) ...))))



(test-begin "yasnippet-string->yasnippet-tree")

(test-with-proc
 test-equal
 yasnippet-string->yasnippet-tree
 ("testo" "in" '(yasnippet (snippet "in"))))

(test-with-proc
 test-equal
 yasnippet-string->yasnippet-tree
 ("basic"
  "aoeu"
  '(yasnippet
    (snippet "aoeu")))
 ("tab-stop"
  "aoeu$0"
  '(yasnippet
    (snippet "aoeu" (tab-stop (number "0")))))
 ("tab-stop with init-value"
  "aoeu${1:value})"
  '(yasnippet
    (snippet "aoeu" (tab-stop (number "1")
                              (init-value "value"))
             ")")))
 ("escape sequences"
  "test\\`\\$\\`\\\\"
  '(yasnippet (snippet "test`$`\\")))
 ("metadata"
  "# -*- mode: snippet -*-
# name: emacs_value
# key: ev
# --
emacs_value"
  '(yasnippet (metadata ((key "name") (value "emacs_value"))
                        ((key "key") (value "ev")))
    (snippet "emacs_value")))

 ("stupid metadata"
  "# -*- mode: snippet -*-
# name: left< right>
# key: <
# --
aoeu"
  '(yasnippet (metadata
               ((key "name") (value "left< right>"))
               ((key "key") (value "<")))
    (snippet "aoeu")))

 ("tabStops"
  "(overlay-put ${1: ov} ${2:property} ${0:value})"
  '(yasnippet (snippet
               "(overlay-put "
               (tab-stop (number "1") (init-value " ov"))
               " "
               (tab-stop (number "2") (init-value "property"))
               " "
               (tab-stop (number "0") (init-value "value"))
               ")")))
 ("embedded-lisp"
  "(overlay-put `(string ?a ?b ?c)`)"
  '(yasnippet (snippet
               "(overlay-put "
               (embedded-lisp "(string ?a ?b ?c)")
               ")")))
 ("field transformation"
  "$1 ${1:$(capitalize yas-text)}"
  '(yasnippet
    (snippet
     (tab-stop (number "1"))
     " "
     (tab-stop
      (number "1")
      (transformation-expr "(capitalize yas-text)")))))
 ("multiple transformations"
  "${1:$(make-string (string-width yas-text) ?\\=)}
${1:Title}
${1:$(make-string (string-width yas-text) ?\\=)}

$0"
  '(yasnippet
    (snippet
     (tab-stop
      (number "1")
      (transformation-expr
       "(make-string (string-width yas-text) ?\\=)"))
     "\n"
     (tab-stop (number "1") (init-value "Title"))
     "\n"
     (tab-stop
      (number "1")
      (transformation-expr
       "(make-string (string-width yas-text) ?\\=)"))
     "\n\n"
     (tab-stop (number "0")))))
 ("unescaped verbatim dollar in snippet text"
  "# -*- mode: snippet -*-
# name: complexity
# key: comp
# --
\\complexity{$O($0)$}
"
  '(yasnippet
    (metadata
     ((key "name") (value "complexity"))
     ((key "key") (value "comp")))
    (snippet
     "\\complexity{$O("
     (tab-stop (number "0"))
     ")$}\n")))
 ("Unusual modeline"
  "# -*- mode: snippet; require-final-newline: nil -*-
# name: Header 3
# key: h3
# uuid: h3
# --
### ${1:Header 3}`(unless markdown-asymmetric-header \" ###\")`"
  '(yasnippet (metadata
               ((key "name") (value "Header 3"))
               ((key "key") (value "h3"))
               ((key "uuid") (value "h3")))
    (snippet
     "### "
     (tab-stop (number "1") (init-value "Header 3"))
     (embedded-lisp
      "(unless markdown-asymmetric-header \" ###\")"))))
 ("indent mark"
  "(progn
$>$0)"
  '(yasnippet (snippet
               "(progn\n"
               indent-mark
               (tab-stop (number "0"))
               ")")))
 ("comment in the metadata lines - WONTFIX"
  "# uuid: matrix
# bla bla, comment not metadata
# --
aoeu"
  '(yasnippet
    (snippet
     "# uuid: matrix\n# bla bla, comment not metadata\n# --\naoeu")))
 ("default value is a lisp expr"
  "${1:`(current-time-string)`}"
  '(yasnippet (snippet
               (tab-stop (number "1")
                         (init-value
                          (embedded-lisp "(current-time-string)"))))))
 ("has a space between contributor and colon"
  "# -*- mode: snippet -*-
# contributor : Jimmy Wu <frozenthrone88@gmail.com>
# group: meta
# name: <meta name=\"...\" content=\"...\" />
# --
<meta name=\"${1:generator}\" content=\"${2:content}\" />"
  '(yasnippet
    (metadata
     ((key "contributor") (value "Jimmy Wu <frozenthrone88@gmail.com>"))
     ((key "group") (value "meta"))
     ((key "name") (value "<meta name=\"...\" content=\"...\" />")))
    (snippet
     "<meta name=\""
     (tab-stop (number "1") (init-value "generator"))
     "\" content=\""
     (tab-stop (number "2") (init-value "content"))
     "\" />")))
 ("modeline but no separator. I thought it wasn't legal but it shows up in a\
bunch of lua snippets"
  "# -*- mode: snippet -*-
math.max(${0:x, y, ...})"
  '(yasnippet (snippet
               "math.max("

               (tab-stop (number "0")
                         (init-value "x, y, ..."))
               ")")))
 ("double hashes on modeline"
  "## -*- mode: snippet -*-
# name: receive
# key: rcv
# --
receive do
  $0
end"
  '(yasnippet
    (metadata
     ((key "name") (value "receive"))
     ((key "key") (value "rcv")))
    (snippet
     "receive do\n  "
     (tab-stop (number "0"))
     "\nend")))
 ("double modeline"
  "# -*- mode: snippet -*-
# -*- coding: utf-8 -*-
# name: pr
# key: pr
# --
(prn $1)
$0"
  '(yasnippet
    (metadata
     ((key "name") (value "pr"))
     ((key "key") (value "pr")))
    (snippet
     "(prn "
     (tab-stop (number "1"))
     ")\n"
     (tab-stop (number "0")))))
 ("hashes after separator"
  "# -*- mode: snippet -*-
# name: cont
# key: cont
# uuid: cont
# --
# contributor: `user-full-name`"

  '(yasnippet
    (metadata
     ((key "name") (value "cont"))
     ((key "key") (value "cont"))
     ((key "uuid") (value "cont")))
    (snippet
     "# contributor: "
     (embedded-lisp "user-full-name"))))
 ("no space before metadata value"
  "# -*- mode: snippet -*-
# name: cont
# key:#
# uuid: cont
# --
aoeu"
  '(yasnippet
    (metadata
     ((key "name") (value "cont"))
     ((key "key") (value "#"))
     ((key "uuid") (value "cont")))
    (snippet "aoeu"))))

(test-end)

(test-begin "yas-string->tempel")

(test-with-proc
 test-equal
 yasnippet-string->tempel
 ("metadata"
  "# -*- mode: snippet -*-
# name: emacs_value
# key: ev
# --
emacs_value" '(ev "emacs_value"))
 ("basic"
  "help" '(unspecified-key "help"))
 ("field"
  "help$1" '(unspecified-key "help" q))
 ("field with mirror - now it needs a name"
  "help$1 $1" '(unspecified-key "help" (s field-1) " " (s field-1)))
 ("field with default"
  "help${1:default}" '(unspecified-key "help" (p "default")))
 ("embedded lisp"
  "help `(current-time-string)`" '(unspecified-key "help " (current-time-string)))
 ("field with default as embedded lisp"
  "${1:`(current-time-string)`}" '(unspecified-key (p (current-time-string))))
 ("mirror transformation"
  "$1${1:$(capitalize yas-text)}" '(unspecified-key (s field-1) (capitalize field-1)))
 ("region"
  "`yas-selected-text`" '(unspecified-key r))
 ("region"
  "`%`" '(unspecified-key r))
 ("unsupported use case of yas-text. this is the expected (mis)behavior"
  "`(capitalize %)`" '(unspecified-key (capitalize %)))
 ("use 'q when appropriate"
  "if ($1) { $0 }" '(unspecified-key "if (" p ") { " q " }"))
 ("use 'q when appropriate"
  "if ($1) { $3 }" '(unspecified-key "if (" p ") { " q " }"))
 ("use 'q when appropriate"
  "if ($1) { ${3:default} }" '(unspecified-key "if (" p ") { " (p "default") " }"))
 ("use 'n"
  "a\nb" '(unspecified-key "a" n "b"))
 ("use 'n and 'n>"
  "print a\n begin\n   print b\nend" '(unspecified-key "print a" n>
                                       "begin" n> "print b" n "end")))

(test-end)
