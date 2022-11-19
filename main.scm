(use-modules (ice-9 peg)
             (ice-9 textual-ports)
             (ice-9 pretty-print))

(define *yasnippet*
  (let* ((port (open-input-file "/home/ym/.config/doom/snippets/emacs-lisp-mode/overlay-put"))
         (snippet (get-string-all port)))
    (close-port port)
    snippet))
;; The string pattern equivalent of these doesn't match special chars e.g ' ' and
;; '(' for some reason, which is why they are written in sexp
(define-peg-pattern notGrave         body (or (range #\x07 #\x5F) (range #\x61 #\x10FFFF)))
(define-peg-pattern notDollar        body (or (range #\x07 #\x10FFFF)))
(define-peg-pattern notRCB           body (or (range #\x07 #\x7C) (range #\x7E #\x10FFFF)))
(define-peg-pattern notParen         body (or (range #\x07 #\x27) (range #\x2a #\x10FFFF)))
(define-peg-pattern notNL            body (or (range #\x07 #\x09) (range #\x0b #\x10FFFF)))


(define-peg-string-patterns
  "yasnippet <-- preamble? snippet !.
preamble <- (modeline metadata? metadataEnd) / (metadata? metadataEnd)
modeline < '# -*-' (notNL !'*-')* '-*-' NL+
metadata <-- metadataLine+
metadataLine <- mtStart key mtSep value NL+
key <-- ([a-zA-Z_] / '-')+
value <-- notNL+
metadataEnd < '# --' NL
mtStart < '# '
mtSep < ': '

snippet <-- (tab-stop / indent-mark / embedded-lisp / snippet-text)*

tab-stop <-- DOLLAR (number / (LCB (number COLON)? (transformation-expr / init-value) RCB))
number <-- NUMBER
init-value <-- (notRCB / embedded-lisp)*

indent-mark <-- INDENT-MARK
embedded-lisp <-- GRAVE notGrave* GRAVE

transformation-expr <-- DOLLAR '(' balancedSexp ')'
balancedSexp <- (sexp-text / '(' balancedSexp ')')*
sexp-text <- escapedParen / notParen
escapedDollarOrGrave <- BACKSLASH ( '$' / '`' / '\\')
escapedParen <- '\\' ('(' / ')' / '\\')

LCB < '{'
RCB < '}'
DOLLAR < '$'
NL < '\n'+
COLON < ':'
NUMBER <- [0-9]
BACKSLASH < '\\'
GRAVE < '`'
INDENT-MARK < '$>'
snippet-text <- (escapedDollarOrGrave / notGrave) ")

;; (peg:tree (match-pattern yasnippet *yasnippet*))
(define (parse-snippet snippet)
  (peg:tree (match-pattern yasnippet snippet)))

(define (test pairs)
  (for-each (lambda (pair)
              (let* ((snippet (car pair))
                     (good-res (cdr pair))
                     (res (parse-snippet snippet))
                     ;; (res (car res))
                     ;; (good-resres (car good-res))
                     )
                (unless (equal? res good-res)
                  (display "Test failed: with input \n")
                  (display snippet)
                  (display "\nThe output was\n")
                  (pretty-print res)
                  (display "\nInstead of\n")
                  (pretty-print good-res)
                  (display "\n"))))
            pairs)
  #f)

(test
 ;; basic
 '(("aoeu" . (yasnippet
              (snippet "aoeu")))
   ;; tab-stop
   ("aoeu$0" . (yasnippet
                (snippet "aoeu" (tab-stop (number "0")))))
   ;; tab-stop with init-value
   ("aoeu${1:value})" . (yasnippet
                         (snippet "aoeu" (tab-stop (number "1")
                                                  (init-value "value"))
                                  ")")))
   ;; escape sequences
   ("test\\`\\$\\`\\\\" . (yasnippet (snippet "test`$`\\")))
   ;; metadata
   ("# -*- mode: snippet -*-
# name: emacs_value
# key: ev
# --
emacs_value" . (yasnippet (metadata ((key "name") (value "emacs_value"))
                                    ((key "key") (value "ev")))
                          (snippet "emacs_value")))

   ;; stupid metadata
   ("# -*- mode: snippet -*-
# name: left< right>
# key: <
# --
aoeu" . (yasnippet (metadata
                    ((key "name") (value "left< right>"))
                    ((key "key") (value "<")))
                   (snippet "aoeu")))

   ;; tabStops
   ("(overlay-put ${1: ov} ${2:property} ${0:value})"
    . (yasnippet (snippet
                  "(overlay-put "
                  (tab-stop (number "1") (init-value " ov"))
                  " "
                  (tab-stop (number "2") (init-value "property"))
                  " "
                  (tab-stop (number "0") (init-value "value"))
                  ")")))
   ;; embedded-lisp
   ("(overlay-put `(string ?a ?b ?c)`)"
    . (yasnippet (snippet
                  "(overlay-put "
                  (embedded-lisp "(string ?a ?b ?c)")
                  ")")))
   ;; field transformation
   ("$1 ${1:$(capitalize yas-text)}" . (yasnippet
                                        (snippet
                                         (tab-stop (number "1"))
                                         " "
                                         (tab-stop
                                          (number "1")
                                          (transformation-expr "(capitalize yas-text)")))))
   ;; multiple transformations
   ("${1:$(make-string (string-width yas-text) ?\\=)}
${1:Title}
${1:$(make-string (string-width yas-text) ?\\=)}

$0" . (yasnippet
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
   ;; unescaped verbatim dollar in snippet text
   ("# -*- mode: snippet -*-
# name: complexity
# key: comp
# --
\\complexity{$O($0)$}
" . (yasnippet
     (metadata
      ((key "name") (value "complexity"))
      ((key "key") (value "comp")))
     (snippet
      "\\complexity{$O("
      (tab-stop (number "0"))
      ")$}\n")))
   ;; Unusual modeline
   ("# -*- mode: snippet; require-final-newline: nil -*-
# name: Header 3
# key: h3
# uuid: h3
# --
### ${1:Header 3}`(unless markdown-asymmetric-header \" ###\")`
" . (yasnippet (metadata
                ((key "name") (value "Header 3"))
                ((key "key") (value "h3"))
                ((key "uuid") (value "h3")))
               (snippet
                "### "
                (tab-stop (number "1") (init-value "Header 3"))
                (embedded-lisp
                 "(unless markdown-asymmetric-header \" ###\")")
                "\n")))
   ;; indent mark
   ("(progn
$>$0)" . (yasnippet (snippet
                     "(progn\n"
                     indent-mark
                     (tab-stop (number "0"))
                     ")")))
   ;; comment in the metadata lines - WONTFIX
   ;; ("# uuid: matrix
;; # possible improvement, compute the number of lines from the argument to array
;; # --
;; aoeu" . (yasnippet))
   ))


