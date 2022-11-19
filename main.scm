(use-modules (ice-9 peg)
             (ice-9 textual-ports)
             (ice-9 pretty-print)
             (ice-9 match)
             (srfi srfi-9)
             (srfi srfi-1))

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
mtSep < ' : ' / ': '

snippet <-- (tab-stop / indent-mark / embedded-lisp / snippet-text)*

tab-stop <-- DOLLAR (number / (LCB (number COLON)? (transformation-expr / init-value) RCB))
number <-- NUMBER
init-value <-- (embedded-lisp / notRCB)*

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

(define (test-fn fn pairs)
  (for-each (lambda (pair)
              (let* ((in (car pair))
                     (expected-res (cdr pair))
                     (res (fn in)))
                (unless (equal? res expected-res)
                  (display "Test failed: with input \n")
                  (display in)
                  (display "\nThe output was\n")
                  (pretty-print res)
                  (display "\nInstead of\n")
                  (pretty-print expected-res)
                  (display "\n"))))
            pairs)
  #f)

(test-fn
 parse-snippet
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

   ;; default value is a lisp expr
   ("${1:`(current-time-string)`}"
    . (yasnippet (snippet
                  (tab-stop (number "1")
                            (init-value
                             (embedded-lisp "(current-time-string)"))))))
   ;; has a space between contributor and colon
   ("# -*- mode: snippet -*-
# contributor : Jimmy Wu <frozenthrone88@gmail.com>
# group: meta
# name: <meta name=\"...\" content=\"...\" />
# --
<meta name=\"${1:generator}\" content=\"${2:content}\" />"
    . (yasnippet
       (metadata
        ((key "contributor") (value "Jimmy Wu <frozenthrone88@gmail.com>"))
        ((key "group") (value "meta"))
        ((key "name") (value "<meta name=\"...\" content=\"...\" />")))
       (snippet
        "<meta name=\""
        (tab-stop (number "1") (init-value "generator"))
        "\" content=\""
        (tab-stop (number "2") (init-value "content"))
        "\" />")))))

(define-record-type <yasnippet>
  (make-yasnippet name key group uuid type
                  condition binding contributor
                  body)
  yasnippet?
  (name yas-name)
  (key yas-key)
  (group yas-group)
  (uuid yas-uuid)
  (type yas-type)
  (condition yas-condition)
  (binding yas-binding)
  (contributor yas-contributor)
  (body yas-body))

(define (parsed-snippet->yasnippet parsed)
  (let ((name #f) (key #f) (group #f) (uuid #f) (type #f)
        (condition #f) (binding #f) (contributor #f)
        (body #f))
    (match parsed
      (('yasnippet ('metadata (('key keys) ('value values)) ...) . _)
       ;; parse metadata
       (map (lambda (key_ value)
              (match key_
                ("name" (set! name value))
                ("key" (set! key value))
                ("group" (set! group value))
                ("uuid" (set! uuid value))
                ("type" (set! type value))
                ("condition" (set! condition value))
                ("binding" (set! binding value))
                ("contributor" (set! contributor value))
                (_ (throw 'invalid-metadata-type-error key value))))
            keys values))
      (_ #f))
    (let* ((snippet (last parsed))
           (body (cdr snippet)))
      (make-yasnippet name key group uuid type
                      condition binding contributor
                      body))))


(define (placeholder-number->symbol n)
  "2 => 'field-2"
  (string->symbol (string-append "field-" n)))

(define (read-from-string s)
  (call-with-input-string s read))

(define (yasnippet->tempel-snippet yas)
  "Return a single sexp defining a tempel snippet with the same key and expantion
as yas.

contributor, group, uuid, type, condition are ignored"

  ;; determine which fields need names
  (let ((field-symbol-table (make-hash-table)))
    (for-each (lambda (atom)
                (match atom
                  (('tab-stop ('number number) _more ...)
                   (match (hash-ref field-symbol-table number)
                     (#f (hash-set! field-symbol-table number 'anonymous))
                     ('anonymous (hash-set! field-symbol-table number 'named))))
                  (_ #f)))
              (yas-body yas))
    ;; if the last (tab-stop) is the one you'll get to last when tabbing, map
    ;; that number to 'last in field-symbol-table
    (let ((max-number-tab-stop
           (hash-fold (lambda (k v max-k)
                        ;; 0 is the last one you jump to and therfore the max
                        (if (and (not (equal? max-k "0"))
                                 (or (string> k max-k)
                                     (equal? k "0")))
                            k
                            max-k))
                      "\x01" field-symbol-table))
          (last-tab-stop-number
           (fold (lambda (atom prev)
                   (match atom
                     (('tab-stop ('number number) _more ...)
                      number)
                     (_ prev)))
                 "\x00" (yas-body yas))))
      (when (and (equal? max-number-tab-stop last-tab-stop-number)
                 (not (eq? (hash-ref field-symbol-table max-number-tab-stop) 'named)))
        (hash-set! field-symbol-table max-number-tab-stop 'last)))

    ;; final result
    (cons (if (yas-key yas)
              (string->symbol (yas-key yas))
              'unspecified-key)
          (map (lambda (atom)
                 (match atom
                   ((? string?)
                    atom)               ; TODO use n and n> from tempel
                   (('tab-stop
                     ('number number)
                     more ...)

                    ;; TODO handle the case where number = 0 (finish snippet)
                    (match more
                      ((('init-value value))
                       `(p ,(match value
                              ((? string?)
                               value)
                              (('embedded-lisp expr)
                               (read-from-string expr)))
                         ;; emit field name only if named
                         ,@(if (eq? (hash-ref field-symbol-table number) 'named)
                               (list (placeholder-number->symbol number))
                               '())))
                      ((('transformation-expr expr))
                       (let replace-yas-text ((expr (read-from-string expr)))
                         ;; yas mirror transformations act on yas-text as the
                         ;; current field, while tempel has a var for each field.
                         ;; Replace yas-text with the corresponding field symbol.

                         ;; TODO handle the case where expr is `yas-selected-text`
                         (match expr
                           ('yas-text
                            (placeholder-number->symbol number))
                           ((? list? expr)
                            (map replace-yas-text expr))
                           (_
                            expr))))
                      ('()
                       ;; if no init-value, the field might be a mirror
                       (match (hash-ref field-symbol-table number)
                         ('named
                          `(s ,(placeholder-number->symbol number)))
                         ;; not a mirror
                         ((or #f 'anonymous)
                          'p)
                         ('last
                          'q)))))
                   (('embedded-lisp expr)
                    (let ((expr (read-from-string expr)))
                      ;; a stand-alone `%` can become 'r
                      (if (or (eq? expr 'yas-selected-text)
                              (eq? expr '%))
                          'r
                          expr)))))
               (yas-body yas)))))

(define (yas-string->tempel s)
  (yasnippet->tempel-snippet (parsed-snippet->yasnippet (parse-snippet s))))

(test-fn
 yas-string->tempel
 '(;; metadata
   ("# -*- mode: snippet -*-
# name: emacs_value
# key: ev
# --
emacs_value" . (ev "emacs_value"))
   ;; basic
   ("help" . (unspecified-key "help"))
   ;; field
   ("help$1" . (unspecified-key "help" q))
   ;; field with mirror - now it needs a name
   ("help$1 $1" . (unspecified-key "help" (s field-1) " " (s field-1)))
   ;; field with default
   ("help${1:default}" . (unspecified-key "help" (p "default")))
   ;; embedded lisp
   ("help `(current-time-string)`" . (unspecified-key "help " (current-time-string)))
   ;; field with default as embedded lisp
   ("${1:`(current-time-string)`}" . (unspecified-key (p (current-time-string))))
   ;; mirror transformation
   ("$1${1:$(capitalize yas-text)}" . (unspecified-key (s field-1) (capitalize field-1)))
   ;; region
   ("`yas-selected-text`" . (unspecified-key r))
   ("`%`" . (unspecified-key r))
   ;; unsupported use case of yas-text. this is the expected (mis)behavior
   ("`(capitalize %)`" . (unspecified-key (capitalize %)))
   ;; use 'q when appropriate
   ("if ($1) { $0 }" . (unspecified-key "if (" p ") { " q " }"))
   ("if ($1) { $3 }" . (unspecified-key "if (" p ") { " q " }"))
   ("if ($1) { ${3:default} }" . (unspecified-key "if (" p ") { " (p "default") " }"))
   ))
