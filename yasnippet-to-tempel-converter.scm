(define-module (yasnippet-to-tempel-converter)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:export (yasnippet-string->yasnippet-tree
            yasnippet-tree->yasnippet
            yasnippet->tempel-snippet
            yasnippet-string->tempel))

;; The string pattern equivalent of these doesn't match special chars e.g ' ' and
;; '(' for some reason, which is why they are written in sexp
(define-peg-pattern notGrave         body (or (range #\x07 #\x5F) (range #\x61 #\x10FFFF)))
(define-peg-pattern notDollar        body (or (range #\x07 #\x10FFFF)))
(define-peg-pattern notRCB           body (or (range #\x07 #\x7C) (range #\x7E #\x10FFFF)))
(define-peg-pattern notParen         body (or (range #\x07 #\x27) (range #\x2a #\x10FFFF)))
(define-peg-pattern notNL            body (or (range #\x07 #\x09) (range #\x0b #\x10FFFF)))


(define-peg-string-patterns
  "yasnippet <-- preamble? snippet !.
preamble <- (modeline+ metadata? metadataEnd) / (metadata? metadataEnd) / modeline
modeline < '#'+ ' -*-' (notNL !'*-')* '-*-' NL+
metadata <-- metadataLine+
metadataLine <- mtStart key mtSep value NL+
key <-- ([a-zA-Z_] / '-')+
value <-- notNL+
metadataEnd < '# --' NL
mtStart < '# '
mtSep < ' : ' / ' :' / ': ' / ':'

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

(define (yasnippet-string->yasnippet-tree snippet)
  "Convert the string in yas format SNIPPET to a tree representation.

Parses metadata like name, uuid too.

\"print $1\" => '(yasnippet (snippet \"print \" (tab-stop (number \"1\")))
See the tests file for examples of more output."
  (peg:tree (match-pattern yasnippet snippet)))


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

(define (yasnippet-tree->yasnippet parsed)
  "Convert the yasnippet tree (output of `yasnippet-string->yasnippet-tree') to a
`<yasnippet>' object, which mostly amounts to putting the metadata into slots."
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
  "\"2\" => 'field-2"
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
    (cons (or (and (yas-key yas)
                   (string->symbol (yas-key yas)))
              (and (yas-name yas)
                   (string->symbol (yas-name yas)))
              'unspecified-key)
          (map (lambda (atom)
                 (match atom
                   ((? string?)
                    atom)               ; TODO use n and n> from tempel
                   (('tab-stop
                     ('number number)
                     more ...)

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

(define (yasnippet-string->tempel s)
  (yasnippet->tempel-snippet (yasnippet-tree->yasnippet (yasnippet-string->yasnippet-tree s))))
