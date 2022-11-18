(use-modules (ice-9 peg)
             (ice-9 textual-ports)
             (ice-9 pretty-print))

(define *yasnippet*
  (let* ((port (open-input-file "/home/ym/.config/doom/snippets/emacs-lisp-mode/overlay-put"))
         (snippet (get-string-all port)))
    (close-port port)
    snippet))


(define-peg-string-patterns
  "yasnippet <-- preamble? snippet !.
preamble <- (modeline metadata? metadataEnd) / (metadata? metadataEnd)
modeline < '# -*- mode: snippet -*-' NL
metadata <-- metadataLine+
metadataLine <- mtStart key mtSep value NL
key <-- ([a-zA-Z_] / '-')+
value <-- ([a-zA-Z_] / '-')+
metadataEnd < '# --' NL
mtStart < '# '
mtSep < ': '

snippet <-- (tabStop / embeddedLisp / snippetText)*

tabStop <-- DOLLAR (number / (LCB (number COLON)? (transformationExpr / initValue) RCB))
number <-- NUMBER
initValue <-- (notRCB / embeddedLisp)*

embeddedLisp <-- GRAVE notGrave* GRAVE

transformationExpr <-- DOLLAR '(' balancedSexp ')'
balancedSexp <- sexpText / '(' balancedSexp ')'
sexpText <- escapedParen / notParen
notParen <- [x20-27] / [x2a-10FFFF]
escapedParen <- '\\\\' ('(' / ')')

LCB < '{'
RCB < '}'
DOLLAR < '$'
NL < '\n'+
COLON < ':'
NUMBER <- [0-9]
BACKSLASH < '\\'
GRAVE < '`'
snippetText <- (escapedDollarOrGrave / notDollarOrGrave)*
notGrave <- [x20-x5F] / [x61-x10FFFF] / '\t' / '\n' / ' '
notDollarOrGrave <- [x20-x23] / [x25-x5F] / [x61-x10FFFF] / '\t' / '\n' / ' '
escapedDollarOrGrave <- BACKSLASH ( DOLLAR / GRAVE)
notRCB <- [x20-x7C] / [x7E-x10FFFF] / '\t' / '\n' / ' '
")

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
 '(("aoeu" . (yasnippet
              (snippet "aoeu")))
   ("aoeu$0" . (yasnippet
                (snippet "aoeu" (tabStop (number "0")))))
   ("# -*- mode: snippet -*-
# name: emacs_value
# key: ev
# --
emacs_value" . (yasnippet (metadata ((key "name") (value "emacs_value"))
                                    ((key "key") (value "ev")))
                          (snippet "emacs_value")))

   ;; ("# -*- mode: snippet -*-
   ;; # name: overlay-put
   ;; # key: ovp
   ;; # --
   ;; (overlay-put ${1: ov} ${2:property} ${0:value})
   ;; " . (yasnippet (metadata (name "overlay-put")
   ;;                          (key "ovp"))
   ;;                (snippet "(overlay-put "
   ;;                         (tabStop (number "1")
   ;;                                  (initValue " ov"))
   ;;                         " "
   ;;                         (tabStop (number "2")
   ;;                                  (initValue "property"))
   ;;                         " "
   ;;                         (tabStop (number "0")
   ;;                                  (initValue "value"))
   ;;                         ")")))
   ))
;; (match-pattern
;;  yasnippet
;;  "# key: value
;; # key: value
;; # --
;; aoeu
;; ")
;; (define-peg-string-patterns
;;   "testing < [x00-x200] / ' ' / '\t'")
;; (match-pattern testing "\n")
