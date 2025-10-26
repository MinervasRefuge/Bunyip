;; bunyip.scm : Tools to work with clang ast
;; Copyright (C) 2025  Abigale Raeck
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (bunyip)
  #:use-module (srfi srfi-1)   ;; SRFI   1 - List library
  ;;#:use-module (srfi srfi-9)   ;; SRFI   9 - Records
  #:use-module (srfi srfi-26)  ;; SRFI  26 - Specializing Parameters (cute, cut)
  #:use-module (srfi srfi-43)  ;; SRFI  43 - Iteration
  ;;#:use-module (srfi srfi-171) ;; SRFI 171 - Transducers (used for prefix finder)
  #:use-module (ice-9 format)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:export (call-clang-for-ast-dump
            pretty-format-ast
            pretty-print-ast
            find-toplevel-matching-asts
            filter-inner-matching-asts
            ;;map-filter-matching-asts

            enum-ast->simple-alist

    
            ast-function->simple-alist
            

            ;;find-longest-prefix
            find-longest-prefix/list
            ;;find-longest-prefix/list*
            
            ;;ast-predicate/simple
            ))

(define-syntax-rule (push! place value)
  (set! place (cons value place)))

(define-syntax-rule (matchλ a ...)
  (match-lambda a ...))



;; id           : str (of a hex 0x--- number),
;; kind         : str,
;; ?name        : str
;; ?valCategory : ?,
;; ?isImplicit  : ?,
;; ?size        : ?,
;; ?loc         : {offset:int, ?line:int, col:int, tokLen:int},
;; inner        : [@This()],
;; ?decl        : {id kind name},
;; ?type        : ?{ ?qualType: str },
;; ?range       : {begin: #loc, end: #loc}
;; ?text        : str
;; ?storageClass: str
;; ?tagUsed     : str

(define (hex-string->integer str)
  (string->number (substring str 2) 16))
  
(define (clean-ast ast)
  "Converts keys into symbols and converts known fields to the respective types"
  (define transform
    (matchλ
     (("id" . id)
      (cons 'id (hex-string->integer id)))
     (((and key (or "kind" "tagUsed" "storageClass")) . val)
      (cons (string->symbol key) (string->symbol val)))
     ((a . b)
      (cons (string->symbol a) (clean-ast b)))))

  (cond
   ((list? ast) (map transform ast))
   ((vector? ast) (vector-map (λ (i elt) (clean-ast elt)) ast))
   (else ast)))

(define* (call-clang-for-ast-dump header-path #:optional (clang "clang"))
  (let* ((in+out (pipe))
         (pid (spawn clang `(,clang "-Xclang" "-ast-dump=json" ,header-path)
                     #:output (cdr in+out))))
    (close-port (cdr in+out))
    (let ((scm (json->scm (car in+out))))
      (close-port (car in+out))
      (waitpid pid)
      (clean-ast scm))))



(define* (string-indent str indent-size #:optional (chr #\space))
  (let ((inchr (make-string indent-size chr)))
    (string-append
     inchr
     (string-join 
      (string-split str #\newline)
      (string-append "\n" inchr)))))

(define (pretty-format-ast ast)
  (define (pre)
    (format #f "<~s~@[ name:~s~]~@[ value:~s~] inner:["
            (assq-ref ast 'kind)
            (assq-ref ast 'name)
            (assq-ref ast 'value)))
  
  (define (post)
    "]>")

  (define (sub inner-ast)
    (string-indent
     (format #f "~%~a" (pretty-format-ast inner-ast))
     2))

  (define str-inner
    (let ((ast-inner (assq-ref ast 'inner)))
      (if (and ast-inner (> (vector-length ast-inner) 0))
          (string-append
           (apply string-append (vector->list (vector-map (λ (_ elt) (sub elt)) ast-inner)))
           "\n")
          "")))
  
  (apply string-append `(,(pre) ,str-inner ,(post))))

(define* (pretty-print-ast ast #:optional (port #t))
  (format port "~a~%" (pretty-format-ast ast)))



(define (find-toplevel-matching-asts provided-ast ast-kind)
  (define roots (list))
  (define ast-match
    (matchλ
      ((and ast (= (cut assq-ref <> 'kind)
                   (? (cut eq? <> ast-kind) _)))
       (push! roots ast))
      ((and (= (cut assq-ref <> 'inner) (? vector? inner)))
       (vector-for-each (λ (_ elt) (ast-match elt)) inner))
      (_ #f)))
  (ast-match provided-ast)
  (reverse roots))

(define (filter-inner-matching-asts provided-ast ast-filter)
  (define roots (list))
  (define ast-match
    (matchλ
      ((? ast-filter ast)
       (push! roots ast))
      ((and (= (cut assq-ref <> 'inner) (? vector? inner)))
       (vector-for-each (λ (_ elt) (ast-match elt)) inner))
      (_ #f)))
  (ast-match provided-ast)
  (reverse roots))



(define (general-cleaner str)
  (string-map
   (λ (c) (if (eq? c #\_) #\- c))
   (string-trim-both (string-downcase str) #\space)))

;; (define (prettify-c-type str)
;;   (string-map
;;    (λ (c) (cond ((eq? c #\_) #\-)
;;                 ((eq? c #\space) #\-)
;;                 (else c)))
;;    (string-trim-both (string-downcase str) #\space)))



(define* (find-longest-prefix a b #:optional (equality? char-ci=?))
  (let lp ((la (string->list a))
           (lb (string->list b))
           (idx 0))
    (cond
     ((or (null? la) (null? lb))
      (substring a 0 idx))
     ((equality? (car la) (car lb))
      (lp (cdr la) (cdr lb) (1+ idx)))
     (else (substring a 0 idx)))))

(define* (find-longest-prefix/list lst)
  (reduce find-longest-prefix "" lst))

;; (define (find-longest-prefix/list* lsts)
;;   (let lp ((ll (map string->list lsts))
;;            (idx 0))
;;     (cond
;;      ((any null? ll)
;;       (substring (car lsts) 0 idx))
;;      ((= 1 (list-transduce
;;             (compose (tmap car) (tdelete-duplicates eqv?))
;;             rcount
;;             ll))
;;       (lp (map cdr ll) (1+ idx)))
;;      (else (substring (car lsts) 0 idx)))))


                                        ; Enum Declares

;; * EnumDecl > EnumConstantDecl ?> ConstantExpr[value]

(define (enum-filter-rule-member ast) 
  (eq? (assq-ref ast 'kind) 'EnumConstantDecl)) ;; EnumNonConst Decl?

(define (enum-filter-rule-value ast)
  (eq? (assq-ref ast 'kind) 'ConstantExpr))

(define (enum-ast->simple-alist ast)
  (define fields 
    (map (λ (z)
           (let ((enum-vals (filter-inner-matching-asts z enum-filter-rule-value)))
             `((name . ,(assq-ref z 'name))
               (value . ,(if (nil? enum-vals)
                             #f
                             (assq-ref (car enum-vals) 'value))))))
         (filter-inner-matching-asts ast enum-filter-rule-member)))

  `((name . ,(assq-ref ast 'name))
    (fields . ,fields)))


                                        ; Function Declares

;; * FunctionDecl[name, type, storageClass] > ParmVarDecl[name, type]
;; type.qualType
;; FullComment > (TextComment, ParagraphComment) 

(define (ast-function->simple-alist ast)
  (match ast
    ((and (= (cut assq-ref <> 'name) (? string? name))
          (= (cut assq-ref <> 'mangledName) (? string? mangled-name))
          (= (cut assq-ref <> 'type)
             (? list? (= (cut assq-ref <> 'qualType)
                         (? string? qual-type))))
          (= (cut assq-ref <> 'inner) inner))
     (let ((params (list)))
       (when inner
         (vector-for-each
          (λ (_ node)
            (case (assq-ref node 'kind)
              ((ParmVarDecl)
               (match node
                 ((and (= (cut assq-ref <> 'name) name) ;; doesn't always have a name :p
                       (= (cut assq-ref <> 'type)
                          (? list? (= (cut assq-ref <> 'qualType) type))))
                  (push! params (cons (if name name (symbol->string (gensym))) type)))))
              ((FullComment ParagraphComment TextComment VisibilityAttr NoThrowAttr NonNullAttr RestrictAttr ColdAttr BuiltinAttr AsmLabelAttr FormatAttr)
               (noop))
              (else => (error 'unknown-ast-node-type node))))
          inner))         
       `((name . ,name)
         (mangled-name . ,mangled-name)
         (qual-type . ,qual-type)
         (args . ,(reverse params)))))
    (_ #f)))

(define string-c-type->guile-ffi-type
  (matchλ
    ((? (cut string-suffix? "*" <>) _) ''*)
    ((? (cut string-suffix? "*restrict" <>) _) ''*)
    ;;((? (cut string-prefix? "enum" <>) _) 'int)
    (str (match (filter (λ (elm) (> (string-length elm) 0))
                        (string-split str #\space))
           (("int8_t")    'int8)
           (("int16_t")   'int16)
           (("int32_t")   'int32)
           (("int64_t")   'int64)
           (("uint8_t")   'uint8)
           (("uint16_t")  'uint16)
           (("uint32_t")  'uint32)
           (("uint64_t")  'uint64)
           (("float")     'float)
           (("double")    'double)
           ;; complex-double
           ;; complex-float
           (("void")      'void)
           (("int")       'int)
           (("long")      'long)
           (("short")     'short)
           (("size_t")    'size_t)
           (("ssize_t")   'ssize_t)
           (("ptrdiff_t") 'ptrdiff_t)
           (("intptr_t")  'intptr_t)
           (("uintptr_t") 'uintptr_t)
           (("unsigned" "int")   'unsigned-int)
           (("unsigned" "long")  'unsigned-long)
           (("unsigned" "short") 'unsigned-short)
           (_ #f)))))

;; (define (string-full-fn-c-type->guile-ffi-return-type str)
;;   (string-c-type->guile-ffi-type (string-trim-both (car (string-split str #\()) #\space)))

;; (define (string-full-fn-c-type->return-type str)
;;   (string-trim-both (car (string-split str #\()) #\space))

;;   ;; foreign-library-function lib name [#:return-type=void] [#:arg-types='()] [#:return-errno?=#f]
;;   ;; args from name onwards

;; (define (ast-function-simple->partial-ffi-define afts)
;;   (list (ast-function-simple-mangled-name afts)
;;         #:return-type
;;         (string-full-fn-c-type->guile-ffi-return-type (ast-function-simple-full-type afts))
;;         #:arg-types
;;         (cons 'list
;;               (map (match-lambda ((a . b) (string-c-type->guile-ffi-type b)))
;;                    (ast-function-simple-params afts)))))



;; TypedefDecl

;; Scan for unknown types. Filter out manually created types.
;; goops lookup for equiv types
;; define fn with define-method

;; (("gcc_jit_context" "*") <jit-context> jit-context->pointer)
;; (("gcc_jit_struct" "*") <jit-struct> jit-obj->pointer)
;; (("const" "char" "*") <string> string->pointer)
;; (("int") <integer> #f)
