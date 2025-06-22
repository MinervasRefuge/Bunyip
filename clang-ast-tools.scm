;; clang-ast-tools.scm : Syntax-rules and methods to work with clang ast
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

(define-module (clang-ast-tools)
  #:use-module (srfi srfi-1)   ;; SRFI   1 - List library
  #:use-module (srfi srfi-9)   ;; SRFI   9 - Records
  #:use-module (srfi srfi-26)  ;; SRFI  26 - Specializing Parameters (cute, cut)
  #:use-module (srfi srfi-43)  ;; SRFI  43 - Iteration
  #:use-module (srfi srfi-171) ;; SRFI 171 - Transducers (used for prefix finder)
  #:use-module (ice-9 format)
  ;#:use-module (ice-9 hash-table) ;; additional alist->... fns
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (system syntax) ;; syntax-local-binding
  #:export (call-clang-for-ast-dump
            pretty-format-ast
            pretty-print-ast
            find-toplevel-matching-asts
            filter-matching-asts
            map-filter-matching-asts

            find-longest-prefix
            find-longest-prefix/list
            find-longest-prefix/list*
            
            ast-predicate/simple)
  #:export-syntax (define-enum
                    ast-enum->define-enum
                    ast-function->define-ffi-function
                    ast-function->define-function)
  )

(define-syntax-rule (pushp place value)
  (set! place (cons value place)))

(define (resolve-syntax-value stx)
  ;; There must be a better way then this...
  (call-with-values (λ () (syntax-local-binding stx)) ;; 'global '(symbol module-sym)
    (λ (binding-type value) ;;(pk binding-type value)
       (module-ref (resolve-module (cdr value))
                   (car value)))))



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
  (map (match-lambda
         (("id" . id) (cons 'id (hex-string->integer id)))
         (((and key (or "kind" "tagUsed" "storageClass")) . val)
          (cons (string->symbol key) (string->symbol val)))
         ((key . (? list? lst))
          (cons (string->symbol key) (clean-ast lst)))
         ((key . (? vector? vec))
          (cons (string->symbol key) (vector-map (λ (i elt) (clean-ast elt)) vec)))
         ((a . b) (cons (string->symbol a) b)))
       ast))

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
    (format #f "<ast ~s~@[ name:~s~]~@[ value:~s~] inner:["
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
    (match-lambda
      ((and ast (= (cut assq-ref <> 'kind)
                   (? (cut eq? <> ast-kind) _)))
       (pushp roots ast))
      ((and (= (cut assq-ref <> 'inner) (? vector? inner)))
       (vector-for-each (λ (_ elt) (ast-match elt)) inner))
      (_ #f)))
  (ast-match provided-ast)
  roots)

(define* (ast-predicate/simple key value #:optional (equality? eq?))
  (match-lambda
    ((= (cut assq-ref <> key)
        (? (cut equality? <> value) _))
     #t)
    (_ #f)))

(define (filter-matching-asts provided-ast ast-filter)
  (define roots (list))
  (define ast-match
    (match-lambda
      ((? ast-filter ast)
       (pushp roots ast))
      ((and (= (cut assq-ref <> 'inner) (? vector? inner)))
       (vector-for-each (λ (_ elt) (ast-match elt)) inner))
      (_ #f)))
  (ast-match provided-ast)
  roots)

(define (map-filter-matching-asts provided-ast ast-map-filter)
  (define roots (list))
  (define ast-match
    (match-lambda
      ((= ast-map-filter (? (match-lambda (#f #f) (_ #t)) ast))
       (pushp roots ast))
      ((and (= (cut assq-ref <> 'inner) (? vector? inner)))
       (vector-for-each (λ (_ elt) (ast-match elt)) inner))
      (_ #f)))
  (ast-match provided-ast)
  roots)



(define (general-cleaner str)
  (string-map
   (λ (c) (if (eq? c #\_) #\- c))
   (string-trim-both (string-downcase str) #\space)))

(define (prettify-c-type str)
  (string-map
   (λ (c) (cond ((eq? c #\_) #\-)
                ((eq? c #\space) #\-)
                (else c)))
   (string-trim-both (string-downcase str) #\space)))

;; (define-syntax cleaners
;;   (syntax-rules ()
;;     ((_ (sym bdy) . body)
;;      )))

(define (cleaner-ref cleaners key)
  (or (assq-ref cleaners key)
      (assq-ref %cleaners% key)))

(define %cleaners%
  `((fn-name          . ,general-cleaner)   ;; (λ (name) name)                 <string> => <string>
    (fn-arg           . ,general-cleaner)   ;; (λ (arg-name) arg-name)         <string> => <string>
    (enum-pre-members .                     ;; (λ (name member-name-list) ...) <string> <list-of-strings> => void
     ,(λ (n ml) (noop)))   
    (enum-name        . ,general-cleaner)   ;; (λ (name) name)                 <string> => <string>
    (enum-member      . ,general-cleaner)   ;; (λ (member-name) member-name)   <string> => <string>
    (enum-prefix . "enum:")
    (enum-member-filter . ,(λ (name) #t))
    (pretty-c-type    . ,prettify-c-type)   ;; (λ (ctype-str) ctype-str) <string> => <string>
    (ffi-prefix . "ffi:")
    (fn-prefix . "f:")
    ))

;; Order of ops.
;; - filter
;; - pre
;; - member
;; - prefix + ^


                                        ; Enum Declares

;; * EnumDecl > EnumConstantDecl ?> ConstantExpr[value]

(define (filter-enum-inner-ast ast) 
  (eq? (assq-ref ast 'kind) 'EnumConstantDecl)) ;; EnumNonConst Decl?

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

(define (find-longest-prefix/list* lsts)
  (let lp ((ll (map string->list lsts))
           (idx 0))
    (cond
     ((any null? ll)
      (substring (car lsts) 0 idx))
     ((= 1 (list-transduce
            (compose (tmap car) (tdelete-duplicates eqv?))
            rcount
            ll))
      (lp (map cdr ll) (1+ idx)))
     (else (substring (car lsts) 0 idx)))))

(define (ast-enum-member->name ast)
  (assq-ref ast 'name))

(define-syntax define-enum
  (λ (stx)
    (syntax-case stx ()
      ((_ xname (sym . int) ...)
       (let* ((name (symbol->string (syntax->datum #'xname)))
              (name/alist    (string-append name "/alist"))
              (name/sym->int (string-append name "/sym->int"))
              (name/int->sym (string-append name "/int->sym"))
              (predicate? (string-append name "?")))
         (with-syntax ((xname/alist    (datum->syntax #'xname (string->symbol name/alist) ))
                       (xname/sym->int (datum->syntax #'xname (string->symbol name/sym->int)))
                       (xname/int->sym (datum->syntax #'xname (string->symbol name/int->sym)))
                       (xpredicate? (datum->syntax #'xname (string->symbol predicate?)))
                       
                       ;; Would like the syntax not to reference this module for export reasons.
                       (xmap (datum->syntax #'xname 'map))
                       (xcons (datum->syntax #'xname 'cons))
                       (xcar (datum->syntax #'xname 'car))
                       (xcdr (datum->syntax #'xname 'cdr))
                       (xalist->hashq-table (datum->syntax #'xname 'alist->hashq-table))
                       (xsymbol? (datum->syntax #'xname 'symbol?))
                       (xnot (datum->syntax #'xname 'not))
                       (xboolean? (datum->syntax #'xname 'boolean?))
                       (xhashq-ref (datum->syntax #'xname 'hashq-ref)))
           #'(begin
               (define xname/alist '((sym . int) ...))
               (define xname/sym->int (xalist->hashq-table xname/alist))
               (define xname/int->sym (xalist->hashq-table (xmap (λ (c) (xcons (xcdr c) (xcar c))) xname/alist)))
               (define-inlinable (xpredicate? a)
                 (and (xsymbol? a)
                      (xnot (xboolean? (xhashq-ref xname/sym->int a))))))))))))

;; (define-syntax ast-enum->define-enum
;;   (λ (stx)
;;     (syntax-case stx ()
;;       ((_ xcleaners xast)
;;        (let* ((ast-enum (syntax->datum #'xast))
;;               (cleaners (syntax->datum #'xcleaners))
;;               (name (assq-ref ast-enum 'name))
;;               (inner (assq-ref ast-enum 'inner))
;;               (inner-len (vector-length inner))
;;               (filtered-inner (filter filter-enum-inner-ast (vector->list inner)))
;;               (inner-names (map (cut assq-ref <> 'name) filtered-inner)))

;;          ((cleaner-ref cleaners 'enum-pre-members) inner-names)
         
;;          (let* ((found-prefix (find-longest-prefix/list (cons name inner-names)))
;;                 (enum-cleaner (cleaner (string-upcase (if (string-suffix? "_" found-prefix)
;;                                                           found-prefix
;;                                                           (string-append found-prefix "_")))))
;;                 (enum-members
;;                  (map (λ (node idx)
;;                         (cons (string->symbol ((cleaner-ref cleaners 'enum-members)
;;                                                (assq-ref node 'name)))
;;                               idx))
;;                       filtered-inner
;;                       (iota inner-len)))))
;;          (xenum-members (map (cut datum->syntax #'xast <>) enum-members))
;;          (with-syntax ((xname (datum->syntax #'xast (string->symbol ((cleaner) name)))))
;;            #`(define-enum
;;                xname
;;                #,@xenum-members)))))))

(define-syntax ast-enum->define-enum
  (λ (stx)
    (syntax-case stx ()
      ((_ xcleaners xast)
       (let* ((ast-enum (syntax->datum #'xast))
              
              (cleaners (resolve-syntax-value #'xcleaners))
              (eprefix (cleaner-ref cleaners 'enum-prefix))
              (name-cleaner (cleaner-ref cleaners 'enum-name))
              (member-cleaner (cleaner-ref cleaners 'enum-member))
              (member-filter (cleaner-ref cleaners 'enum-member-filter))
              
              (name (assq-ref ast-enum 'name))
              (inner (assq-ref ast-enum 'inner))
              (filtered-inner (filter (λ (node) (and (filter-enum-inner-ast node)
                                                     (member-filter (assq-ref node 'name))))
                                      (vector->list inner)))
              (inner-names (map (cut assq-ref <> 'name) filtered-inner)))

         ((cleaner-ref cleaners 'enum-pre-members) name inner-names)
         
         (let* ((enum-members
                 (map (λ (name idx)
                        (cons (string->symbol (member-cleaner name))
                              idx))
                      inner-names
                      (iota (length inner-names))))
                (xenum-members (map (cut datum->syntax #'xast <>) enum-members)))
           (with-syntax ((xname (datum->syntax #'xast (string->symbol (string-append eprefix (name-cleaner name))))))
             #`(define-enum
                 xname
                 #,@xenum-members))))))))



                                        ; Function Declares

;; * FunctionDecl[name, type, storageClass] > ParmVarDecl[name, type]
;; type.qualType
;; FullComment > (TextComment, ParagraphComment) 

(define-record-type <ast-function-simple>
  (make-ast-function-simple name mangled-name full-type loc doc-asts params)
  ast-function-simple?
  (name         ast-function-simple-name)          ;; string
  (mangled-name ast-function-simple-mangled-name)  ;; string
  (full-type    ast-function-simple-full-type)     ;; type signature string
  (loc          ast-function-simple-loc)           ;; location ast
  (doc-asts     ast-function-simple-doc-asts)      ;; 'TextComment 'ParagraphComment 'FullComment
  (params       ast-function-simple-params)        ;; ((name . type) ...)
  ) 

(define (ast-function->simple ast)
  (match ast
    ((and (= (cut assq-ref <> 'name) (? string? name))
          (= (cut assq-ref <> 'mangledName) (? string? mangled-name))
          (= (cut assq-ref <> 'storageClass) 'extern)
          (= (cut assq-ref <> 'loc) loc)
          (= (cut assq-ref <> 'type)
             (? list? (= (cut assq-ref <> 'qualType)
                         (? string? qual-type))))
          (= (cut assq-ref <> 'inner) inner))
     (let ((docs (list))
           (params (list)))
       (when inner
         (vector-for-each
          (λ (_ node)
            (case (assq-ref node 'kind)
              ((ParmVarDecl)
               (match node
                 ((and (= (cut assq-ref <> 'name) name) ;; doesn't always have a name :p
                       (= (cut assq-ref <> 'type)
                          (? list? (= (cut assq-ref <> 'qualType) type))))
                  (pushp params (cons (if name name (symbol->string (gensym))) type))) 
                 ))
              ((FullComment ParagraphComment TextComment)
               (pushp docs node))
              ((NoThrowAttr NonNullAttr RestrictAttr ColdAttr BuiltinAttr AsmLabelAttr FormatAttr) (noop))
              (else => (error 'unknown-ast-node-type node))))
          inner))         
       (make-ast-function-simple
        name
        mangled-name
        qual-type
        loc
        (reverse docs) ;; note: docs don't capture most comment blocks
        (reverse params))))
    (_ #f)))

(define string-c-type->guile-ffi-type
  (match-lambda
    ((? (cut string-suffix? "*" <>) _) ''*)
    ((? (cut string-suffix? "*restrict" <>) _) ''*)
    ((? (cut string-prefix? "enum" <>) _) 'int)
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
           
           ;;(("const" "__sigset_t" "*restrict") ''*)
           ;;(("const" "struct" "timespec" "*restrict") ''*)
           ;;(("fd_set" "*restrict") ''*)
           ;;(("struct" "timeval" "*restrict") ''*)
           ;;(("fpos_t" "*restrict") ''*)
           ;;(("FILE" "*restrict") ''*)
           (("__off_t") 'int)
           ;;(("const" "void" "*restrict") ''*)
           ;;(("void" "*restrict") ''*)
           ;;(("const" "char" "*restrict") ''*)
           (("__ssize_t") 'ssize_t)
           ;;(("size_t" "*restrict") ''*)
           ;;(("char" "**restrict") ''*)
           (("cookie_io_functions_t") ''*)

           (err (error 'unknown-type err))))))

(define (string-full-fn-c-type->guile-ffi-return-type str)
  (string-c-type->guile-ffi-type (string-trim-both (car (string-split str #\()) #\space)))

(define (string-full-fn-c-type->return-type str)
  (string-trim-both (car (string-split str #\()) #\space))

  ;; foreign-library-function lib name [#:return-type=void] [#:arg-types='()] [#:return-errno?=#f]
  ;; args from name onwards

(define (ast-function-simple->partial-ffi-define afts)
  (list (ast-function-simple-mangled-name afts)
        #:return-type
        (string-full-fn-c-type->guile-ffi-return-type (ast-function-simple-full-type afts))
        #:arg-types
        (cons 'list
              (map (match-lambda ((a . b) (string-c-type->guile-ffi-type b)))
                   (ast-function-simple-params afts)))))

(define-syntax ast-function->define-ffi-function
  (λ (stx)
    (syntax-case stx ()
      ((_ xcleaners xlib xast)
       (let* ((ast (syntax->datum #'xast))
              (sfn (ast-function->simple ast)))
         (if sfn
             (let* ((cleaners (resolve-syntax-value #'xcleaners))
                    (fn-name ((cleaner-ref cleaners 'fn-name)
                              (ast-function-simple-name sfn)))
                    (ffi-name (string-append (cleaner-ref cleaners 'ffi-prefix)
                                             fn-name))
                    (arg-types (ast-function-simple->partial-ffi-define sfn))
                    (xarg-types (map (cut datum->syntax #'xast <>) arg-types)))
               (with-syntax ((xffi-name (datum->syntax #'xast (string->symbol ffi-name)))
                             (xforeign-library-function (datum->syntax #'xlib 'foreign-library-function)))
                 #`(define xffi-name
                     (xforeign-library-function xlib #,@xarg-types))))
             (with-syntax ((xerr (datum->syntax #'xast (assq-ref ast 'name))))
               #''(xlib xerr))))))))

(define-syntax ast-function->define-function
  (λ (stx)
    (syntax-case stx ()
      ((_ xcleaners xast)
       (let* ((ast (syntax->datum #'xast))
              (sfn (ast-function->simple ast)))
         (if sfn
             (let* ((cleaners (resolve-syntax-value #'xcleaners))
                    (fn-name ((cleaner-ref cleaners 'fn-name)
                              (ast-function-simple-name sfn)))
                    (ffi-name (string-append (cleaner-ref cleaners 'ffi-prefix)
                                             fn-name))
                    (arg-cleaner (cleaner-ref cleaners 'fn-arg))
                    (pretty-c-type (cleaner-ref cleaners 'pretty-c-type))
                    (args (map (match-lambda
                                 ((a . b)
                                  (string->symbol
                                   (string-append (arg-cleaner a) "/" (pretty-c-type b)))))
                               (ast-function-simple-params sfn)))
                    (xargs (map (cut datum->syntax #'xast <>) args)))
               (with-syntax ((xfn-name (datum->syntax #'xast
                                                      (string->symbol
                                                       (string-append
                                                        (cleaner-ref cleaners 'fn-prefix)
                                                        fn-name
                                                        "/"
                                                        (pretty-c-type
                                                         (string-full-fn-c-type->return-type
                                                          (ast-function-simple-full-type sfn)))))))
                             (xffi-name (datum->syntax #'xast (string->symbol ffi-name))))
                 #`(define (xfn-name #,@xargs)
                       (xffi-name #,@xargs))))
             #''()))))))



;; TypedefDecl

;; Scan for unknown types. Filter out manually created types.
;; goops lookup for equiv types
;; define fn with define-method

;; (("gcc_jit_context" "*") <jit-context> jit-context->pointer)
;; (("gcc_jit_struct" "*") <jit-struct> jit-obj->pointer)
;; (("const" "char" "*") <string> string->pointer)
;; (("int") <integer> #f)
