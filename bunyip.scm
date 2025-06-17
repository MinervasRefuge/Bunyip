;; BSD-3-Clause : Copyright © 2025 Abigale Raeck.

;; Note: the temp path fix
;; LIBRARY_PATH=/gnu/store/...-gcc-15.1.0-lib/lib/gcc:$LIBRARY_PATH guile

(add-to-load-path ".")

(define-module (bunyip)
  #:use-module (srfi srfi-1)      ;; SRFI  1 - List library
  #:use-module (srfi srfi-9)      ;; SRFI  9 - Records
  #:use-module (srfi srfi-9 gnu)  ;; SRFI  9 - extention set-record-type-printer!
  ;;#:use-module (srfi srfi-26)   ;; SRFI 26 - specializing parameters (cute, cut)
  ;;#:use-module (srfi srfi-43)   ;; SRFI 43 - Iteration
  ;;#:use-module (srfi srfi-11)   ;; SRFI 11 - let-values
  
  ;;#:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table) ;; additional alist->... fns
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (libgccjit-bindings)
  )

(define-syntax ->
  (syntax-rules ()
    ((_ a (b . b-rst) . rst)
     (-> (b a . b-rst) . rst))
    ((_ a b . rst)
     (-> (b a) . rst))
    ((_ a) a)))

(define-syntax ->>
  (syntax-rules ()
    ((_ a (b-rst ... b-end) . rst)
     (->> (b-rst ... b-end a) . rst))
    ((_ a b . rst)
     (->> (b a) . rst))
    ((_ a) a)))
                                        ;
                                        ; gcc jit clean bindings
                                        ;

                                        ; Types

;; ** Nicked from =libgccjit.h=
;; An object created within a context.  Such objects are automatically
;;    cleaned up when the context is released.
;;
;;    The class hierarchy looks like this:
;;
;;      +- gcc_jit_object
;; 	 +- gcc_jit_location
;; 	 +- gcc_jit_type
;; 	    +- gcc_jit_struct
;; 	    +- gcc_jit_function_type
;; 	    +- gcc_jit_vector_type
;; 	 +- gcc_jit_field
;; 	 +- gcc_jit_function
;; 	 +- gcc_jit_block
;; 	 +- gcc_jit_rvalue
;; 	     +- gcc_jit_lvalue
;; 		 +- gcc_jit_param
;; 	 +- gcc_jit_case
;; 	 +- gcc_jit_extended_asm

(define-syntax define-ptr-record
  (λ (stx)
    (syntax-case stx ()
      ((_ xname)
       (let ((name (syntax->datum #'xname)))
         (with-syntax ((xrec     (datum->syntax #'xname (symbol-append '< name '>)))
                       (xmake    (datum->syntax #'xname (symbol-append 'make-  name)))
                       (xpred?   (datum->syntax #'xname (symbol-append name '?)))
                       (xptr-ref (datum->syntax #'xname (symbol-append name '-ptr))))
           #'(define-record-type xrec (xmake ptr) xpred? (ptr xptr-ref))))))))

(define-ptr-record jit-context)
(define-ptr-record jit-result)

(define-ptr-record jit-object)
(define-ptr-record jit-location)
(define-ptr-record jit-field)
(define-ptr-record jit-function)
(define-ptr-record jit-block)
(define-ptr-record jit-case)
(define-ptr-record jit-extended-asm)

(define-ptr-record jit-type)
(define-ptr-record jit-struct) ;; or jit-struct-type?
(define-ptr-record jit-function-type)
(define-ptr-record jit-vector-type)

(define-ptr-record jit-rvalue)
(define-ptr-record jit-lvalue)
(define-ptr-record jit-parameter)

                                        ; printers
(define-syntax define-printer-for-like-jit-object
  (λ (stx)
    (syntax-case stx ()
      ((_ (xname xupcast-raw) ...)
       #'(begin (define-printer-for-like-jit-object xname xupcast-raw)
                ...))
      ((_ xname xupcast-raw)
       (let ((name (syntax->datum #'xname)))
         (with-syntax ((xrec     (datum->syntax #'xname (symbol-append '< name '>)))
                       (xptr-ref (datum->syntax #'xname (symbol-append name '-ptr))))
           #'(set-record-type-printer!
              xrec
              (λ (record port)
                (format port "~a"
                        (-> record
                            xptr-ref
                            xupcast-raw
                            f:object-get-debug-string/const-char-*
                            pointer->string))))))))))

(set-record-type-printer! <jit-object>
 (λ (record port)
   (format port "~a"
           (-> record
               jit-object-ptr
               f:object-get-debug-string/const-char-*
               pointer->string))))

(define-printer-for-like-jit-object
  (jit-location     f:location-as-object/gcc-jit-object-*)
  (jit-field        f:field-as-object/gcc-jit-object-*)
  (jit-function     f:function-as-object/gcc-jit-object-*)
  (jit-block        f:block-as-object/gcc-jit-object-*)
  (jit-case         f:case-as-object/gcc-jit-object-*)
  (jit-extended-asm f:extended-asm-as-object/gcc-jit-object-*)

  (jit-type f:type-as-object/gcc-jit-object-*)

  (jit-rvalue    f:rvalue-as-object/gcc-jit-object-*)
  (jit-lvalue    f:lvalue-as-object/gcc-jit-object-*)
  (jit-parameter f:param-as-object/gcc-jit-object-*)
  
  ;; These don't have direct to `->object` implementations, is there a reason?
  (jit-struct (-> f:struct-as-type/gcc-jit-type-* f:type-as-object/gcc-jit-object-*))
  ;; jit-function-type & jit-vector-type has no converters
)

                                        ; Predicates
(define-inlinable (f-or-jit-location? a)
  (or (and (boolean? a) (not a))
      (jit-location? a)))

(define-inlinable (f-or-string? a)
  (or (and (boolean? a) (not a))
      (string? a)))

(define-inlinable (list-of-jit-parameters? a)
  (and (list? a)
       (every jit-parameter? a)))

(define-inlinable (list-of-jit-rvalues? a)
  (and (list? a)
       (every jit-rvalue? a)))

                                        ; To Conversions
(define-syntax define-to-conversion
  (λ (stx)
    (syntax-case stx ()
      ((_ (xfrom xto xconv) ...)
       #'(begin (define-to-conversion xfrom xto xconv)
                ...))
      ((_ xfrom xto xconv)
       (let ((from (syntax->datum #'xfrom))
             (to   (syntax->datum #'xto)))
         (with-syntax ((xconvfn  (datum->syntax #'xconv (symbol-append from '-> to)))
                       (xptr-ref (datum->syntax #'xfrom (symbol-append from '-ptr)))
                       (xtomake  (datum->syntax #'xto (symbol-append 'make- to))))
           #'(define (xconvfn xfrom)
               (-> xfrom xptr-ref xconv xtomake))))))))

(define-to-conversion
  (jit-location     jit-object f:location-as-object/gcc-jit-object-*)
  (jit-field        jit-object f:field-as-object/gcc-jit-object-*)
  (jit-function     jit-object f:function-as-object/gcc-jit-object-*)
  (jit-case         jit-object f:case-as-object/gcc-jit-object-*)
  (jit-extended-asm jit-object f:extended-asm-as-object/gcc-jit-object-*)
  
  (jit-parameter jit-lvalue f:param-as-lvalue/gcc-jit-lvalue-*)
  (jit-parameter jit-rvalue f:param-as-rvalue/gcc-jit-rvalue-*)
  (jit-parameter jit-object f:param-as-object/gcc-jit-object-*)
  (jit-lvalue jit-rvalue f:lvalue-as-rvalue/gcc-jit-rvalue-*)
  (jit-lvalue jit-object f:lvalue-as-object/gcc-jit-object-*)
  (jit-rvalue jit-object f:rvalue-as-object/gcc-jit-object-*))
;; These don't have direct to `->object` implementations
;; jit-struct, jit-function-type & jit-vector-type

(define-syntax with-return-pointer-guard
  (λ (stx)
    (syntax-case stx ()
      ((_ xreturn-type (xdef xparams . xbody))
       (let ((return-type (symbol->string (syntax->datum #'xreturn-type))))
         (with-syntax ((xmake (->> (substring return-type 1 (1- (string-length return-type)))
                                   (string-append "make-")
                                   string->symbol
                                   (datum->syntax #'xreturn-type))))
           #'(xdef xparams
               (let ((out (begin . xbody)))
                 (if (null-pointer? out)
                     #f
                     (xmake out))))))))))

;; Note: Probably no need to guard records as the type gets check on the *-ptr call
(define-syntax-rule (guards (guard? var) ...)
  (begin (unless (guard? var) (throw 'wrong-type-arg 'guard? var))
         ...))

(define (jit-object->debug-string obj)
  (-> obj
      jit-object-ptr
      f:object-get-debug-string/const-char-*
      pointer->string
      string-copy))

(with-return-pointer-guard
 <jit-type>
 (define* (symbol->jit-type sym #:optional (context (current-context)))
   (guards (enum:types? sym))
   
   (f:context-get-type/gcc-jit-type-*
    (jit-context-ptr context)
    (hashq-ref enum:types/sym->int sym))))

                                        ; Utils
(define-inlinable (jit-location-ptr/nullable location)
  (if location
      (jit-location-ptr location)
      %null-pointer))

(define (pointer-list->bytevector lst)
  (define ptr-size (sizeof '*))
  (define out (make-bytevector (* ptr-size (length lst)) 0))
  (for-each
   (λ (idx ptr) (bytevector-uint-set!
                 out
                 (* idx ptr-size)
                 (pointer-address ptr)
                 (native-endianness)
                 ptr-size))
   (iota (length lst))
   lst)
  out)

                                        ; Common Functions
(define current-context (make-parameter #f))

(define (context-acquire)
  (make-jit-context (f:context-acquire/gcc-jit-context-*)))

(define (context-release context)
  (f:context-release/void (jit-context-ptr context)))

(with-return-pointer-guard
 <jit-parameter>
 (define* (new-parameter type name #:optional (context (current-context)) #:key (location #f))
   (guards
    (string? name)
    (f-or-jit-location? location))
   
   (f:context-new-param/gcc-jit-param-*
    (jit-context-ptr context)
    (jit-location-ptr/nullable location)    
    (jit-type-ptr type)
    (string->pointer name))))

(with-return-pointer-guard
 <jit-function>
 (define* (new-function name kind return-type parameter-list #:optional (context (current-context)) #:key (location #f) (variadic? #f) )
   (guards
    (string? name)
    (enum:function-kind? kind)
    (f-or-jit-location? location)
    (list-of-jit-parameters? parameter-list))
   
   (f:context-new-function/gcc-jit-function-*
    (jit-context-ptr context)
    (jit-location-ptr/nullable location)
    (hashq-ref enum:function-kind/sym->int kind)
    (jit-type-ptr return-type)
    (string->pointer name)
    (length parameter-list)
    (bytevector->pointer (pointer-list->bytevector (map jit-parameter-ptr parameter-list)))
    (if variadic? 1 0))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (string->string-literal str #:optional (context (current-context)))
   (f:context-new-string-literal/gcc-jit-rvalue-*
    (jit-context-ptr context)
    (string->pointer str))))

(with-return-pointer-guard
 <jit-block>
 (define* (new-block function #:optional (name #f))
   (guards (f-or-string? name))
   
   (f:function-new-block/gcc-jit-block-*
    (jit-function-ptr function)
    (if name
        (string->pointer name)
        %null-pointer))))

(define* (add-eval block rvalue #:optional (location #f))
  (guards (f-or-jit-location? location))
  
  (f:block-add-eval/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)
   (jit-rvalue-ptr rvalue)))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (new-call function rvalue-list #:optional (context (current-context)) #:key (location #f))
   (guards (list-of-jit-rvalues? rvalue-list)
           (f-or-jit-location? location))
   
   (f:context-new-call/gcc-jit-rvalue-*
    (jit-context-ptr context)
    (jit-location-ptr/nullable location)
    (jit-function-ptr function)
    (length rvalue-list)
    (bytevector->pointer (pointer-list->bytevector (map jit-rvalue-ptr rvalue-list))))))

(define* (block-end/void block #:optional #:key (location #f))
  (guards (f-or-jit-location? location))

  (f:block-end-with-void-return/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)))

;; f:block-end-with-jump/void
;; f:block-end-with-return/void
;; f:block-end-with-switch/void
;; f:block-end-with-conditional/void
;; f:block-end-with-extended-asm-goto/gcc-jit-extended-asm-*

(with-return-pointer-guard
 <jit-result>
 (define* (compile-jit #:optional (context (current-context)))
   (f:context-compile/gcc-jit-result-*
    (jit-context-ptr context))))

(define (jit-result->get-code-ptr result name)
  (guards (string? name))

  (f:result-get-code/void-*
   (jit-result-ptr result)
   (string->pointer name)))

(define (jit-result-release result)
  (f:result-release/void (jit-result-ptr result)))


(define (example-1)
  (define type:const-char* (symbol->jit-type 'const-char-ptr))
  (define type:void        (symbol->jit-type 'void))
  (define type:int         (symbol->jit-type 'int))
  
  (define function:printf
    (new-function "printf" 'imported type:int (list (new-parameter type:const-char* "format")) #:variadic? #t))

  (define function:greet
    (let* ((param-name  (new-parameter type:const-char* "name"))
           (greet-def   (new-function "greet" 'exported type:void (list param-name)))
           (str-literal (string->string-literal "hello %s\n"))
           (rvalue-name (jit-parameter->jit-rvalue param-name))
           (block:1     (new-block greet-def)))
      (add-eval block:1 (new-call function:printf (list str-literal rvalue-name)))
      (block-end/void block:1)))

  (compile-jit))


(define (run-example-1)
  (parameterize ((current-context (context-acquire)))
    (let* ((result (example-1))
           (gfn (jit-result->get-code-ptr result "greet")))
      ((pointer->procedure void gfn (list '*)) (string->pointer "Abby!"))
      (jit-result-release result))
    
    (context-release (current-context))))
