;; BSD-3-Clause : Copyright © 2025 Abigale Raeck.

;; Note: the temp path fix
;; LIBRARY_PATH=/gnu/store/...-gcc-15.1.0-lib/lib/gcc:$LIBRARY_PATH guile

(add-to-load-path ".")

(define-module (bunyip)
  #:use-module (srfi srfi-1)      ;; SRFI  1 - List library
  #:use-module (srfi srfi-2)      ;; SRFI  2 - and-let*
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
  ;;#:use-module (libgccjit-bindings)
  #:use-module (libgccjit-exported)
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

(eval-when (expand load eval)
  (define (syntax? s)
    (string-prefix? "#<syntax" (format #f "~a" s)))

  (define (format-syntax/string id fmt . args)
    (datum->syntax id 
      (apply format #f fmt
             (map (λ (a) (if (syntax? a) (syntax->datum a) a))
                  args))))
  
  (define (format-syntax/symbol id fmt . args)
    (datum->syntax id
      (string->symbol
       (apply format #f fmt
              (map (λ (a) (if (syntax? a) (syntax->datum a) a))
                   args))))))
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

(define-record-type <jit-context>
  (make-jit-context ptr parent)
  jit-context?
  (ptr jit-context-ptr)
  (parent jit-context-parent))

(define-ptr-record jit-result)

(define-ptr-record jit-object)
(define-ptr-record jit-location)
(define-ptr-record jit-field)
(define-ptr-record jit-function)
(define-ptr-record jit-block)
(define-ptr-record jit-case)
(define-ptr-record jit-extended-asm)

(define-ptr-record jit-type)
(define-ptr-record jit-type/struct)
(define-ptr-record jit-type/function)
(define-ptr-record jit-type/vector)

(define-ptr-record jit-rvalue)
(define-ptr-record jit-lvalue)
(define-ptr-record jit-parameter)


(define current-context (make-parameter #f))

(define *guardian-jit-context* (make-guardian))
(define *guardian-jit-result*  (make-guardian))


                                        ; Error

(define (get-first-error context)
  (and-let* ((ptr (f:context-get-first-error/const-char-* (jit-context-ptr context)))
             ((not (null-pointer? ptr))))
    (string-copy (pointer->string ptr))))

(define (get-last-error context)
  (and-let* ((ptr (f:context-get-last-error/const-char-* (jit-context-ptr context)))
             ((not (null-pointer? ptr))))
    (string-copy (pointer->string ptr))))

(define-inlinable (throw-if-context-error context)
  (let ((last-err (get-last-error context)))
    (when last-err
      (throw 'gcc-jit last-err))))

(define-syntax-rule (with-jit-err context return)
  (let ((ptr return))
    (throw-if-context-error context)
    ptr))


                                        ; Printers

(define-syntax define-printer-for-like-jit-object
  (λ (stx)
    (syntax-case stx ()
      ((_ (xname xupcast-raw) ...)
       #'(begin (define-printer-for-like-jit-object xname xupcast-raw)
                ...))
      ((_ xname xupcast-raw)
       (with-syntax ((xrec     (format-syntax/symbol #'xname "<~a>" #'xname))
                     (xptr-ref (format-syntax/symbol #'xname "~a-ptr" #'xname))
                     (xfmt-str (format-syntax/string #'xname "<~a ~~a>" #'xname)))
         #'(set-record-type-printer!
            xrec
            (λ (record port)
              (or (and-let* ((rptr (xptr-ref record))
                             ((pointer? rptr))
                             ((not (null-pointer? rptr)))
                             (uptr (xupcast-raw rptr))
                             ((not (null-pointer? uptr)))
                             (sptr (f:object-get-debug-string/const-char-* uptr))
                             ((not (null-pointer? sptr))))
                    (format port xfmt-str (pointer->string sptr)))
                  (format port xfmt-str (xptr-ref record))))))))))

(define-printer-for-like-jit-object
  (jit-object       identity)
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
  (jit-type/struct (-> f:struct-as-type/gcc-jit-type-* f:type-as-object/gcc-jit-object-*))
  ;; jit-type/function & jit-type/vector has no converters
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

(define-inlinable (list-of-jit-fields? a)
  (and (list? a)
       (every jit-field? a)))


                                        ; To Conversions

(define-syntax define-to-conversion
  (λ (stx)
    (syntax-case stx ()
      ((_ (xfrom xto xconv) ...)
       #'(begin (define-to-conversion xfrom xto xconv)
                ...))
      ((_ xfrom xto xconv)
       (with-syntax ((xconvfn  (format-syntax/symbol #'xconv "~a->~a" #'xfrom #'xto))
                     (xptr-ref (format-syntax/symbol #'xfrom "~a-ptr" #'xfrom))
                     (xtomake  (format-syntax/symbol #'xto  "make-~a" #'xto)))
         #'(define (xconvfn xfrom)
             (-> xfrom xptr-ref xconv xtomake)))))))

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
  (jit-rvalue jit-object f:rvalue-as-object/gcc-jit-object-*)
  
  (jit-type/struct jit-type f:struct-as-type/gcc-jit-type-*))
;; These don't have direct to `->object` implementations
;; jit-struct, jit-function-type & jit-vector-type

(define-syntax with-return-pointer-guard
  (λ (stx)
    (define (type->make type-stx)
      (let ((type (-> (syntax->datum type-stx)
                      symbol->string)))
        (format-syntax/symbol type-stx "make-~a" (substring type 1 (1- (string-length type))))))
    
    (syntax-case stx ()
      ((_ xreturn-type (xdef xparams xdoc . xbody)) (string? (syntax->datum #'xdoc))
       (with-syntax ((xmake (type->make #'xreturn-type)))
         #'(xdef xparams
             xdoc
             (let ((out (begin . xbody)))
               (if (null-pointer? out)
                   #f
                   (xmake out))))))

      ((_ xreturn-type (xdef xparams . xbody))
       (with-syntax ((xmake (type->make #'xreturn-type)))
         #'(xdef xparams
             (let ((out (begin . xbody)))
               (if (null-pointer? out)
                   #f
                   (xmake out)))))))))

;; Note: Probably no need to guard records as the type gets check on the *-ptr call
(define-syntax-rule (guards (guard? var) ...)
  (begin (unless (guard? var) (throw 'wrong-type-arg 'guard? var))
         ...))

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

(define (jit-object->debug-string obj)
  (let ((sptr (-> obj jit-object-ptr f:object-get-debug-string/const-char-*)))
    (if (null-pointer? sptr)
        #f
        (string-copy (pointer->string sptr)))))

(with-return-pointer-guard
 <jit-type>
 (define* (symbol->jit-type sym #:optional (context (current-context)))
   (guards (enum:types? sym))

   (with-jit-err
    context
    (f:context-get-type/gcc-jit-type-*
     (jit-context-ptr context)
     (hashq-ref enum:types/sym->int sym)))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (string->string-literal str #:optional (context (current-context)))
   (with-jit-err
    context
    (f:context-new-string-literal/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (string->pointer str)))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (jit-type->0 type #:optional (context (current-context)))
   (with-jit-err
    context
    (f:context-zero/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (jit-type-ptr type)))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (jit-type->1 type #:optional (context (current-context)))
   (with-jit-err
    context
    (f:context-one/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (jit-type-ptr type)))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (integer->jit-rvalue type int #:optional (context (current-context)))
   (guards (integer? int))

   (with-jit-err
    context
    (f:context-new-rvalue-from-int/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (jit-type-ptr type)
     int))))

;; (f:context-new-rvalue-from-ptr/gcc-jit-rvalue-*
;;  f:context-new-rvalue-from-long/gcc-jit-rvalue-*
;;  f:context-new-rvalue-from-double/gcc-jit-rvalue-*
;;  f:context-new-rvalue-from-vector/gcc-jit-rvalue-*)

(define (jit-object-like->jit-context obj)
  (let ((ptr (f:object-get-context/gcc-jit-context-*
              (record-accessor obj 'ptr))))
    (if (null-pointer? ptr)
        #f
        (make-jit-context ptr #f))))

                                        ; Common Functions

(define (context-acquire)
  (let ((cx (make-jit-context (f:context-acquire/gcc-jit-context-*) #f)))
    (*guardian-jit-context* cx)
    cx))

(define (context-release context)
  (f:context-release/void (jit-context-ptr context)))

(define (context-acquire/child parent-context)
  (let* ((ptr (f:context-new-child-context/gcc-jit-context-*
               (jit-context-ptr parent-context)))
         (cx (make-jit-context ptr parent-context)))
    (*guardian-jit-context* cx)
    cx))

(define* (set-boolean-option! option bool #:optional (context (current-context)))
  (guards (boolean? bool)
          (enum:bool-option? option))

  (with-jit-err
   context
   (f:context-set-bool-option/void
    (jit-context-ptr context)
    (hashq-ref enum:bool-option/sym->int option)
    (if bool 1 0))))

(with-return-pointer-guard
 <jit-parameter>
 (define* (new-parameter type name #:optional (context (current-context)) #:key (location #f))
   (guards
    (string? name)
    (f-or-jit-location? location))

   (with-jit-err
    context
    (f:context-new-param/gcc-jit-param-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (jit-type-ptr type)
     (string->pointer name)))))

(with-return-pointer-guard
 <jit-function>
 (define* (new-function name kind return-type parameter-list #:optional (context (current-context)) #:key (location #f) (variadic? #f) )
   (guards
    (string? name)
    (enum:function-kind? kind)
    (f-or-jit-location? location)
    (list-of-jit-parameters? parameter-list))

   (with-jit-err
    context
    (f:context-new-function/gcc-jit-function-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (hashq-ref enum:function-kind/sym->int kind)
     (jit-type-ptr return-type)
     (string->pointer name)
     (length parameter-list)
     (bytevector->pointer (pointer-list->bytevector (map jit-parameter-ptr parameter-list)))
     (if variadic? 1 0)))))

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

   (with-jit-err
    context
    (f:context-new-call/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (jit-function-ptr function)
     (length rvalue-list)
     (bytevector->pointer (pointer-list->bytevector (map jit-rvalue-ptr rvalue-list)))))))

(define* (block-end/void block #:optional #:key (location #f))
  (guards (f-or-jit-location? location))

  (f:block-end-with-void-return/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)))

(define* (block-end/return block return-value #:optional #:key (location #f))
  (guards (f-or-jit-location? location))

  (f:block-end-with-return/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)
   (jit-rvalue-ptr return-value)))

(define* (block-end/jump block block-target #:optional #:key (location #f))
  (guards (f-or-jit-location? location))

  (f:block-end-with-jump/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)
   (jit-block-ptr block-target)))

(define* (block-end/conditional block boolean true-block false-block #:optional #:key (location #f))
  (guards (f-or-jit-location? location))

  (f:block-end-with-conditional/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)
   (jit-rvalue-ptr boolean)
   (jit-block-ptr true-block)
   (jit-block-ptr false-block)))

;; f:block-end-with-switch/void
;; f:block-end-with-extended-asm-goto/gcc-jit-extended-asm-*

(define* (compile-jit #:optional (context (current-context)))
  (let ((ptr (with-jit-err
              context
              (f:context-compile/gcc-jit-result-* (jit-context-ptr context)))))
    (if (null-pointer? ptr)
        #f
        (let ((res (make-jit-result ptr)))
          (*guardian-jit-result* res)
          res))))
            
(define (jit-result->get-code-ptr result name)
  (guards (string? name))

  (f:result-get-code/void-*
   (jit-result-ptr result)
   (string->pointer name)))

(define (jit-result->procedure result name ffi-return-type . ffi-arg-types)
  (let ((pptr (jit-result->get-code-ptr result name)))
    (if (null-pointer? pptr)
        (throw 'gcc-jit "Can't get procedure" name)
        (pointer->procedure ffi-return-type pptr ffi-arg-types))))

(define (jit-result-release result)
  (f:result-release/void (jit-result-ptr result)))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (new-binary-op binary-op return-type a b #:optional (context (current-context)) #:key (location #f))
   (guards
    (f-or-jit-location? location)
    (enum:binary-op? binary-op))

   (with-jit-err
    context
    (f:context-new-binary-op/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (hashq-ref enum:binary-op/sym->int binary-op)
     (jit-type-ptr return-type)
     (jit-rvalue-ptr a)
     (jit-rvalue-ptr b)))))

(with-return-pointer-guard
 <jit-lvalue>
 (define* (new-local function name type #:optional #:key (location #f))
   (guards
    (f-or-jit-location? location)
    (string? name))
   
   (f:function-new-local/gcc-jit-lvalue-*
    (jit-function-ptr function)
    (jit-location-ptr/nullable location)
    (jit-type-ptr type)
    (string->pointer name))))

(define* (add-assignment block lvalue rvalue #:optional #:key (location #f))
  (guards (f-or-jit-location? location))
  
  (f:block-add-assignment/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)
   (jit-lvalue-ptr lvalue)
   (jit-rvalue-ptr rvalue)))

(define* (add-assignment/op block operation lvalue rvalue #:optional #:key (location #f))
  (guards (f-or-jit-location? location)
          (enum:binary-op? operation))

  (f:block-add-assignment-op/void
   (jit-block-ptr block)
   (jit-location-ptr/nullable location)
   (jit-lvalue-ptr lvalue)
   (hashq-ref enum:binary-op/sym->int operation)
   (jit-rvalue-ptr rvalue)))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (new-comparison compare a b #:optional (context (current-context)) #:key (location #f))
   (guards (enum:comparison? compare)
           (f-or-jit-location? location))

   (with-jit-err
    context
    (f:context-new-comparison/gcc-jit-rvalue-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (hashq-ref enum:comparison/sym->int compare)
     (jit-rvalue-ptr a)
     (jit-rvalue-ptr b)))))

(define (jit-type-sizeof type)
  (f:type-get-size/ssize-t
   (jit-type-ptr type)))

;; typing tools
(with-return-pointer-guard
 <jit-type>
 (define (jit-type->pointer-type type)
   "'T' => 'T*'"
   (f:type-get-pointer/gcc-jit-type-* (jit-type-ptr type))))

(with-return-pointer-guard
 <jit-type>
 (define (jit-type->const-type type)
   "'T' => 'const T'"
   (f:type-get-const/gcc-jit-type-* (jit-type-ptr type))))

(with-return-pointer-guard
 <jit-type>
 (define (jit-type->volatile-type type)
   "'T' => 'volatile T'"
   (f:type-get-volatile/gcc-jit-type-* (jit-type-ptr type))))

(with-return-pointer-guard
 <jit-type>
 (define* (jit-type->array-type type count #:optional (context (current-context)) #:key (location #f))
   "'T' => 'T[N]'"
   (guards (f-or-jit-location? location)
           (integer? count))

   (with-jit-err
    context
    (f:context-new-array-type/gcc-jit-type-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (jit-type-ptr type)
     count))))

(with-return-pointer-guard
 <jit-type>
 (define* (jit-type->aligned-type type #:optional (byte-alignment (jit-type-sizeof type)))
   "'T' => 'align(sizeof) T'"
   (f:type-get-aligned/gcc-jit-type-*
    (jit-type-ptr type)
    byte-alignment)))

;; other
(with-return-pointer-guard
 <jit-lvalue>
 (define* (new-array-access pointer index #:optional (context (current-context)) #:key (location #f))
   (guards (f-or-jit-location? location))

   (with-jit-err
    context
    (f:context-new-array-access/gcc-jit-lvalue-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (jit-rvalue-ptr pointer)
     (jit-rvalue-ptr index)))))

;;gcc_jit_context_new_function_ptr_type

;; struct

(with-return-pointer-guard
 <jit-field>
 (define* (new-field name type #:optional (context (current-context)) #:key (location #f))
   (guards (f-or-jit-location? location))

   (with-jit-err
    context
    (f:context-new-field/gcc-jit-field-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (jit-type-ptr type)
     (string->pointer name)))))

(with-return-pointer-guard
 <jit-type/struct>
 (define* (new-struct name fields #:optional (context (current-context)) #:key (location #f))
   (guards (f-or-jit-location? location)
           (list-of-jit-fields? fields))

   (with-jit-err
    context
    (f:context-new-struct-type/gcc-jit-struct-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (string->pointer name)
     (length fields)
     (-> (map jit-field-ptr fields)
         pointer-list->bytevector
         bytevector->pointer)))))

;; (define-syntax-rule (define-jit-struct name (field type) ...)
;;   (new-struct (symbol->string 'name) (list (new-field (symbol->string 'field) type) ...)))

(with-return-pointer-guard
 <jit-type/struct>
 (define* (new-opaque-struct name #:optional (context (current-context)) #:key (location #f))
   (guards (f-or-jit-location? location))

   (with-jit-err
    context
    (f:context-new-opaque-struct/gcc-jit-struct-*
     (jit-context-ptr context)
     (jit-location-ptr/nullable location)
     (string->pointer name)))))

(with-return-pointer-guard
 <jit-lvalue>
 (define* (access-field/lvalue expr field #:optional #:key (location #f))
   "expr.field = ..."
   (guards (f-or-jit-location? location))
   
   (f:lvalue-access-field/gcc-jit-lvalue-*
    (jit-lvalue-ptr expr)
    (jit-location-ptr/nullable location)
    (jit-field-ptr field))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (access-field/rvalue expr field #:optional #:key (location #f))
   "expr.field"
   (guards (f-or-jit-location? location))
   
   (f:rvalue-access-field/gcc-jit-rvalue-* 
    (jit-rvalue-ptr expr)
    (jit-location-ptr/nullable location)
    (jit-field-ptr field))))

(with-return-pointer-guard
 <jit-rvalue>
 (define* (dereference-field ptr field #:optional #:key (location #f))
   "ptr->field"
   (guards (f-or-jit-location? location))
   
   (f:rvalue-dereference-field/gcc-jit-lvalue-*
    (jit-rvalue-ptr ptr)
    (jit-location-ptr/nullable location)
    (jit-field-ptr field))))

;;context-set-fields
;;new-bitfield

;; new-union


                                        ; GC Hook Cleanup

(add-hook! after-gc-hook
  (λ () (let lp ((discard (*guardian-jit-context*)))
          (when discard
            (context-release discard)
            (lp (*guardian-jit-context*))))))

(add-hook! after-gc-hook
  (λ () (let lp ((discard (*guardian-jit-result*)))
          (when discard
            (jit-result-release discard)
            (lp (*guardian-jit-result*))))))



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
    (set-boolean-option! 'dump-generated-code #t)
    (let* ((result (example-1))
           (gfn (jit-result->procedure result "greet" void '*)))
      (gfn (string->pointer "Abby!")))))



(define (example-2)
  (define type:int (symbol->jit-type 'int))

  (let* ((param-i    (new-parameter type:int "i"))
         (square-def (new-function "square" 'exported type:int (list param-i)))
         (block      (new-block square-def))
         (expr       (new-binary-op '* type:int 
                                    (jit-parameter->jit-rvalue param-i)
                                    (jit-parameter->jit-rvalue param-i))))
    (block-end/return block expr)
    (compile-jit)))

(define (run-example-2)
  (parameterize ((current-context (context-acquire)))
    (set-boolean-option! 'dump-generated-code #t)
    (let* ((result (example-2))
           (gfn (jit-result->procedure result "square" int int)))
      (format #t "squaring 5 in jit: ~s~%" (gfn 5)))))



(define-syntax with-block
  (syntax-rules (_)
    ((_ block (block-fn _  . args) ...)
     (begin
       (block-fn block . args) ...))))

(define (example-3)
  (define type:int (symbol->jit-type 'int))

  (let* ((param-n (new-parameter type:int "n"))
         (fn (new-function "loop_test" 'exported type:int (list param-n)))
         (local-i   (new-local fn "i"   type:int))
         (local-sum (new-local fn "sum" type:int))
         (block:initial  (new-block fn "inital"))
         (block:lp-cond  (new-block fn "loop_cond"))
         (block:lp-body  (new-block fn "loop_body"))
         (block:after-lp (new-block fn "after_loop")))
    
    (with-block block:initial
      (add-assignment _ local-sum (jit-type->0 type:int))
      (add-assignment _ local-i   (jit-type->0 type:int))
      (block-end/jump _ block:lp-cond))

    (let ((cmp (new-comparison '>= (jit-lvalue->jit-rvalue local-i) (jit-parameter->jit-rvalue param-n))))
      (block-end/conditional block:lp-cond cmp block:after-lp block:lp-body))

    (with-block block:lp-body
      (add-assignment/op _ '+ local-sum (new-binary-op '* type:int
                                                       (jit-lvalue->jit-rvalue local-i)
                                                       (jit-lvalue->jit-rvalue local-i)))
      (add-assignment/op _ '+ local-i (jit-type->1 type:int))
      (block-end/jump _ block:lp-cond))

    (block-end/return block:after-lp (jit-lvalue->jit-rvalue local-sum))
    
    (compile-jit)))

(define (run-example-3)
  (parameterize ((current-context (context-acquire)))
    (set-boolean-option! 'dump-generated-code #t)
    (let* ((result (example-3))
           (gfn (jit-result->procedure result "loop_test" int int)))
      (format #t "result of loop over 10: ~s~%" (gfn 10)))))


