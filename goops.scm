;; BSD-3-Clause : Copyright © 2025 Abigale Raeck.

;; ** Nicked from =libgccjit.h=
;; An object created within a context.  Such objects are automatically
;;    cleaned up when the context is released.

;;    The class hierarchy looks like this:

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


(define-class <jit-object> ()
  (ptr #:getter jit-object->pointer #:init-keyword #:ptr))

(define-class <jit-location>      (<jit-object>))

(define-class <jit-type>          (<jit-object>))
(define-class <jit-struct>        (<jit-type>))
(define-class <jit-function-type> (<jit-type>))
(define-class <jit-vector-type>   (<jit-type>))

(define-class <jit-field>        (<jit-object>))
(define-class <jit-function>     (<jit-object>))
(define-class <jit-block>        (<jit-object>))

(define-class <jit-rvalue>       (<jit-object>))
(define-class <jit-lvalue>       (<jit-rvalue>))
(define-class <jit-param>        (<jit-lvalue>))

(define-class <jit-case>         (<jit-object>))
(define-class <jit-extended-asm> (<jit-object>))

(define-class <jit-context> ()
  (ptr #:getter jit-context->pointer #:init-keyword #:ptr)
  (pool #:init-thunk list))

(define-syntax-rule (add-to-pool context value)
  (slot-set! context 'pool (cons value (slot-ref context 'pool))))

(define-class <jit-result> ()
  (ptr #:getter jit-result->pointer #:init-keyword #:ptr))

;; (define-syntax-rule (define-method/pointer-guard return-type (name args* ...) . body)
;;   (define-method (name args* ...)
;;     (let ((out (begin . body)))
;;       (if (null-pointer? out)
;;           #f
;;           (make return-type out)))))

;; (define-syntax with-pointer-guard
;;   (syntax-rules ()
;;     ((_ return-type (def (name args* ...) . body))
;;      (def (name args* ...)
;;        (let ((out (begin . body)))
;;          (if (null-pointer? out)
;;              #f
;;              (make return-type out)))))))

(define-syntax-rule (with-pointer-guard return-type (def params . body))
  (def params
    (let ((out (begin . body)))
      (if (null-pointer? out)
          #f
          (make return-type #:ptr out)))))

;; (define-class <jit-location+boolean> (<jit-location> <boolean>))

;; (with-pointer-guard
;;  <jit-type>
;;  (define-method (get-jit-type (ctxt <jit-context>) (types-symbol <symbol>))
;;    (ffi-context-get-type
;;     (jit-context->pointer ctxt)
;;     (hashq-ref enum-jit-type/si types-symbol))))

(define-inlinable (false-or-pointer->raw-pointer val unwrap-fn)
  (if (boolean? val)
      (if val
          (error 'expected-false-not-true)
          %null-pointer)
      (unwrap-fn val)))

;; (define-method (unwrap-ptr (ptr-obj <jit-object>))
;;   (jit-obj-ptr ptr))

;; (define-method (unwrap-ptr (ptr-obj <jit-context>))
;;   (jit-context-ptr ptr-obj))

(define (context-acquire)
  (make <jit-context> #:ptr (f:context-acquire/gcc-jit-context-*)))

(define-method (context-release (ctxt <jit-context>))
  (f:context-release/void (jit-context->pointer ctxt)))

(with-pointer-guard
 <jit-type>
 (define-method (get-jit-type (ctxt <jit-context>) (types-symbol <symbol>))
   (f:context-get-type/gcc-jit-type-*
    (jit-context->pointer ctxt)
    (hashq-ref enum:types/sym->int types-symbol))))

(with-pointer-guard
 <jit-param>
 (define-method (context-new-param (ctxt <jit-context>) location (type <jit-type>) (name <string>))
   (unless (or (boolean? location) (is-a? location <jit-location>))
     (error 'not-a-bool-or-jit-location location))
   (add-to-pool ctxt name)
   (f:context-new-param/gcc-jit-param-*
    (jit-context->pointer ctxt)
    (false-or-pointer->raw-pointer location jit-object->pointer)
    (jit-object->pointer type)
    (string->pointer name))))

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

(define-inlinable (list-of-jit-params? a)
  (and (list? a)
       (every (cut is-a? <> <jit-param>) a)))


(with-pointer-guard
 <jit-function>
 (define-method (context-new-function (ctxt <jit-context>)
                                      location
                                      (kind <symbol>)
                                      (return-type <jit-type>)
                                      (name <string>)
                                      (params <list>)
                                      (variadic? <boolean>))
   (unless (or (boolean? location) (is-a? location <jit-location>))
     (error 'not-a-bool-or-jit-location location))
   (unless (list-of-jit-params? params)
     (error 'expected-list-of-jit-params params))
   
   (let ((params-bv (pointer-list->bytevector (map jit-object->pointer params))))

     (add-to-pool ctxt name)
     (add-to-pool ctxt params-bv)
     
     (f:context-new-function/gcc-jit-function-*
      (jit-context->pointer ctxt)
      (false-or-pointer->raw-pointer location jit-object->pointer)
      (hashq-ref enum:function-kind/sym->int kind)
      (jit-object->pointer return-type)
      (string->pointer name)
      (length params)
      (bytevector->pointer params-bv)
      (if variadic? 1 0)))))

(with-pointer-guard
 <jit-rvalue>
 (define-method (context-new-string-literal (ctxt <jit-context>) (str <string>))
   (add-to-pool ctxt str)
   (f:context-new-string-literal/gcc-jit-rvalue-*
    (jit-context->pointer ctxt)
    (string->pointer str))))

(with-pointer-guard
 <jit-rvalue>
 (define-method (context-new-rvalue-from-int (ctxt <jit-context>) (type <jit-type>) (int <integer>))
   (f:context-new-rvalue-from-int/gcc-jit-rvalue-*
    (jit-context->pointer ctxt)
    (jit-object->pointer type)
    int)))

(with-pointer-guard
 <jit-rvalue>
 (define-method (jit-param->jit-rvalue (param <jit-param>))
   (f:param-as-rvalue/gcc-jit-rvalue-* (jit-object->pointer param))))

(with-pointer-guard
 <jit-block>
 (define-method (function-new-block (fn <jit-function>) name)
   (unless (or (boolean? name) (string? name))
     (error 'not-a-bool-or-string name))
   ;; (when (string? name)
   ;;   (add-to-pool ctxt name))
   (f:function-new-block/gcc-jit-block-*
    (jit-object->pointer fn)
    (false-or-pointer->raw-pointer name string->pointer))))

(define-method (block-add-eval (block <jit-block>) location (rvalue <jit-rvalue>))
  (unless (or (boolean? location) (is-a? location <jit-location>))
    (error 'not-a-bool-or-jit-location location))
  (f:block-add-eval/void
   (jit-object->pointer block)
   (false-or-pointer->raw-pointer location jit-object->pointer)
   (jit-object->pointer rvalue)))

(define-inlinable (list-of-jit-rvalues? a)
  (and (list? a)
       (every (cut is-a? <> <jit-rvalue>) a)))

(with-pointer-guard
 <jit-rvalue>
 (define-method (context-new-call (ctxt <jit-context>)
                                  location
                                  (fn <jit-function>)
                                  (rvalues <list>))
   (unless (or (boolean? location) (is-a? location <jit-location>))
     (error 'not-a-bool-or-jit-location location))
   (unless (list-of-jit-rvalues? rvalues)
     (error 'expected-list-of-jit-rvalues rvalues))
   
   (let ((bv (pointer-list->bytevector (map jit-object->pointer rvalues))))
     (add-to-pool ctxt bv)
     (f:context-new-call/gcc-jit-rvalue-*
      (jit-context->pointer ctxt)
      (false-or-pointer->raw-pointer location jit-object->pointer)
      (jit-object->pointer fn)
      (length rvalues)
      (bytevector->pointer bv)))))

(define-method (block-end-with-void-return (block <jit-block>) location)
  (unless (or (boolean? location) (is-a? location <jit-location>))
    (error 'not-a-bool-or-jit-location location))

  (f:block-end-with-void-return/void
   (jit-object->pointer block)
   (false-or-pointer->raw-pointer location jit-object->pointer)))

(define-method (block-end-with-return (block <jit-block>) location (rvalue <jit-rvalue>))
  (unless (or (boolean? location) (is-a? location <jit-location>))
    (error 'not-a-bool-or-jit-location location))

  (f:block-end-with-return/void
   (jit-object->pointer block)
   (false-or-pointer->raw-pointer location jit-object->pointer)
   (jit-object->pointer rvalue)))

(define-method (context-set-bool-option (ctxt <jit-context>)
                                        (option <symbol>)
                                        (state <boolean>))
  (unless (enum:bool-option? option)
    (error 'not-a-bool-option option))
  
  (f:context-set-bool-option/void
   (jit-context->pointer ctxt)
   (hashq-ref enum:bool-option/sym->int option)
   (if state 1 0)))

(with-pointer-guard
 <jit-result>
 (define-method (context-compile (ctxt <jit-context>))
   (f:context-compile/gcc-jit-result-* (jit-context->pointer ctxt))))

(define-method (jit-result->get-code (result <jit-result>) (name <string>))
  ;;(add-to-pool ctxt name)
  (f:result-get-code/void-*
   (jit-result->pointer result)
   (string->pointer name)))

(define-method (jit-result-release (result <jit-result>))
  (f:result-release/void (jit-result->pointer result)))

(with-pointer-guard
 <jit-rvalue>
 (define-method (context-new-binary-op (ctxt <jit-context>)
                                       location
                                       (binary-op <symbol>)
                                       (return-type <jit-type>)
                                       (a <jit-rvalue>)
                                       (b <jit-rvalue>))
   (unless (or (boolean? location) (is-a? location <jit-location>))
     (error 'not-a-bool-or-jit-location location))

   (unless (enum:binary-op? binary-op)
     (error 'not-a-binary-op binary-op))
   
   (f:context-new-binary-op/gcc-jit-rvalue-*
    (jit-context->pointer ctxt)
    (false-or-pointer->raw-pointer location jit-object->pointer)
    (hashq-ref enum:binary-op/sym->int binary-op)
    (jit-object->pointer return-type)
    (jit-object->pointer a)
    (jit-object->pointer b))))



                                        ; Testing with goops

(define (example-1 context)
  (define type:const-char* (get-jit-type context 'const-char-ptr))
  (define type:void        (get-jit-type context 'void))
  (define type:int         (get-jit-type context 'int))
  
  (define function:printf
    (let* ((param (context-new-param context #f type:const-char* "format")))
      (context-new-function context #f 'imported type:int "printf" (list param) #t)))

  (define function:greet
    (let* ((param-name  (context-new-param context #f type:const-char* "name"))
           (greet-def   (context-new-function context #f 'exported type:void "greet" (list param-name) #f))
           (str-literal (context-new-string-literal context "hello %s\n"))
           (rvalue-name (jit-param->jit-rvalue param-name))
           (block:1     (function-new-block greet-def #f)))
      (block-add-eval block:1 #f (context-new-call context #f function:printf (list str-literal rvalue-name)))
      (block-end-with-void-return block:1 #f)))

  (context-compile context))

(define (run-example-1)
  (let ((ctxt #f))
    (dynamic-wind
      (λ ()
        (set! ctxt (context-acquire)))
      (λ ()
        (context-set-bool-option ctxt 'dump-generated-code #t)

        (let* ((result (example-1 ctxt))
               (gfn (jit-result->get-code result "greet")))
          ((pointer->procedure void gfn (list '*)) (string->pointer "Abby!"))
          (jit-result-release result)))
      (λ ()
        (context-release ctxt)
        (set! ctxt #f)))))


(define (example-2 context)
  (define type:int (get-jit-type context 'int))

  (let* ((param-i (context-new-param context #f type:int "i"))
         (square-def (context-new-function context #f 'exported type:int "square" (list param-i) #f))
         (block (function-new-block square-def #f))
         (expr (context-new-binary-op context #f 'mult type:int
                                      (jit-param->jit-rvalue param-i)
                                      (jit-param->jit-rvalue param-i))))
    (block-end-with-return block #f expr)
    (context-compile context)))

(define (run-example-2)
  (let ((ctxt #f))
    (dynamic-wind
      (λ ()
        (set! ctxt (context-acquire)))
      (λ ()
        (context-set-bool-option ctxt 'dump-generated-code #t)

        (let* ((result (example-2 ctxt))
               (gfn (jit-result->get-code result "square")))
          (display ((pointer->procedure int gfn (list int)) 5))
          (newline)
          (jit-result-release result)))
      (λ ()
        (context-release ctxt)
        (set! ctxt #f)))))



(define (c:define-gsubr context)
  (let* ((type:void*       (get-jit-type context 'void-ptr))
         (type:int         (get-jit-type context 'int))
         (type:const-char* (get-jit-type context 'const-char-ptr))
         (param-name (context-new-param context #f type:const-char* "name"))
         (param-req  (context-new-param context #f type:int "req"))
         (param-opt  (context-new-param context #f type:int "opt"))
         (param-rst  (context-new-param context #f type:int "rst"))
         (param-fn   (context-new-param context #f type:void* "fn")))
    (context-new-function context #f 'imported type:void* "scm_c_define_gsubr"
                          (list param-name param-req param-opt param-rst param-fn) #f)))

(define (c:in->out context)
  (let* ((type:void*  (get-jit-type context 'void-ptr))
         (param-scm   (context-new-param context #f type:void* "scm"))
         (in->out-def (context-new-function context #f 'exported type:int "in_to_out" (list param-scm) #f))
         (block       (function-new-block in->out-def #f)))
    (block-end-with-return block #f (jit-param->jit-rvalue param-scm))
    in->out-def))

(define (c:register context)
  (let ((type:void    (get-jit-type context 'void))
        (type:int     (get-jit-type context 'int))
        (register-def (context-new-function context #f 'exported type:void "register" '() #f))
        (str-name     (context-new-string-literal context "lazarus"))
        (int1         (context-new-rvalue-from-int context type:int 1))
        (int0         (context-new-rvalue-from-int context type:int 0))
        (block:1      (function-new-block register-def #f)))
    (block-add-eval block:1 #f (context-new-call context #f c:define-gsubr (list str-name int1 int0 int0 fn-ptr)))))
