;; Exported bindings from libgccjit-bindings.scm

(define-module (libgccjit-exported)
  #:use-module (ice-9 hash-table) ;; additional alist->... fns
  #:use-module (system foreign)
  #:use-module (system foreign-library))

(define gcc-jit-lib
  (load-foreign-library
   "libgccjit"
   #:search-path '("/gnu/store/6wnizcd3zsq2cbinwczfilpn7dsbh17j-libgccjit-15.1.0/lib/")))

(begin
  (define ffi:context-set-output-ident
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_output_ident"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:context-set-output-ident/void
           ctxt/gcc-jit-context-*
           output-ident/const-char-*)
    (ffi:context-set-output-ident
      ctxt/gcc-jit-context-*
      output-ident/const-char-*))
  (define ffi:lvalue-add-string-attribute
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_add_string_attribute"
      #:return-type
      void
      #:arg-types
      (list '* int '*)))
  (define (f:lvalue-add-string-attribute/void
           variable/gcc-jit-lvalue-*
           attribute/enum-gcc-jit-variable-attribute
           value/const-char-*)
    (ffi:lvalue-add-string-attribute
      variable/gcc-jit-lvalue-*
      attribute/enum-gcc-jit-variable-attribute
      value/const-char-*))
  (define enum:variable-attribute/alist
    '((visibility . 0) (max . 1)))
  (define enum:variable-attribute/sym->int
    (alist->hashq-table
      enum:variable-attribute/alist))
  (define enum:variable-attribute/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:variable-attribute/alist)))
  (define (#{% enum:variable-attribute?-procedure}# a)
    (and (symbol? a)
         (not (boolean?
                (hashq-ref enum:variable-attribute/sym->int a)))))
  (define ffi:function-add-integer-array-attribute
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_add_integer_array_attribute"
      #:return-type
      void
      #:arg-types
      (list '* int '* size_t)))
  (define (f:function-add-integer-array-attribute/void
           func/gcc-jit-function-*
           attribute/enum-gcc-jit-fn-attribute
           value/const-int-*
           length/size-t)
    (ffi:function-add-integer-array-attribute
      func/gcc-jit-function-*
      attribute/enum-gcc-jit-fn-attribute
      value/const-int-*
      length/size-t))
  (define ffi:function-add-string-attribute
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_add_string_attribute"
      #:return-type
      void
      #:arg-types
      (list '* int '*)))
  (define (f:function-add-string-attribute/void
           func/gcc-jit-function-*
           attribute/enum-gcc-jit-fn-attribute
           value/const-char-*)
    (ffi:function-add-string-attribute
      func/gcc-jit-function-*
      attribute/enum-gcc-jit-fn-attribute
      value/const-char-*))
  (define ffi:function-add-attribute
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_add_attribute"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:function-add-attribute/void
           func/gcc-jit-function-*
           attribute/enum-gcc-jit-fn-attribute)
    (ffi:function-add-attribute
      func/gcc-jit-function-*
      attribute/enum-gcc-jit-fn-attribute))
  (define enum:fn-attribute/alist
    '((alias . 0)
      (always-inline . 1)
      (inline . 2)
      (noinline . 3)
      (target . 4)
      (used . 5)
      (visibility . 6)
      (cold . 7)
      (returns-twice . 8)
      (pure . 9)
      (const . 10)
      (weak . 11)
      (nonnull . 12)
      (max . 13)))
  (define enum:fn-attribute/sym->int
    (alist->hashq-table enum:fn-attribute/alist))
  (define enum:fn-attribute/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:fn-attribute/alist)))
  (define (#{% enum:fn-attribute?-procedure}# a)
    (and (symbol? a)
         (not (boolean?
                (hashq-ref enum:fn-attribute/sym->int a)))))
  (define ffi:type-unqualified
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_unqualified"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-unqualified/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-unqualified type/gcc-jit-type-*))
  (define ffi:vector-type-get-element-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_vector_type_get_element_type"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:vector-type-get-element-type/gcc-jit-type-*
           vector-type/gcc-jit-vector-type-*)
    (ffi:vector-type-get-element-type
      vector-type/gcc-jit-vector-type-*))
  (define ffi:vector-type-get-num-units
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_vector_type_get_num_units"
      #:return-type
      size_t
      #:arg-types
      (list '*)))
  (define (f:vector-type-get-num-units/size-t
           vector-type/gcc-jit-vector-type-*)
    (ffi:vector-type-get-num-units
      vector-type/gcc-jit-vector-type-*))
  (define ffi:type-is-struct
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_is_struct"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-is-struct/gcc-jit-struct-*
           type/gcc-jit-type-*)
    (ffi:type-is-struct type/gcc-jit-type-*))
  (define ffi:type-dyncast-vector
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_dyncast_vector"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-dyncast-vector/gcc-jit-vector-type-*
           type/gcc-jit-type-*)
    (ffi:type-dyncast-vector type/gcc-jit-type-*))
  (define ffi:type-is-pointer
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_is_pointer"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-is-pointer/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-is-pointer type/gcc-jit-type-*))
  (define ffi:type-is-integral
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_is_integral"
      #:return-type
      int
      #:arg-types
      (list '*)))
  (define (f:type-is-integral/int type/gcc-jit-type-*)
    (ffi:type-is-integral type/gcc-jit-type-*))
  (define ffi:function-type-get-param-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_type_get_param_type"
      #:return-type
      '*
      #:arg-types
      (list '* size_t)))
  (define (f:function-type-get-param-type/gcc-jit-type-*
           function-type/gcc-jit-function-type-*
           index/size-t)
    (ffi:function-type-get-param-type
      function-type/gcc-jit-function-type-*
      index/size-t))
  (define ffi:function-type-get-param-count
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_type_get_param_count"
      #:return-type
      size_t
      #:arg-types
      (list '*)))
  (define (f:function-type-get-param-count/size-t
           function-type/gcc-jit-function-type-*)
    (ffi:function-type-get-param-count
      function-type/gcc-jit-function-type-*))
  (define ffi:function-type-get-return-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_type_get_return_type"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:function-type-get-return-type/gcc-jit-type-*
           function-type/gcc-jit-function-type-*)
    (ffi:function-type-get-return-type
      function-type/gcc-jit-function-type-*))
  (define ffi:type-dyncast-function-ptr-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_dyncast_function_ptr_type"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-dyncast-function-ptr-type/gcc-jit-function-type-*
           type/gcc-jit-type-*)
    (ffi:type-dyncast-function-ptr-type
      type/gcc-jit-type-*))
  (define ffi:type-is-bool
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_is_bool"
      #:return-type
      int
      #:arg-types
      (list '*)))
  (define (f:type-is-bool/int type/gcc-jit-type-*)
    (ffi:type-is-bool type/gcc-jit-type-*))
  (define ffi:type-dyncast-array
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_dyncast_array"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-dyncast-array/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-dyncast-array type/gcc-jit-type-*))
  (define ffi:function-get-param-count
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_get_param_count"
      #:return-type
      size_t
      #:arg-types
      (list '*)))
  (define (f:function-get-param-count/size-t
           func/gcc-jit-function-*)
    (ffi:function-get-param-count
      func/gcc-jit-function-*))
  (define ffi:function-get-return-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_get_return_type"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:function-get-return-type/gcc-jit-type-*
           func/gcc-jit-function-*)
    (ffi:function-get-return-type
      func/gcc-jit-function-*))
  (define ffi:context-add-top-level-asm
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_add_top_level_asm"
      #:return-type
      void
      #:arg-types
      (list '* '* '*)))
  (define (f:context-add-top-level-asm/void
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           asm-stmts/const-char-*)
    (ffi:context-add-top-level-asm
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      asm-stmts/const-char-*))
  (define ffi:extended-asm-add-clobber
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_extended_asm_add_clobber"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:extended-asm-add-clobber/void
           ext-asm/gcc-jit-extended-asm-*
           victim/const-char-*)
    (ffi:extended-asm-add-clobber
      ext-asm/gcc-jit-extended-asm-*
      victim/const-char-*))
  (define ffi:extended-asm-add-input-operand
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_extended_asm_add_input_operand"
      #:return-type
      void
      #:arg-types
      (list '* '* '* '*)))
  (define (f:extended-asm-add-input-operand/void
           ext-asm/gcc-jit-extended-asm-*
           asm-symbolic-name/const-char-*
           constraint/const-char-*
           src/gcc-jit-rvalue-*)
    (ffi:extended-asm-add-input-operand
      ext-asm/gcc-jit-extended-asm-*
      asm-symbolic-name/const-char-*
      constraint/const-char-*
      src/gcc-jit-rvalue-*))
  (define ffi:extended-asm-add-output-operand
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_extended_asm_add_output_operand"
      #:return-type
      void
      #:arg-types
      (list '* '* '* '*)))
  (define (f:extended-asm-add-output-operand/void
           ext-asm/gcc-jit-extended-asm-*
           asm-symbolic-name/const-char-*
           constraint/const-char-*
           dest/gcc-jit-lvalue-*)
    (ffi:extended-asm-add-output-operand
      ext-asm/gcc-jit-extended-asm-*
      asm-symbolic-name/const-char-*
      constraint/const-char-*
      dest/gcc-jit-lvalue-*))
  (define ffi:extended-asm-set-inline-flag
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_extended_asm_set_inline_flag"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:extended-asm-set-inline-flag/void
           ext-asm/gcc-jit-extended-asm-*
           flag/int)
    (ffi:extended-asm-set-inline-flag
      ext-asm/gcc-jit-extended-asm-*
      flag/int))
  (define ffi:extended-asm-set-volatile-flag
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_extended_asm_set_volatile_flag"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:extended-asm-set-volatile-flag/void
           ext-asm/gcc-jit-extended-asm-*
           flag/int)
    (ffi:extended-asm-set-volatile-flag
      ext-asm/gcc-jit-extended-asm-*
      flag/int))
  (define ffi:extended-asm-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_extended_asm_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:extended-asm-as-object/gcc-jit-object-*
           ext-asm/gcc-jit-extended-asm-*)
    (ffi:extended-asm-as-object
      ext-asm/gcc-jit-extended-asm-*))
  (define ffi:block-end-with-extended-asm-goto
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_end_with_extended_asm_goto"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '* '*)))
  (define (f:block-end-with-extended-asm-goto/gcc-jit-extended-asm-*
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           asm-template/const-char-*
           num-goto-blocks/int
           goto-blocks/gcc-jit-block-**
           fallthrough-block/gcc-jit-block-*)
    (ffi:block-end-with-extended-asm-goto
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      asm-template/const-char-*
      num-goto-blocks/int
      goto-blocks/gcc-jit-block-**
      fallthrough-block/gcc-jit-block-*))
  (define ffi:block-add-extended-asm
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_add_extended_asm"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:block-add-extended-asm/gcc-jit-extended-asm-*
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           asm-template/const-char-*)
    (ffi:block-add-extended-asm
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      asm-template/const-char-*))
  (define ffi:version-patchlevel
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_version_patchlevel"
      #:return-type
      int
      #:arg-types
      (list)))
  (define (f:version-patchlevel/int)
    (ffi:version-patchlevel))
  (define ffi:version-minor
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_version_minor"
      #:return-type
      int
      #:arg-types
      (list)))
  (define (f:version-minor/int)
    (ffi:version-minor))
  (define ffi:version-major
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_version_major"
      #:return-type
      int
      #:arg-types
      (list)))
  (define (f:version-major/int)
    (ffi:version-major))
  (define ffi:context-new-rvalue-from-vector
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_rvalue_from_vector"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* size_t '*)))
  (define (f:context-new-rvalue-from-vector/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           vec-type/gcc-jit-type-*
           num-elements/size-t
           elements/gcc-jit-rvalue-**)
    (ffi:context-new-rvalue-from-vector
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      vec-type/gcc-jit-type-*
      num-elements/size-t
      elements/gcc-jit-rvalue-**))
  (define ffi:function-get-address
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_get_address"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:function-get-address/gcc-jit-rvalue-*
           fn/gcc-jit-function-*
           loc/gcc-jit-location-*)
    (ffi:function-get-address
      fn/gcc-jit-function-*
      loc/gcc-jit-location-*))
  (define ffi:type-get-vector
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_vector"
      #:return-type
      '*
      #:arg-types
      (list '* size_t)))
  (define (f:type-get-vector/gcc-jit-type-*
           type/gcc-jit-type-*
           num-units/size-t)
    (ffi:type-get-vector
      type/gcc-jit-type-*
      num-units/size-t))
  (define ffi:type-get-aligned
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_aligned"
      #:return-type
      '*
      #:arg-types
      (list '* size_t)))
  (define (f:type-get-aligned/gcc-jit-type-*
           type/gcc-jit-type-*
           alignment-in-bytes/size-t)
    (ffi:type-get-aligned
      type/gcc-jit-type-*
      alignment-in-bytes/size-t))
  (define ffi:rvalue-set-bool-require-tail-call
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_rvalue_set_bool_require_tail_call"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:rvalue-set-bool-require-tail-call/void
           call/gcc-jit-rvalue-*
           require-tail-call/int)
    (ffi:rvalue-set-bool-require-tail-call
      call/gcc-jit-rvalue-*
      require-tail-call/int))
  (define ffi:timer-print
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_timer_print"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:timer-print/void
           timer/gcc-jit-timer-*
           f-out/file-*)
    (ffi:timer-print
      timer/gcc-jit-timer-*
      f-out/file-*))
  (define ffi:timer-pop
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_timer_pop"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:timer-pop/void
           timer/gcc-jit-timer-*
           item-name/const-char-*)
    (ffi:timer-pop
      timer/gcc-jit-timer-*
      item-name/const-char-*))
  (define ffi:timer-push
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_timer_push"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:timer-push/void
           timer/gcc-jit-timer-*
           item-name/const-char-*)
    (ffi:timer-push
      timer/gcc-jit-timer-*
      item-name/const-char-*))
  (define ffi:context-get-timer
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_timer"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:context-get-timer/gcc-jit-timer-*
           ctxt/gcc-jit-context-*)
    (ffi:context-get-timer ctxt/gcc-jit-context-*))
  (define ffi:context-set-timer
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_timer"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:context-set-timer/void
           ctxt/gcc-jit-context-*
           timer/gcc-jit-timer-*)
    (ffi:context-set-timer
      ctxt/gcc-jit-context-*
      timer/gcc-jit-timer-*))
  (define ffi:timer-release
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_timer_release"
      #:return-type
      void
      #:arg-types
      (list '*)))
  (define (f:timer-release/void timer/gcc-jit-timer-*)
    (ffi:timer-release timer/gcc-jit-timer-*))
  (define ffi:timer-new
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_timer_new"
      #:return-type
      '*
      #:arg-types
      (list)))
  (define (f:timer-new/gcc-jit-timer-*)
    (ffi:timer-new))
  (define ffi:context-enable-dump
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_enable_dump"
      #:return-type
      void
      #:arg-types
      (list '* '* '*)))
  (define (f:context-enable-dump/void
           ctxt/gcc-jit-context-*
           dumpname/const-char-*
           out-ptr/char-**)
    (ffi:context-enable-dump
      ctxt/gcc-jit-context-*
      dumpname/const-char-*
      out-ptr/char-**))
  (define ffi:context-dump-reproducer-to-file
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_dump_reproducer_to_file"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:context-dump-reproducer-to-file/void
           ctxt/gcc-jit-context-*
           path/const-char-*)
    (ffi:context-dump-reproducer-to-file
      ctxt/gcc-jit-context-*
      path/const-char-*))
  (define ffi:context-new-child-context
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_child_context"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:context-new-child-context/gcc-jit-context-*
           parent-ctxt/gcc-jit-context-*)
    (ffi:context-new-child-context
      parent-ctxt/gcc-jit-context-*))
  (define ffi:block-end-with-switch
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_end_with_switch"
      #:return-type
      void
      #:arg-types
      (list '* '* '* '* int '*)))
  (define (f:block-end-with-switch/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           expr/gcc-jit-rvalue-*
           default-block/gcc-jit-block-*
           num-cases/int
           cases/gcc-jit-case-**)
    (ffi:block-end-with-switch
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      expr/gcc-jit-rvalue-*
      default-block/gcc-jit-block-*
      num-cases/int
      cases/gcc-jit-case-**))
  (define ffi:case-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_case_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:case-as-object/gcc-jit-object-*
           case-/gcc-jit-case-*)
    (ffi:case-as-object case-/gcc-jit-case-*))
  (define ffi:context-new-case
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_case"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-case/gcc-jit-case-*
           ctxt/gcc-jit-context-*
           min-value/gcc-jit-rvalue-*
           max-value/gcc-jit-rvalue-*
           dest-block/gcc-jit-block-*)
    (ffi:context-new-case
      ctxt/gcc-jit-context-*
      min-value/gcc-jit-rvalue-*
      max-value/gcc-jit-rvalue-*
      dest-block/gcc-jit-block-*))
  (define ffi:block-end-with-void-return
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_end_with_void_return"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:block-end-with-void-return/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*)
    (ffi:block-end-with-void-return
      block/gcc-jit-block-*
      loc/gcc-jit-location-*))
  (define ffi:block-end-with-return
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_end_with_return"
      #:return-type
      void
      #:arg-types
      (list '* '* '*)))
  (define (f:block-end-with-return/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           rvalue/gcc-jit-rvalue-*)
    (ffi:block-end-with-return
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      rvalue/gcc-jit-rvalue-*))
  (define ffi:block-end-with-jump
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_end_with_jump"
      #:return-type
      void
      #:arg-types
      (list '* '* '*)))
  (define (f:block-end-with-jump/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           target/gcc-jit-block-*)
    (ffi:block-end-with-jump
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      target/gcc-jit-block-*))
  (define ffi:block-end-with-conditional
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_end_with_conditional"
      #:return-type
      void
      #:arg-types
      (list '* '* '* '* '*)))
  (define (f:block-end-with-conditional/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           boolval/gcc-jit-rvalue-*
           on-true/gcc-jit-block-*
           on-false/gcc-jit-block-*)
    (ffi:block-end-with-conditional
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      boolval/gcc-jit-rvalue-*
      on-true/gcc-jit-block-*
      on-false/gcc-jit-block-*))
  (define ffi:block-add-comment
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_add_comment"
      #:return-type
      void
      #:arg-types
      (list '* '* '*)))
  (define (f:block-add-comment/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           text/const-char-*)
    (ffi:block-add-comment
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      text/const-char-*))
  (define ffi:block-add-assignment-op
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_add_assignment_op"
      #:return-type
      void
      #:arg-types
      (list '* '* '* int '*)))
  (define (f:block-add-assignment-op/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           lvalue/gcc-jit-lvalue-*
           op/enum-gcc-jit-binary-op
           rvalue/gcc-jit-rvalue-*)
    (ffi:block-add-assignment-op
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      lvalue/gcc-jit-lvalue-*
      op/enum-gcc-jit-binary-op
      rvalue/gcc-jit-rvalue-*))
  (define ffi:block-add-assignment
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_add_assignment"
      #:return-type
      void
      #:arg-types
      (list '* '* '* '*)))
  (define (f:block-add-assignment/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           lvalue/gcc-jit-lvalue-*
           rvalue/gcc-jit-rvalue-*)
    (ffi:block-add-assignment
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      lvalue/gcc-jit-lvalue-*
      rvalue/gcc-jit-rvalue-*))
  (define ffi:block-add-eval
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_add_eval"
      #:return-type
      void
      #:arg-types
      (list '* '* '*)))
  (define (f:block-add-eval/void
           block/gcc-jit-block-*
           loc/gcc-jit-location-*
           rvalue/gcc-jit-rvalue-*)
    (ffi:block-add-eval
      block/gcc-jit-block-*
      loc/gcc-jit-location-*
      rvalue/gcc-jit-rvalue-*))
  (define ffi:function-new-temp
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_new_temp"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:function-new-temp/gcc-jit-lvalue-*
           func/gcc-jit-function-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*)
    (ffi:function-new-temp
      func/gcc-jit-function-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*))
  (define ffi:function-new-local
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_new_local"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:function-new-local/gcc-jit-lvalue-*
           func/gcc-jit-function-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           name/const-char-*)
    (ffi:function-new-local
      func/gcc-jit-function-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      name/const-char-*))
  '(gcc-jit-lib "gcc_jit_lvalue_set_register_name")
  '()
  (define ffi:lvalue-set-link-section
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_set_link_section"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:lvalue-set-link-section/void
           lvalue/gcc-jit-lvalue-*
           section-name/const-char-*)
    (ffi:lvalue-set-link-section
      lvalue/gcc-jit-lvalue-*
      section-name/const-char-*))
  (define ffi:lvalue-set-tls-model
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_set_tls_model"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:lvalue-set-tls-model/void
           lvalue/gcc-jit-lvalue-*
           model/enum-gcc-jit-tls-model)
    (ffi:lvalue-set-tls-model
      lvalue/gcc-jit-lvalue-*
      model/enum-gcc-jit-tls-model))
  (define ffi:lvalue-get-address
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_get_address"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:lvalue-get-address/gcc-jit-rvalue-*
           lvalue/gcc-jit-lvalue-*
           loc/gcc-jit-location-*)
    (ffi:lvalue-get-address
      lvalue/gcc-jit-lvalue-*
      loc/gcc-jit-location-*))
  (define ffi:rvalue-dereference
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_rvalue_dereference"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:rvalue-dereference/gcc-jit-lvalue-*
           rvalue/gcc-jit-rvalue-*
           loc/gcc-jit-location-*)
    (ffi:rvalue-dereference
      rvalue/gcc-jit-rvalue-*
      loc/gcc-jit-location-*))
  (define ffi:rvalue-dereference-field
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_rvalue_dereference_field"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:rvalue-dereference-field/gcc-jit-lvalue-*
           ptr/gcc-jit-rvalue-*
           loc/gcc-jit-location-*
           field/gcc-jit-field-*)
    (ffi:rvalue-dereference-field
      ptr/gcc-jit-rvalue-*
      loc/gcc-jit-location-*
      field/gcc-jit-field-*))
  (define ffi:rvalue-access-field
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_rvalue_access_field"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:rvalue-access-field/gcc-jit-rvalue-*
           struct-or-union/gcc-jit-rvalue-*
           loc/gcc-jit-location-*
           field/gcc-jit-field-*)
    (ffi:rvalue-access-field
      struct-or-union/gcc-jit-rvalue-*
      loc/gcc-jit-location-*
      field/gcc-jit-field-*))
  (define ffi:lvalue-access-field
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_access_field"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:lvalue-access-field/gcc-jit-lvalue-*
           struct-or-union/gcc-jit-lvalue-*
           loc/gcc-jit-location-*
           field/gcc-jit-field-*)
    (ffi:lvalue-access-field
      struct-or-union/gcc-jit-lvalue-*
      loc/gcc-jit-location-*
      field/gcc-jit-field-*))
  (define ffi:context-new-vector-access
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_vector_access"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-vector-access/gcc-jit-lvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           vector/gcc-jit-rvalue-*
           index/gcc-jit-rvalue-*)
    (ffi:context-new-vector-access
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      vector/gcc-jit-rvalue-*
      index/gcc-jit-rvalue-*))
  (define ffi:context-new-rvalue-vector-perm
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_rvalue_vector_perm"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '* '*)))
  (define (f:context-new-rvalue-vector-perm/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           elements1/gcc-jit-rvalue-*
           elements2/gcc-jit-rvalue-*
           mask/gcc-jit-rvalue-*)
    (ffi:context-new-rvalue-vector-perm
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      elements1/gcc-jit-rvalue-*
      elements2/gcc-jit-rvalue-*
      mask/gcc-jit-rvalue-*))
  (define ffi:context-convert-vector
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_convert_vector"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-convert-vector/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           vector/gcc-jit-rvalue-*
           type/gcc-jit-type-*)
    (ffi:context-convert-vector
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      vector/gcc-jit-rvalue-*
      type/gcc-jit-type-*))
  (define ffi:context-new-array-access
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_array_access"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-array-access/gcc-jit-lvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           ptr/gcc-jit-rvalue-*
           index/gcc-jit-rvalue-*)
    (ffi:context-new-array-access
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      ptr/gcc-jit-rvalue-*
      index/gcc-jit-rvalue-*))
  (define ffi:lvalue-get-alignment
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_get_alignment"
      #:return-type
      unsigned-int
      #:arg-types
      (list '*)))
  (define (f:lvalue-get-alignment/unsigned-int
           lvalue/gcc-jit-lvalue-*)
    (ffi:lvalue-get-alignment
      lvalue/gcc-jit-lvalue-*))
  (define ffi:lvalue-set-alignment
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_set_alignment"
      #:return-type
      void
      #:arg-types
      (list '* unsigned-int)))
  (define (f:lvalue-set-alignment/void
           lvalue/gcc-jit-lvalue-*
           bytes/unsigned-int)
    (ffi:lvalue-set-alignment
      lvalue/gcc-jit-lvalue-*
      bytes/unsigned-int))
  (define ffi:context-new-bitcast
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_bitcast"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-bitcast/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           rvalue/gcc-jit-rvalue-*
           type/gcc-jit-type-*)
    (ffi:context-new-bitcast
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      rvalue/gcc-jit-rvalue-*
      type/gcc-jit-type-*))
  (define ffi:context-new-cast
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_cast"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-cast/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           rvalue/gcc-jit-rvalue-*
           type/gcc-jit-type-*)
    (ffi:context-new-cast
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      rvalue/gcc-jit-rvalue-*
      type/gcc-jit-type-*))
  (define ffi:context-new-call-through-ptr
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_call_through_ptr"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '*)))
  (define (f:context-new-call-through-ptr/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           fn-ptr/gcc-jit-rvalue-*
           numargs/int
           args/gcc-jit-rvalue-**)
    (ffi:context-new-call-through-ptr
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      fn-ptr/gcc-jit-rvalue-*
      numargs/int
      args/gcc-jit-rvalue-**))
  (define ffi:context-new-call
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_call"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '*)))
  (define (f:context-new-call/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           func/gcc-jit-function-*
           numargs/int
           args/gcc-jit-rvalue-**)
    (ffi:context-new-call
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      func/gcc-jit-function-*
      numargs/int
      args/gcc-jit-rvalue-**))
  (define ffi:context-new-comparison
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_comparison"
      #:return-type
      '*
      #:arg-types
      (list '* '* int '* '*)))
  (define (f:context-new-comparison/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           op/enum-gcc-jit-comparison
           a/gcc-jit-rvalue-*
           b/gcc-jit-rvalue-*)
    (ffi:context-new-comparison
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      op/enum-gcc-jit-comparison
      a/gcc-jit-rvalue-*
      b/gcc-jit-rvalue-*))
  (define enum:comparison/alist
    '((= . 0)
      (!= . 1)
      (< . 2)
      (<= . 3)
      (> . 4)
      (>= . 5)))
  (define enum:comparison/sym->int
    (alist->hashq-table enum:comparison/alist))
  (define enum:comparison/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:comparison/alist)))
  (define (#{% enum:comparison?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:comparison/sym->int a)))))
  (define ffi:context-new-binary-op
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_binary_op"
      #:return-type
      '*
      #:arg-types
      (list '* '* int '* '* '*)))
  (define (f:context-new-binary-op/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           op/enum-gcc-jit-binary-op
           result-type/gcc-jit-type-*
           a/gcc-jit-rvalue-*
           b/gcc-jit-rvalue-*)
    (ffi:context-new-binary-op
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      op/enum-gcc-jit-binary-op
      result-type/gcc-jit-type-*
      a/gcc-jit-rvalue-*
      b/gcc-jit-rvalue-*))
  (define enum:binary-op/alist
    '((+ . 0)
      (- . 1)
      (* . 2)
      (/ . 3)
      (% . 4)
      (& . 5)
      (^ . 6)
      (bor . 7)
      (and . 8)
      (or . 9)
      (<< . 10)
      (>> . 11)))
  (define enum:binary-op/sym->int
    (alist->hashq-table enum:binary-op/alist))
  (define enum:binary-op/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:binary-op/alist)))
  (define (#{% enum:binary-op?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:binary-op/sym->int a)))))
  (define ffi:context-new-unary-op
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_unary_op"
      #:return-type
      '*
      #:arg-types
      (list '* '* int '* '*)))
  (define (f:context-new-unary-op/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           op/enum-gcc-jit-unary-op
           result-type/gcc-jit-type-*
           rvalue/gcc-jit-rvalue-*)
    (ffi:context-new-unary-op
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      op/enum-gcc-jit-unary-op
      result-type/gcc-jit-type-*
      rvalue/gcc-jit-rvalue-*))
  (define enum:unary-op/alist
    '((- . 0)
      (bitwise-negate . 1)
      (logical-negate . 2)
      (abs . 3)))
  (define enum:unary-op/sym->int
    (alist->hashq-table enum:unary-op/alist))
  (define enum:unary-op/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:unary-op/alist)))
  (define (#{% enum:unary-op?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:unary-op/sym->int a)))))
  (define ffi:context-new-string-literal
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_string_literal"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-new-string-literal/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           value/const-char-*)
    (ffi:context-new-string-literal
      ctxt/gcc-jit-context-*
      value/const-char-*))
  (define ffi:context-new-alignof
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_alignof"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-new-alignof/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           type/gcc-jit-type-*)
    (ffi:context-new-alignof
      ctxt/gcc-jit-context-*
      type/gcc-jit-type-*))
  (define ffi:context-new-sizeof
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_sizeof"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-new-sizeof/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           type/gcc-jit-type-*)
    (ffi:context-new-sizeof
      ctxt/gcc-jit-context-*
      type/gcc-jit-type-*))
  (define ffi:context-null
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_null"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-null/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           pointer-type/gcc-jit-type-*)
    (ffi:context-null
      ctxt/gcc-jit-context-*
      pointer-type/gcc-jit-type-*))
  (define ffi:context-new-rvalue-from-ptr
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_rvalue_from_ptr"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:context-new-rvalue-from-ptr/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           pointer-type/gcc-jit-type-*
           value/void-*)
    (ffi:context-new-rvalue-from-ptr
      ctxt/gcc-jit-context-*
      pointer-type/gcc-jit-type-*
      value/void-*))
  (define ffi:context-new-rvalue-from-double
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_rvalue_from_double"
      #:return-type
      '*
      #:arg-types
      (list '* '* double)))
  (define (f:context-new-rvalue-from-double/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           numeric-type/gcc-jit-type-*
           value/double)
    (ffi:context-new-rvalue-from-double
      ctxt/gcc-jit-context-*
      numeric-type/gcc-jit-type-*
      value/double))
  (define ffi:context-one
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_one"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-one/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           numeric-type/gcc-jit-type-*)
    (ffi:context-one
      ctxt/gcc-jit-context-*
      numeric-type/gcc-jit-type-*))
  (define ffi:context-zero
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_zero"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-zero/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           numeric-type/gcc-jit-type-*)
    (ffi:context-zero
      ctxt/gcc-jit-context-*
      numeric-type/gcc-jit-type-*))
  (define ffi:context-new-rvalue-from-long
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_rvalue_from_long"
      #:return-type
      '*
      #:arg-types
      (list '* '* long)))
  (define (f:context-new-rvalue-from-long/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           numeric-type/gcc-jit-type-*
           value/long)
    (ffi:context-new-rvalue-from-long
      ctxt/gcc-jit-context-*
      numeric-type/gcc-jit-type-*
      value/long))
  (define ffi:context-new-rvalue-from-int
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_rvalue_from_int"
      #:return-type
      '*
      #:arg-types
      (list '* '* int)))
  (define (f:context-new-rvalue-from-int/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           numeric-type/gcc-jit-type-*
           value/int)
    (ffi:context-new-rvalue-from-int
      ctxt/gcc-jit-context-*
      numeric-type/gcc-jit-type-*
      value/int))
  (define ffi:rvalue-get-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_rvalue_get_type"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:rvalue-get-type/gcc-jit-type-*
           rvalue/gcc-jit-rvalue-*)
    (ffi:rvalue-get-type rvalue/gcc-jit-rvalue-*))
  (define ffi:rvalue-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_rvalue_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:rvalue-as-object/gcc-jit-object-*
           rvalue/gcc-jit-rvalue-*)
    (ffi:rvalue-as-object rvalue/gcc-jit-rvalue-*))
  (define ffi:lvalue-as-rvalue
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_as_rvalue"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:lvalue-as-rvalue/gcc-jit-rvalue-*
           lvalue/gcc-jit-lvalue-*)
    (ffi:lvalue-as-rvalue lvalue/gcc-jit-lvalue-*))
  (define ffi:lvalue-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_lvalue_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:lvalue-as-object/gcc-jit-object-*
           lvalue/gcc-jit-lvalue-*)
    (ffi:lvalue-as-object lvalue/gcc-jit-lvalue-*))
  (define ffi:global-set-readonly
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_global_set_readonly"
      #:return-type
      void
      #:arg-types
      (list '*)))
  (define (f:global-set-readonly/void
           global/gcc-jit-lvalue-*)
    (ffi:global-set-readonly global/gcc-jit-lvalue-*))
  (define ffi:global-set-initializer
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_global_set_initializer"
      #:return-type
      '*
      #:arg-types
      (list '* '* size_t)))
  (define (f:global-set-initializer/gcc-jit-lvalue-*
           global/gcc-jit-lvalue-*
           blob/const-void-*
           num-bytes/size-t)
    (ffi:global-set-initializer
      global/gcc-jit-lvalue-*
      blob/const-void-*
      num-bytes/size-t))
  (define ffi:context-get-target-builtin-function
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_target_builtin_function"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-get-target-builtin-function/gcc-jit-function-*
           ctxt/gcc-jit-context-*
           name/const-char-*)
    (ffi:context-get-target-builtin-function
      ctxt/gcc-jit-context-*
      name/const-char-*))
  (define ffi:global-set-initializer-rvalue
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_global_set_initializer_rvalue"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:global-set-initializer-rvalue/gcc-jit-lvalue-*
           global/gcc-jit-lvalue-*
           init-value/gcc-jit-rvalue-*)
    (ffi:global-set-initializer-rvalue
      global/gcc-jit-lvalue-*
      init-value/gcc-jit-rvalue-*))
  (define ffi:context-new-array-constructor
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_array_constructor"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* size_t '*)))
  (define (f:context-new-array-constructor/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           num-values/size-t
           values/gcc-jit-rvalue-**)
    (ffi:context-new-array-constructor
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      num-values/size-t
      values/gcc-jit-rvalue-**))
  (define ffi:context-new-union-constructor
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_union_constructor"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '* '*)))
  (define (f:context-new-union-constructor/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           field/gcc-jit-field-*
           value/gcc-jit-rvalue-*)
    (ffi:context-new-union-constructor
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      field/gcc-jit-field-*
      value/gcc-jit-rvalue-*))
  (define ffi:context-new-struct-constructor
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_struct_constructor"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* size_t '* '*)))
  (define (f:context-new-struct-constructor/gcc-jit-rvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           num-values/size-t
           fields/gcc-jit-field-**
           values/gcc-jit-rvalue-**)
    (ffi:context-new-struct-constructor
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      num-values/size-t
      fields/gcc-jit-field-**
      values/gcc-jit-rvalue-**))
  (define ffi:context-new-global
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_global"
      #:return-type
      '*
      #:arg-types
      (list '* '* int '* '*)))
  (define (f:context-new-global/gcc-jit-lvalue-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           kind/enum-gcc-jit-global-kind
           type/gcc-jit-type-*
           name/const-char-*)
    (ffi:context-new-global
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      kind/enum-gcc-jit-global-kind
      type/gcc-jit-type-*
      name/const-char-*))
  (define enum:global-kind/alist
    '((exported . 0) (internal . 1) (imported . 2)))
  (define enum:global-kind/sym->int
    (alist->hashq-table enum:global-kind/alist))
  (define enum:global-kind/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:global-kind/alist)))
  (define (#{% enum:global-kind?-procedure}# a)
    (and (symbol? a)
         (not (boolean?
                (hashq-ref enum:global-kind/sym->int a)))))
  (define ffi:block-get-function
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_get_function"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:block-get-function/gcc-jit-function-*
           block/gcc-jit-block-*)
    (ffi:block-get-function block/gcc-jit-block-*))
  (define ffi:block-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_block_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:block-as-object/gcc-jit-object-*
           block/gcc-jit-block-*)
    (ffi:block-as-object block/gcc-jit-block-*))
  (define ffi:function-new-block
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_new_block"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:function-new-block/gcc-jit-block-*
           func/gcc-jit-function-*
           name/const-char-*)
    (ffi:function-new-block
      func/gcc-jit-function-*
      name/const-char-*))
  (define ffi:function-dump-to-dot
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_dump_to_dot"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:function-dump-to-dot/void
           func/gcc-jit-function-*
           path/const-char-*)
    (ffi:function-dump-to-dot
      func/gcc-jit-function-*
      path/const-char-*))
  (define ffi:function-get-param
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_get_param"
      #:return-type
      '*
      #:arg-types
      (list '* int)))
  (define (f:function-get-param/gcc-jit-param-*
           func/gcc-jit-function-*
           index/int)
    (ffi:function-get-param
      func/gcc-jit-function-*
      index/int))
  (define ffi:function-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_function_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:function-as-object/gcc-jit-object-*
           func/gcc-jit-function-*)
    (ffi:function-as-object func/gcc-jit-function-*))
  (define ffi:context-get-builtin-function
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_builtin_function"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:context-get-builtin-function/gcc-jit-function-*
           ctxt/gcc-jit-context-*
           name/const-char-*)
    (ffi:context-get-builtin-function
      ctxt/gcc-jit-context-*
      name/const-char-*))
  (define ffi:context-new-function
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_function"
      #:return-type
      '*
      #:arg-types
      (list '* '* int '* '* int '* int)))
  (define (f:context-new-function/gcc-jit-function-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           kind/enum-gcc-jit-function-kind
           return-type/gcc-jit-type-*
           name/const-char-*
           num-params/int
           params/gcc-jit-param-**
           is-variadic/int)
    (ffi:context-new-function
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      kind/enum-gcc-jit-function-kind
      return-type/gcc-jit-type-*
      name/const-char-*
      num-params/int
      params/gcc-jit-param-**
      is-variadic/int))
  (define enum:tls-model/alist
    '((none . 0)
      (global-dynamic . 1)
      (local-dynamic . 2)
      (initial-exec . 3)
      (local-exec . 4)))
  (define enum:tls-model/sym->int
    (alist->hashq-table enum:tls-model/alist))
  (define enum:tls-model/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:tls-model/alist)))
  (define (#{% enum:tls-model?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:tls-model/sym->int a)))))
  (define enum:function-kind/alist
    '((exported . 0)
      (internal . 1)
      (imported . 2)
      (always-inline . 3)))
  (define enum:function-kind/sym->int
    (alist->hashq-table enum:function-kind/alist))
  (define enum:function-kind/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:function-kind/alist)))
  (define (#{% enum:function-kind?-procedure}# a)
    (and (symbol? a)
         (not (boolean?
                (hashq-ref enum:function-kind/sym->int a)))))
  (define ffi:param-as-rvalue
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_param_as_rvalue"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:param-as-rvalue/gcc-jit-rvalue-*
           param/gcc-jit-param-*)
    (ffi:param-as-rvalue param/gcc-jit-param-*))
  (define ffi:param-as-lvalue
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_param_as_lvalue"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:param-as-lvalue/gcc-jit-lvalue-*
           param/gcc-jit-param-*)
    (ffi:param-as-lvalue param/gcc-jit-param-*))
  (define ffi:param-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_param_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:param-as-object/gcc-jit-object-*
           param/gcc-jit-param-*)
    (ffi:param-as-object param/gcc-jit-param-*))
  (define ffi:context-new-param
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_param"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-param/gcc-jit-param-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           name/const-char-*)
    (ffi:context-new-param
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      name/const-char-*))
  (define ffi:context-new-function-ptr-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_function_ptr_type"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '* int)))
  (define (f:context-new-function-ptr-type/gcc-jit-type-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           return-type/gcc-jit-type-*
           num-params/int
           param-types/gcc-jit-type-**
           is-variadic/int)
    (ffi:context-new-function-ptr-type
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      return-type/gcc-jit-type-*
      num-params/int
      param-types/gcc-jit-type-**
      is-variadic/int))
  (define ffi:context-new-union-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_union_type"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '*)))
  (define (f:context-new-union-type/gcc-jit-type-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           name/const-char-*
           num-fields/int
           fields/gcc-jit-field-**)
    (ffi:context-new-union-type
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      name/const-char-*
      num-fields/int
      fields/gcc-jit-field-**))
  (define ffi:struct-get-field-count
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_struct_get_field_count"
      #:return-type
      size_t
      #:arg-types
      (list '*)))
  (define (f:struct-get-field-count/size-t
           struct-type/gcc-jit-struct-*)
    (ffi:struct-get-field-count
      struct-type/gcc-jit-struct-*))
  (define ffi:struct-get-field
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_struct_get_field"
      #:return-type
      '*
      #:arg-types
      (list '* size_t)))
  (define (f:struct-get-field/gcc-jit-field-*
           struct-type/gcc-jit-struct-*
           index/size-t)
    (ffi:struct-get-field
      struct-type/gcc-jit-struct-*
      index/size-t))
  (define ffi:struct-set-fields
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_struct_set_fields"
      #:return-type
      void
      #:arg-types
      (list '* '* int '*)))
  (define (f:struct-set-fields/void
           struct-type/gcc-jit-struct-*
           loc/gcc-jit-location-*
           num-fields/int
           fields/gcc-jit-field-**)
    (ffi:struct-set-fields
      struct-type/gcc-jit-struct-*
      loc/gcc-jit-location-*
      num-fields/int
      fields/gcc-jit-field-**))
  (define ffi:struct-as-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_struct_as_type"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:struct-as-type/gcc-jit-type-*
           struct-type/gcc-jit-struct-*)
    (ffi:struct-as-type struct-type/gcc-jit-struct-*))
  (define ffi:context-new-opaque-struct
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_opaque_struct"
      #:return-type
      '*
      #:arg-types
      (list '* '* '*)))
  (define (f:context-new-opaque-struct/gcc-jit-struct-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           name/const-char-*)
    (ffi:context-new-opaque-struct
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      name/const-char-*))
  (define ffi:context-new-struct-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_struct_type"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '*)))
  (define (f:context-new-struct-type/gcc-jit-struct-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           name/const-char-*
           num-fields/int
           fields/gcc-jit-field-**)
    (ffi:context-new-struct-type
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      name/const-char-*
      num-fields/int
      fields/gcc-jit-field-**))
  (define ffi:field-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_field_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:field-as-object/gcc-jit-object-*
           field/gcc-jit-field-*)
    (ffi:field-as-object field/gcc-jit-field-*))
  (define ffi:context-new-bitfield
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_bitfield"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int '*)))
  (define (f:context-new-bitfield/gcc-jit-field-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           width/int
           name/const-char-*)
    (ffi:context-new-bitfield
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      width/int
      name/const-char-*))
  (define ffi:context-new-field
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_field"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* '*)))
  (define (f:context-new-field/gcc-jit-field-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           type/gcc-jit-type-*
           name/const-char-*)
    (ffi:context-new-field
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      type/gcc-jit-type-*
      name/const-char-*))
  (define ffi:context-new-array-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_array_type"
      #:return-type
      '*
      #:arg-types
      (list '* '* '* int)))
  (define (f:context-new-array-type/gcc-jit-type-*
           ctxt/gcc-jit-context-*
           loc/gcc-jit-location-*
           element-type/gcc-jit-type-*
           num-elements/int)
    (ffi:context-new-array-type
      ctxt/gcc-jit-context-*
      loc/gcc-jit-location-*
      element-type/gcc-jit-type-*
      num-elements/int))
  (define ffi:type-get-size
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_size"
      #:return-type
      ssize_t
      #:arg-types
      (list '*)))
  (define (f:type-get-size/ssize-t type/gcc-jit-type-*)
    (ffi:type-get-size type/gcc-jit-type-*))
  (define ffi:compatible-types
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_compatible_types"
      #:return-type
      int
      #:arg-types
      (list '* '*)))
  (define (f:compatible-types/int
           ltype/gcc-jit-type-*
           rtype/gcc-jit-type-*)
    (ffi:compatible-types
      ltype/gcc-jit-type-*
      rtype/gcc-jit-type-*))
  (define ffi:type-get-restrict
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_restrict"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-get-restrict/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-get-restrict type/gcc-jit-type-*))
  (define ffi:type-get-volatile
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_volatile"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-get-volatile/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-get-volatile type/gcc-jit-type-*))
  (define ffi:type-get-const
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_const"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-get-const/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-get-const type/gcc-jit-type-*))
  (define ffi:type-get-pointer
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_get_pointer"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-get-pointer/gcc-jit-type-*
           type/gcc-jit-type-*)
    (ffi:type-get-pointer type/gcc-jit-type-*))
  (define ffi:context-get-int-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_int_type"
      #:return-type
      '*
      #:arg-types
      (list '* int int)))
  (define (f:context-get-int-type/gcc-jit-type-*
           ctxt/gcc-jit-context-*
           num-bytes/int
           is-signed/int)
    (ffi:context-get-int-type
      ctxt/gcc-jit-context-*
      num-bytes/int
      is-signed/int))
  (define ffi:context-get-type
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_type"
      #:return-type
      '*
      #:arg-types
      (list '* int)))
  (define (f:context-get-type/gcc-jit-type-*
           ctxt/gcc-jit-context-*
           type-/enum-gcc-jit-types)
    (ffi:context-get-type
      ctxt/gcc-jit-context-*
      type-/enum-gcc-jit-types))
  (define enum:types/alist
    '((void . 0)
      (void-ptr . 1)
      (bool . 2)
      (char . 3)
      (signed-char . 4)
      (unsigned-char . 5)
      (short . 6)
      (unsigned-short . 7)
      (int . 8)
      (unsigned-int . 9)
      (long . 10)
      (unsigned-long . 11)
      (long-long . 12)
      (unsigned-long-long . 13)
      (float . 14)
      (double . 15)
      (long-double . 16)
      (const-char-ptr . 17)
      (size-t . 18)
      (file-ptr . 19)
      (complex-float . 20)
      (complex-double . 21)
      (complex-long-double . 22)
      (uint8-t . 23)
      (uint16-t . 24)
      (uint32-t . 25)
      (uint64-t . 26)
      (uint128-t . 27)
      (int8-t . 28)
      (int16-t . 29)
      (int32-t . 30)
      (int64-t . 31)
      (int128-t . 32)
      (bfloat16 . 33)))
  (define enum:types/sym->int
    (alist->hashq-table enum:types/alist))
  (define enum:types/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:types/alist)))
  (define (#{% enum:types?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:types/sym->int a)))))
  (define ffi:type-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_type_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:type-as-object/gcc-jit-object-*
           type/gcc-jit-type-*)
    (ffi:type-as-object type/gcc-jit-type-*))
  (define ffi:location-as-object
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_location_as_object"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:location-as-object/gcc-jit-object-*
           loc/gcc-jit-location-*)
    (ffi:location-as-object loc/gcc-jit-location-*))
  (define ffi:context-new-location
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_new_location"
      #:return-type
      '*
      #:arg-types
      (list '* '* int int)))
  (define (f:context-new-location/gcc-jit-location-*
           ctxt/gcc-jit-context-*
           filename/const-char-*
           line/int
           column/int)
    (ffi:context-new-location
      ctxt/gcc-jit-context-*
      filename/const-char-*
      line/int
      column/int))
  (define ffi:object-get-debug-string
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_object_get_debug_string"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:object-get-debug-string/const-char-*
           obj/gcc-jit-object-*)
    (ffi:object-get-debug-string
      obj/gcc-jit-object-*))
  (define ffi:object-get-context
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_object_get_context"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:object-get-context/gcc-jit-context-*
           obj/gcc-jit-object-*)
    (ffi:object-get-context obj/gcc-jit-object-*))
  (define ffi:result-release
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_result_release"
      #:return-type
      void
      #:arg-types
      (list '*)))
  (define (f:result-release/void result/gcc-jit-result-*)
    (ffi:result-release result/gcc-jit-result-*))
  (define ffi:result-get-global
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_result_get_global"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:result-get-global/void-*
           result/gcc-jit-result-*
           name/const-char-*)
    (ffi:result-get-global
      result/gcc-jit-result-*
      name/const-char-*))
  (define ffi:result-get-code
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_result_get_code"
      #:return-type
      '*
      #:arg-types
      (list '* '*)))
  (define (f:result-get-code/void-*
           result/gcc-jit-result-*
           funcname/const-char-*)
    (ffi:result-get-code
      result/gcc-jit-result-*
      funcname/const-char-*))
  (define ffi:context-get-last-error
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_last_error"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:context-get-last-error/const-char-*
           ctxt/gcc-jit-context-*)
    (ffi:context-get-last-error
      ctxt/gcc-jit-context-*))
  (define ffi:context-get-first-error
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_get_first_error"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:context-get-first-error/const-char-*
           ctxt/gcc-jit-context-*)
    (ffi:context-get-first-error
      ctxt/gcc-jit-context-*))
  (define ffi:context-set-logfile
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_logfile"
      #:return-type
      void
      #:arg-types
      (list '* '* int int)))
  (define (f:context-set-logfile/void
           ctxt/gcc-jit-context-*
           logfile/file-*
           flags/int
           verbosity/int)
    (ffi:context-set-logfile
      ctxt/gcc-jit-context-*
      logfile/file-*
      flags/int
      verbosity/int))
  (define ffi:context-dump-to-file
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_dump_to_file"
      #:return-type
      void
      #:arg-types
      (list '* '* int)))
  (define (f:context-dump-to-file/void
           ctxt/gcc-jit-context-*
           path/const-char-*
           update-locations/int)
    (ffi:context-dump-to-file
      ctxt/gcc-jit-context-*
      path/const-char-*
      update-locations/int))
  (define ffi:context-compile-to-file
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_compile_to_file"
      #:return-type
      void
      #:arg-types
      (list '* int '*)))
  (define (f:context-compile-to-file/void
           ctxt/gcc-jit-context-*
           output-kind/enum-gcc-jit-output-kind
           output-path/const-char-*)
    (ffi:context-compile-to-file
      ctxt/gcc-jit-context-*
      output-kind/enum-gcc-jit-output-kind
      output-path/const-char-*))
  (define enum:output-kind/alist
    '((assembler . 0)
      (object-file . 1)
      (dynamic-library . 2)
      (executable . 3)))
  (define enum:output-kind/sym->int
    (alist->hashq-table enum:output-kind/alist))
  (define enum:output-kind/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:output-kind/alist)))
  (define (#{% enum:output-kind?-procedure}# a)
    (and (symbol? a)
         (not (boolean?
                (hashq-ref enum:output-kind/sym->int a)))))
  (define ffi:context-compile
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_compile"
      #:return-type
      '*
      #:arg-types
      (list '*)))
  (define (f:context-compile/gcc-jit-result-*
           ctxt/gcc-jit-context-*)
    (ffi:context-compile ctxt/gcc-jit-context-*))
  (define ffi:context-add-driver-option
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_add_driver_option"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:context-add-driver-option/void
           ctxt/gcc-jit-context-*
           optname/const-char-*)
    (ffi:context-add-driver-option
      ctxt/gcc-jit-context-*
      optname/const-char-*))
  (define ffi:context-add-command-line-option
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_add_command_line_option"
      #:return-type
      void
      #:arg-types
      (list '* '*)))
  (define (f:context-add-command-line-option/void
           ctxt/gcc-jit-context-*
           optname/const-char-*)
    (ffi:context-add-command-line-option
      ctxt/gcc-jit-context-*
      optname/const-char-*))
  (define ffi:context-set-bool-use-external-driver
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_bool_use_external_driver"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:context-set-bool-use-external-driver/void
           ctxt/gcc-jit-context-*
           bool-value/int)
    (ffi:context-set-bool-use-external-driver
      ctxt/gcc-jit-context-*
      bool-value/int))
  (define ffi:context-set-bool-print-errors-to-stderr
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_bool_print_errors_to_stderr"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:context-set-bool-print-errors-to-stderr/void
           ctxt/gcc-jit-context-*
           enabled/int)
    (ffi:context-set-bool-print-errors-to-stderr
      ctxt/gcc-jit-context-*
      enabled/int))
  (define ffi:context-set-bool-allow-unreachable-blocks
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_bool_allow_unreachable_blocks"
      #:return-type
      void
      #:arg-types
      (list '* int)))
  (define (f:context-set-bool-allow-unreachable-blocks/void
           ctxt/gcc-jit-context-*
           bool-value/int)
    (ffi:context-set-bool-allow-unreachable-blocks
      ctxt/gcc-jit-context-*
      bool-value/int))
  (define ffi:context-set-bool-option
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_bool_option"
      #:return-type
      void
      #:arg-types
      (list '* int int)))
  (define (f:context-set-bool-option/void
           ctxt/gcc-jit-context-*
           opt/enum-gcc-jit-bool-option
           value/int)
    (ffi:context-set-bool-option
      ctxt/gcc-jit-context-*
      opt/enum-gcc-jit-bool-option
      value/int))
  (define ffi:context-set-int-option
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_int_option"
      #:return-type
      void
      #:arg-types
      (list '* int int)))
  (define (f:context-set-int-option/void
           ctxt/gcc-jit-context-*
           opt/enum-gcc-jit-int-option
           value/int)
    (ffi:context-set-int-option
      ctxt/gcc-jit-context-*
      opt/enum-gcc-jit-int-option
      value/int))
  (define ffi:context-set-str-option
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_set_str_option"
      #:return-type
      void
      #:arg-types
      (list '* int '*)))
  (define (f:context-set-str-option/void
           ctxt/gcc-jit-context-*
           opt/enum-gcc-jit-str-option
           value/const-char-*)
    (ffi:context-set-str-option
      ctxt/gcc-jit-context-*
      opt/enum-gcc-jit-str-option
      value/const-char-*))
  (define enum:bool-option/alist
    '((debuginfo . 0)
      (dump-initial-tree . 1)
      (dump-initial-gimple . 2)
      (dump-generated-code . 3)
      (dump-summary . 4)
      (dump-everything . 5)
      (selfcheck-gc . 6)
      (keep-intermediates . 7)))
  (define enum:bool-option/sym->int
    (alist->hashq-table enum:bool-option/alist))
  (define enum:bool-option/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:bool-option/alist)))
  (define (#{% enum:bool-option?-procedure}# a)
    (and (symbol? a)
         (not (boolean?
                (hashq-ref enum:bool-option/sym->int a)))))
  (define enum:int-option/alist
    '((optimization-level . 0)))
  (define enum:int-option/sym->int
    (alist->hashq-table enum:int-option/alist))
  (define enum:int-option/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:int-option/alist)))
  (define (#{% enum:int-option?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:int-option/sym->int a)))))
  (define enum:str-option/alist
    '((progname . 0)
      (special-chars-in-func-names . 1)))
  (define enum:str-option/sym->int
    (alist->hashq-table enum:str-option/alist))
  (define enum:str-option/int->sym
    (alist->hashq-table
      (map (lambda (c) (cons (cdr c) (car c)))
           enum:str-option/alist)))
  (define (#{% enum:str-option?-procedure}# a)
    (and (symbol? a)
         (not (boolean? (hashq-ref enum:str-option/sym->int a)))))
  (define ffi:context-release
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_release"
      #:return-type
      void
      #:arg-types
      (list '*)))
  (define (f:context-release/void ctxt/gcc-jit-context-*)
    (ffi:context-release ctxt/gcc-jit-context-*))
  (define ffi:context-acquire
    (foreign-library-function
      gcc-jit-lib
      "gcc_jit_context_acquire"
      #:return-type
      '*
      #:arg-types
      (list)))
  (define (f:context-acquire/gcc-jit-context-*)
    (ffi:context-acquire)))

(module-export-all! (current-module))
