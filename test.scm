(add-to-load-path ".")

(use-modules (bunyip) (srfi srfi-26) (srfi srfi-1) (ice-9 match))

(define header "")
(define ast (call-clang-for-ast-dump header))

(define enums (find-toplevel-matching-asts ast 'EnumDecl))
(define funcs (find-toplevel-matching-asts ast 'FunctionDecl))
(define type-decls (find-toplevel-matching-asts ast 'TypedefDecl))

(length enums)
(length funcs)
(length type-decls)

(length (filter filter-only-in-file enums))
(length (filter filter-only-in-file funcs))
(length (filter filter-only-in-file type-decls))

(map (λ (ast) (assq-ref ast 'name)) (filter filter-only-in-file type-decls))

(map (λ (ast) (assq-ref ast 'name)) (filter filter-only-in-file funcs))

(map ast-function->simple-alist (filter filter-only-in-file funcs))

(car (filter filter-only-in-file funcs))

(define (filter-only-in-file ast)
  (define loc (assq-ref ast 'loc))
  (if loc
      (let ((exp (assq-ref loc 'expansionLoc)))
        (if exp
            (if (assq-ref exp 'includedFrom)
                #f
                #t)
            (if (assq-ref loc 'includedFrom)
                #f
                #t)))
      #f))


(map (λ (ast) (assq-ref ast 'name)) enums)
(map (λ (ast) (assq-ref ast 'name)) (filter filter-only-in-file enums))

(define all-enums (map (λ (e) (enum-assign-all-values (enum-clean (enum-ast->simple-alist e))))
                       (filter filter-only-in-file enums)))

(set-procedure-property! asdf 'documentation (assq-ref alst 'qual-type))

(define (general-cleaner str)
  (string-map
   (λ (c) (if (eq? c #\_) #\- c))
   (string-trim-both (string-downcase str) #\space)))

(define* (enum-clean alst #:optional #:key
                     (name-cleaner general-cleaner)
                     (field-name-cleaner general-cleaner))
  (define fields (assq-ref alst 'fields))
  (define name (assq-ref alst 'name))
  (define prefix
    (if (every (λ (n) (string-prefix? name (string-downcase n)))
               (map (cut assq-ref <> 'name) fields))
        (+ 1 (string-length (assq-ref alst 'name)))
        (string-length
         (find-longest-prefix/list
          (map (cut assq-ref <> 'name) fields))))
    )

  `((name . ,(name-cleaner (assq-ref alst 'name)))
    (fields . ,(map (λ (field)
                      `((name . ,(field-name-cleaner
                                  (string-drop (assq-ref field 'name)
                                               prefix)))
                        (value . ,(assq-ref field 'value))))
                    fields))))


(define (enum-assign-all-values alst)
  (define val 0)

  `((name . ,(assq-ref alst 'name))
    (fields . ,(map (λ (field)
                      `((name . ,(assq-ref field 'name))
                        (value . ,(let ((v (assq-ref field 'value)))
                                    (if v
                                        (set! val (string->number v))
                                        (set! val (+ 1 val)))
                                    val))))
                    (assq-ref alst 'fields)))))
