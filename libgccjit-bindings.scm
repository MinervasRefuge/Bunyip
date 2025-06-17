;; libgccjit-bindings.scm : Bindings for libgccjit
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

(define-module (libgccjit-bindings)
  #:use-module (srfi srfi-1)   ;; SRFI  1 - List library
  #:use-module (srfi srfi-9)   ;; SRFI  9 - Records
  #:use-module (srfi srfi-26)  ;; SRFI 26 - specializing parameters (cute, cut)
  
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table) ;; additional alist->... fns
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (ice-9 match)
  #:use-module (clang-ast-tools)
)

                                        ;
                                        ; auto generate rules
                                        ;
;; (define-syntax-rule (as-alist a ...)
;;   (list `(a . ,a) ...))

;; (define-syntax-rule (incf place value)
;;   (set! place (+ place value)))

(define-syntax-rule (pushp place value)
  (set! place (cons value place)))

;; (define-syntax-rule (popp place)
;;   (if (null? place)
;;       '()
;;       (let ((v (car v)))
;;         (set! place (cdr place))
;;         v)))

(define-syntax generate-ast
  (λ (x)
    (syntax-case x ()
      ((_ header-path clang-exe)
       (let* ((lclang    (syntax->datum #'clang-exe))
              (lheader   (syntax->datum #'header-path))
              (clang-ast (call-clang-for-ast-dump lheader lclang)))
         (with-syntax ((ast (datum->syntax #'header-path clang-ast)))
           #''ast))))))

(eval-when (expand load eval)
  (define header-ast (generate-ast "/gnu/store/6wnizcd3zsq2cbinwczfilpn7dsbh17j-libgccjit-15.1.0/include/libgccjit.h"
                                   "/gnu/store/bq87yzd4bmvg8al3fl4nprmpiv62g7vv-clang-20.1.4/bin/clang")))

(eval-when (expand load eval)
  (define (predicate-filter-functions fn)
    (match fn
      ((= (cut assq-ref <> 'name)
          (or (? (cut string-prefix? "__" <>) _)
              (? (λ (a) (not (string-prefix? "gcc_jit_" a))) _)))
       #f)
      ((= (cut assq-ref <> 'name) #f)
       #f)
      ((= (cut assq-ref <> 'loc) ;; probably an external function if has includedFrom
          (? list? (= (cut assq-ref <> 'includedFrom)
                      (? list? _))))
       #f)
      (_ #t)))

  ;; (define (chunk lst start len)
  ;;   (drop (take lst (+ start len)) start))

                                        ; cleaners
  (define* (fn-cleaner #:optional (prefix-remove "GCC_JIT_"))
    (λ (str)
      (string-map
       (λ (c) (if (eq? c #\_) #\- c))
       (string-trim-both (string-downcase (substring str (string-length prefix-remove)))
                         #\space))))

  (define (fn-arg-cleaner str)
    (string-map
     (λ (c) (if (eq? c #\_) #\- c))
     (string-trim-both (string-downcase str) #\space)))

  (define (enum-cleaners)
    (define found-prefix "")
    
    (define (pre name member-names)
      (let ((found (find-longest-prefix/list (cons name member-names))))
        (set! found-prefix
              (if (string-suffix? "_" found)
                  found
                  (string-append found "_")))))

    (define (cleaner member-name)
      ((fn-cleaner found-prefix) member-name))

    (values pre cleaner))

  (define gcc-jit-cleaners (list)) ;; alist
  
  (define (set-cleaners)
    (define (sc key val)
      (pushp gcc-jit-cleaners (cons key val)))
    
    (define-values (epre ecl) (enum-cleaners))
    
    (sc 'enum-pre-members epre)
    (sc 'enum-member      ecl)
    (sc 'enum-name        (fn-cleaner))
    (sc 'enum-member-filter (λ (name) (not (string-prefix? "GCC_JIT_NUM_" name))))
    (sc 'fn-name          (fn-cleaner))
    (sc 'fn-arg           fn-arg-cleaner))

  (set-cleaners))
  


(define-syntax all-enums
  (λ (stx)
    (syntax-case stx ()
      ((_)
       (let* ((enum-asts (find-toplevel-matching-asts header-ast 'EnumDecl))
              (xenums (map (cut datum->syntax stx <>) enum-asts)))
         #`(all-enums #,@xenums)))
      ((_ a* ...)
       #'(begin (ast-enum->define-enum gcc-jit-cleaners a*) ...)
       ))))

(define-syntax all-functions
  (λ (stx)
    (syntax-case stx ()
      ((_ lib)
       (let* ((function-asts (filter predicate-filter-functions (find-toplevel-matching-asts header-ast 'FunctionDecl)))
              (xfns (map (cut datum->syntax #'lib <>) function-asts)))
         #`(all-functions lib #,@xfns)))
      ((_ lib a* ...)
       #'(begin (begin (ast-function->define-ffi-function gcc-jit-cleaners lib a*)
                       (ast-function->define-function gcc-jit-cleaners a*)
                       )
                ...)))))


                                        ;
                                        ; Foreign Library
                                        ;

(define-syntax-rule (define-with-guard (name (arg guard?) ...) . body)
  (define (name arg ...)
    (unless (guard? arg) (throw 'wrong-type-arg (quote guard?) arg)) ...
    . body))

(define gcc-jit (load-foreign-library "libgccjit"
                                     #:search-path '("/gnu/store/6wnizcd3zsq2cbinwczfilpn7dsbh17j-libgccjit-15.1.0/lib/")
                                     ))
(all-enums)
(all-functions gcc-jit)

(module-export-all! (current-module))
