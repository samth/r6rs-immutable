#lang scheme/base

(define-syntax re-export
  (syntax-rules ()
    [(_) (re-export rnrs-immutable/base-6
                    rnrs-immutable/exceptions-6
                    rnrs-immutable/programs-6
                    rnrs-immutable/files-6
                    rnrs-immutable/bytevectors-6
                    rnrs-immutable/hashtables-6
                    rnrs-immutable/sorting-6
                    rnrs-immutable/syntax-case-6
                    rnrs-immutable/conditions-6
                    rnrs-immutable/unicode-6
                    rnrs-immutable/control-6
                    rnrs-immutable/lists-6
                    rnrs-immutable/enums-6
                    rnrs-immutable/arithmetic/bitwise-6
                    rnrs-immutable/arithmetic/fixnums-6
                    rnrs-immutable/arithmetic/flonums-6
                    rnrs-immutable/io/ports-6
                    rnrs-immutable/io/simple-6
                    rnrs-immutable/records/inspection-6
                    rnrs-immutable/records/syntactic-6
                    rnrs-immutable/records/procedural-6)]
    [(_ id) (begin
              (require id
                       ;; Shift any run time exports to for-syntax:
                       (for-syntax (only-meta-in 0 id))
                       ;; Shift any for-syntax exports for run time:
                       (for-template (only-meta-in 1 id)))
              (provide (all-from-out id)
                       (for-template (all-from-out id))
                       (for-syntax (all-from-out id))))]
    [(_ id ...)
     (begin (re-export id) ...)]))

(re-export)

;; Also need to export prelims for syntax, since there will
;;  not be a for-syntax import when this module is imported:
(require (for-syntax r6rs/private/prelims))
(provide (for-syntax (all-from-out r6rs/private/prelims)))
