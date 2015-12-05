#lang scheme/base

(require (for-syntax scheme/base))

(provide when unless do 
         (rename-out [r6rs:case-lambda case-lambda]))

(define-syntax (r6rs:case-lambda stx)
  (syntax-case stx ()
    [(_ clause ...)
     (quasisyntax/loc stx
       (case-lambda
        . #,(map (lambda (clause)
                   (syntax-case clause ()
                     [[formals body1 body ...]
                      (syntax-case #'formals ()
                        [(id ...)
                         (andmap identifier? (syntax->list #'(id ...)))
                         clause]
                        [(id ... . rest)
                         (and (identifier? #'rest)
                              (andmap identifier? (syntax->list #'(id ...))))
                         #`[formals 
                            (let ([rest rest])
                              (#%stratified-body body1 body ...))]]
                        [rest
                         (identifier? #'rest)
                         #`[formals 
                            (let ([rest rest])
                              (#%stratified-body body1 body ...))]]
                        [_
                         (raise-syntax-error
                          #f
                          "ill-formed argument sequence"
                          stx
                          #'formals)])]
                     [else
                      (raise-syntax-error
                       #f
                       "ill-formed clause"
                       stx
                       clause)]))
                 (syntax->list #'(clause ...)))))]))

