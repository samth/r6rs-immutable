#lang scheme/base

(require rnrs-immutable/enums-6 rnrs/bytevectors-6)
(define list->mlist values)
(define mlist->list values)
(define mlist? list?)
;; copied from rnrs/bytevectors-6
(define (check-endian endianness)
  (unless (or (eq? endianness 'little)
              (eq? endianness 'big))
    (raise-type-error 'bytevector-operation "'big or 'little" endianness)))

(provide endianness
         native-endianness
         (rename-out [bytes? bytevector?]
                     [bytes-length bytevector-length]
                     [bytes=? bytevector=?]
                     [bytes-copy bytevector-copy]
                     [bytes-ref bytevector-u8-ref]
                     [bytes-set! bytevector-u8-set!])
         bytevector-copy!
         bytevector->u8-list
         u8-list->bytevector
         make-bytevector 
         bytevector-fill!
         bytevector-s8-ref
         bytevector-s8-set!

         bytevector-u16-ref
         bytevector-s16-ref
         bytevector-u16-native-ref
         bytevector-s16-native-ref
         bytevector-u16-set!
         bytevector-s16-set!
         bytevector-u16-native-set!
         bytevector-s16-native-set!

         bytevector-u32-ref
         bytevector-s32-ref
         bytevector-u32-native-ref
         bytevector-s32-native-ref
         bytevector-u32-set!
         bytevector-s32-set!
         bytevector-u32-native-set!
         bytevector-s32-native-set!

         bytevector-u64-ref
         bytevector-s64-ref
         bytevector-u64-native-ref
         bytevector-s64-native-ref
         bytevector-u64-set!
         bytevector-s64-set!
         bytevector-u64-native-set!
         bytevector-s64-native-set!

         bytevector-uint-ref
         bytevector-sint-ref
         bytevector-uint-set!
         bytevector-sint-set!

         bytevector-ieee-single-ref
         bytevector-ieee-single-native-ref
         bytevector-ieee-single-set!
         bytevector-ieee-single-native-set!
         bytevector-ieee-double-ref
         bytevector-ieee-double-native-ref
         bytevector-ieee-double-set!
         bytevector-ieee-double-native-set!

         bytevector->uint-list
         bytevector->sint-list
         uint-list->bytevector
         sint-list->bytevector

         string->utf8
         string->utf16
         string->utf32
         utf8->string
         utf16->string
         utf32->string)

(define (bytevector->u8-list bv)
  (list->mlist (bytes->list bv)))

(define (u8-list->bytevector l)
  (list->bytes (mlist->list l)))

(define (bytevector->int-list who ref bv endianness size)
  (unless (bytes? bv)
    (raise-type-error who "bytevector" bv))
  (check-endian endianness)
  (unless (exact-positive-integer? size)
    (raise-type-error who "exact positive integer" size))
  (unless (zero? (modulo (bytes-length bv) size))
    (raise-mismatch-error who "bytevector length is not a mulitple of given size: " size))
  (list->mlist
   (for/list ([k (in-range 0 (bytes-length bv) size)])
     (ref bv k endianness size))))

(define (bytevector->uint-list bv endianness size)
  (bytevector->int-list 'bytevector->uint-list bytevector-uint-ref bv endianness size))

(define (bytevector->sint-list bv endianness size)
  (bytevector->int-list 'bytevector->sint-list bytevector-sint-ref bv endianness size))

(define (int-list->bytevector who signed? set l endianness size)
  (unless (mlist? l)
    (raise-type-error who "list" l))
  (check-endian endianness)
  (unless (exact-positive-integer? size)
    (raise-type-error who "exact positive integer" size))
  (let* ([l (mlist->list l)]
         [len (length l)]
         [bv (make-bytes (* size len))])
    (for ([v (in-list l)]
          [k (in-naturals)])
      (set bv (* k size) v endianness size))
    bv))

(define (uint-list->bytevector l endianness size)
  (int-list->bytevector 'uint-list->bytevector #f bytevector-uint-set! l endianness size))

(define (sint-list->bytevector l endianness size)
  (int-list->bytevector 'sint-list->bytevector #f bytevector-sint-set! l endianness size))


