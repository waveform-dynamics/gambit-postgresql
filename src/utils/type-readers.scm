(##namespace ("postgresql/utils/type-readers#"))

(##include "~~lib/gambit#.scm")

(declare
 (block)
 (standard-bindings)
 (extended-bindings))

(define current-reader-table (make-parameter (make-table)))

(define current-writer-table (make-parameter (make-table)))

(define (get-reader type #!optional (table (current-reader-table)))
  (table-ref table type #f))

(define (get-writer type #!optional (table (current-writer-table)))
  (table-ref table type #f))

(define (set-reader! type reader #!optional (table (current-reader-table)))
  (table-set! table type reader))

(define (set-writer! type writer #!optional (table (current-writer-table)))
  (table-set! table type writer))

(define (type-of data)
  (cond
   ((string? data) 'text)
   ((char? data) 'char)
   ((number? data) 'number)
   ((u8vector? data) 'byte-vector)
   ((vector? data) 'vector)
   (else #f)))

(define (u8vector->data type vector #!optional
			(table (current-reader-table)))
  (let ((reader (get-reader type table)))
    (if reader
	(with-input-from-u8vector vector reader)
	vector)))

(define (data->u8vector data #!optional
			(type (type-of data))
			(table (current-writer-table)))
  (cond
   ((eq? type 'byte-vector) data)
   ((get-writer type table) =>
    (lambda (writer) (with-output-to-u8vector (u8vector) (lambda () (writer data)))))
   (else
    (with-output-to-u8vector (u8vector) (lambda () (write data))))))

(define (read-text) (read-line (current-input-port) #f))

(define (read-text-array)
  (read-all (current-input-port) (lambda (p) (read-line p #\,))))

(define (read-number) (string->number (read-text)))

(define (write-text txt)
  (display txt))

(define (read-bool)
  (if (equal? (read-text) "t")
      #t #f))

(set-reader! 'text read-text)
(set-reader! 'char read-char)

(set-reader! 'int2 read-number)
(set-reader! 'int4 read-number)
(set-reader! 'int8 read-number)
(set-reader! 'numeric read-number)
(set-reader! 'float4 read-number)
(set-reader! 'float8 read-number)

;; (set-reader! 'oid ?)
(set-reader! 'bool read-bool)
;; (set-reader! 'bytea ?)
;; (set-reader! 'bpchar ?)
;; (set-reader! 'varchar ?)

;; (set-reader! 'date ?)
;; (set-reader! '_date ?)
;; (set-reader! '_interval ?)
;; (set-reader! 'interval ?)
;; (set-reader! '_timestamptz ?)
;; (set-reader! 'timestamptz ?)
;; (set-reader! 'time ?)
;; (set-reader! '_time ?)

(set-reader! 'json read-text)
;; (set-reader! '_json ?)

;; (set-reader! 'jsonb ?)
;; (set-reader! '_jsonb ?)
;; (set-reader! '_uuid ?)
;; (set-reader! 'uuid ?)
;; (set-reader! '_bit ?)
;; (set-reader! '_int2 ?)
;; (set-reader! '_int4 ?)
;; (set-reader! '_int8 ?)
;; (set-reader! '_oid ?)
;; (set-reader! '_numeric ?)
;; (set-reader! '_float4 ?)
;; (set-reader! '_float8 ?)
;; (set-reader! '_bool ?)

(set-reader! '_bpchar read-text)
(set-reader! '_char read-text)
(set-reader! '_text read-text-array)
(set-reader! '_varchar read-text)

(set-writer! 'text write-text)
