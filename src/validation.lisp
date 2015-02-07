(in-package :cl-user)
(defpackage integral.validation
  (:use :cl)
  (:import-from :integral.table
                :<dao-class>))

(in-package integral.validation)

(cl-syntax:use-syntax :annot)

@export
(defgeneric validate-presence-of (object slot-name)
  (:method ((object <dao-class>) slot-name)
    (and (slot-boundp object slot-name)
     (not (null (slot-value object slot-name))))))

@export
(defgeneric validate-uniqueness-of (object slot-name)
  (:method ((object <dao-class>) slot-name)
    (let ((val (slot-value object slot-name)))
      (null (select-sql
             (select :*
                     (from (intern (table-name class)))
                     (where (:= slot-name val))))))))

@export
(defgeneric validate-length-of (object slot-name &key min max is)
  (:method ((object <dao-class>) slot-name &key (min 0) max is)
    (let ((val (slot-value object slot-name)))
      (cond (is (= is (length val)))
            (max
             (<= min (length val) max))
            (<= min (length val))))))

@export
(defgeneric validate-format-of (object slot-name format)
  (:method ((object <dao-class>) slot-name format)
    (let ((val (slot-value object slot-name)))
      (cl-ppcre:all-matches format val))))

