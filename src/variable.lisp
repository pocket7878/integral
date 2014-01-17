#|
  This file is a part of integral project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage integral.variable
  (:use :cl))
(in-package :integral.variable)

(cl-syntax:use-syntax :annot)

@export
(defvar *auto-migrating-mode* nil
  "Whether use auto-migrating mode or not.")
