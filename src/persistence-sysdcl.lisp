;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

(setf (logical-pathname-translations "base")
      '(("**;*.*" "~/**/*.*")))

(setf (logical-pathname-translations "persistence")
      '(("**;*.*" "base:persistence;**;*.*")))

(load "persistence:define-system.lisp")

(define-system persistence 
    (:default-pathname "persistence:")
  (:serial 
   "persistence-package"
   "persistence2"
   "tests"))

(defun load-persistence-manager (&optional force-p)
  (load-system 'persistence :force-p force-p))

(load-persistence-manager t)

(princ (persistence::test))


;; you need about ~500 KB of stack for this! 
;; just hit continues / extend stack when you get 
;; a stack overflow 
(princ (persistence::run-stest))

;; you need about ~500 KB of stack for this! 
;; just hit continues / extend stack when you get 
;; a stack overflow 
(princ (persistence::run-test))

