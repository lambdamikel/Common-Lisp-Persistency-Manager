;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package :CL-USER)

;;;
;;; Define Packages
;;;

(defpackage persistence
  (:use common-lisp)
  (:shadow #:defclass #:defstruct)
  (:export 
   #:defpersistentclass 
   #:defpersistentstruct 
   #:initialize-loaded-persistent-object
   #:make-object-persistent
   #:load-persistent-object
   #:print-persistence-manager-info

   #:user-write-constructor
   #:user-write-component-constructor
   #:user-write-initializer
   #:user-write-component-initializer
   #:user-fill-object 
   #:user-read-component-object
   #:user-create-empty-object))

