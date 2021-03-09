# Common-Lisp-Persistency-Manager
A File-Based Fully Automatic Lisp Datastructure Serializer / Persistency Manager, Similar to "Pickle" for Python

## About

Another piece of Common Lisp legacy software from my quarter century
old Lisp archive :-) It still works flawlessly (tested with LispWorks
6.1 on Ubuntu).

This package allows you to serialize / store arbitary Common Lisp
datastructures to disc, without having to write a single line of code.
See the file `tests.lisp` for examples.

For example, simple Common Lisp datatypes can be serialized as
follows:

```
(defun test ()
  (make-object-persistent 
     (list 'symbol 123 'test "abcdef" 123.3 4/3 (list 'a 1 2 'b "xyz"))
     "pertest")
  (load-persistent-object "pertest"))
```

Cyclic data structures are no problem either! 

It also serializes vectors, hashtables, and CLOS classes 
that have been defined with the macros `defpersistentclass` and 
`defpersistentstruct`. For CLOS classes:  

```
(defpersistentclass test ()
  ((a :accessor a :initarg :a)
   (b :accessor b :initarg :b)))

(defpersistentclass test2 (test)
  ((c :accessor c :initarg :c)))

(defun run-test ()
  (setf table (let ((table (make-hash-table :test #'equal
                                            :size 100
                                            :rehash-size 100)))
                (loop as i from 1 to 100 do
                      (setf (gethash (list i i i) table)
                            (loop as j from 1 to (+ i 10) collect 
                                  (list (* j j )
                                        (- (* j j ))))))
                table))
  (setf x (make-instance 'test
	                 :a table))
  (setf y (make-instance 'test :a x
		         :b
		         (make-array '(10))))
  (setf z (make-instance 'test2 :c (list x y
				         (make-array '(3)
						     :initial-contents (list x y x)))))
  (setf orig (vector x y z (list x table (vector x z y) x z)))
  (make-object-persistent orig "test")
  (setf copy (load-persistent-object "test")))
```

For structures: 

````
(defpersistentstruct stest 
                     (a)
                     (b))

(defpersistentstruct (stest2 (:include stest))
                     (c))


(defun run-stest ()
  (setf table (let ((table (make-hash-table :test #'equal
                                            :size 100
                                            :rehash-size 100)))
                (loop as i from 1 to 100 do
                      (setf (gethash (list i i i) table)
                            (loop as j from 1 to (+ i 10) collect (* j j ))))
                table))
  (setf x (make-stest
           :a table))
  (setf y (make-stest :a x
                      :b
                      (make-array '(10))))
  (setf z (make-stest2 :c (list x y
                                (make-array '(3)
                                            :initial-contents (list x y x)))))
  (setf orig (vector x y z (list x table (vector x z y) x z)))
  (make-object-persistent orig "test")
  (setf copy (load-persistent-object "test")))
```


## Loading / Usage 

Adjust the logical pathname translations in `persistence-sysdcl.lisp`
to match your environment. Then, simply do a `(load
"persistence:persistence-sysdcl.lisp")`. This also runs a few tests.
You will need a larger stack size (i.e., in the 500 KB range).

## History 

There was a time in software development when you had to write your
own libraries, even for something as fundamental like a Python
`pickle` that many programmers take for granted nowadys.
Well, in 1995, I had to write my own Common Lisp `pickle`. 

I started writed complex Common Lisp applications in 1994. For my
first complex CLIM Common Lisp Application (the graphical editor
"GenEd"), in 1996, I had to write a lot of serializers (and
deserializers) for CLOS objects by hand. I already knew about the
Metaobject Protocol and an approach developed by a colleague, Heiko
Kirschke, "PLOB" (Persistent Lisp Objects), but figured that this was
a little bit too sophisticated for my applications (given that he even
had a database backend and was using the not-so-well support
Metaobject Protocol with CLOS in Allegro Common Lisp, which mad the
approach not very portable).

So, I decided to create my own little persistency manager, for my
diploma theses work that finished in 1998. My master thesis program
`VISCO` already used this persistency manager.

Later on, my colleague (and later boss) Ralf Möller extended the
approach to also allow for package serialization etc. The extended
persistency manager was use in the Racer description logic reasoner,
which later became (RacerPro)[https://github.com/ha-mo-we/Racer].  The
(racer-persistence-lisp)[https://github.com/ha-mo-we/Racer/blob/master/source/racer-persistence.lisp]
file is the extended version.

We have successfully used this with LispWorks, SBCL, Allegro and
Common Lisp.

Later, companies such as Franz Inc. were beginning to offer
industrial-grade persistency packages, i.e., AllegroCache. However,
none of this was available to me in 1996, and AllegroCache appeared
much later. 

Also, I remember (Nick Levine)[https://www.nicklevine.org/] gave a
presentation about a very similar software package at the European
Common Lisp Meeting in Hamburg 30, April 2006. I could have given 
that talk 10 years earlier, but missed the opportunity ;-)  

For Racer, we then also used this serialization format for our
product licenses! By changing the encoding to some higher-based
number system (I believe we used the entire alphabet, so it was 
a number system with base 26 or something like that!) we had a
highly obscurred RacerPro license file. No customer ever managed
to crack the scheme. In fact, it was just a serialized (but rather
complex) `defpersistenclass license` object that was read from 
the license file, detailing the properties of the license. 

