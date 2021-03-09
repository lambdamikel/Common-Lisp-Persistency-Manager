# Common-Lisp-Persistency-Manager
A File-Based Fully Automatic Lisp Datastructure Serializer / Persistency Manager, Similar to "Pickle" for Python

## About

Another piece of Common Lisp legacy software from my quarter
century-old Lisp archive :-) It still works flawlessly in 2021 (tested
with LispWorks 6.1 on Ubuntu).

This software package allows you to serialize / "persist" arbitary
Common Lisp datastructures to disc, and read them back, without having
to write a single line of code. It supports CLOS classes and
structures, hash tables, arrays, vectors, symbolcs, numbers, lists, etc.

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

```
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
`pickle` that many programmers take for granted nowadays.  Well, in
1995, I had to write my own Common Lisp `pickle`, simply because
such a functionality didn't exist! 

I started writing complex Common Lisp applications in 1994. For my
first complex Common Lisp CLIM application [(the graphical editor
"GenEd")](https://www.michael-wessel.info/gened.html) I still had to
write a large number of serializers / deserializers by hand.  Given
the large number of CLOS classes for its graphical objects, this was a
tedious endeavor and not enjoyable. I already knew about the MOP
(Metaobject Protocol) and an approach developed by a university
colleague, Heiko Kirschke, called "PLOB" (Persistent Lisp Objects),
but figured that his solution to the problem was a little bit too
sophisticated for me, given that he PLOB required a full-fledged
relational database backend. Also, the use of the not-so-well
supported and not standardized Metaobject Protocol made the approach
not very portable. 

I hence decided to create my own little persistency manager, just good
enough for my own purposes, to save me time. I knew enough about
macros and garbage collection (and the object graph), so I was able to
write `defpersistentclass`. I figured out how to deal with cyclic
references, and to my surprise, it worked out surprisingly well.  I
could even store [whole city maps of a few hundred
KBs](https://www.michael-wessel.info/visco/40.gif) with my persistency
manager! So, for the accompanying program of my dipoma thesis [(the
Visual Spatial Query Lanaguage
"VISCO")](https://www.michael-wessel.info/visco.html) I was already
using this software.

Later on, my colleague (and later boss) Ralf Möller extended the
persistency manager support for more data structures, e.g.,
packages. This extended persistency manager was then used in the Racer
description logic reasoner for its "persistency layer". Racer later
became [RacerPro, which is available on GitHub as OpenSource as
well](https://github.com/ha-mo-we/Racer).  RacerPro's
[racer-persistence.lisp persistency
package](https://github.com/ha-mo-we/Racer/blob/master/source/racer-persistence.lisp)
is this extended version.

Since 1998, I have used this little small software pacakge for every
Common Lisp / CLIM application that I wrote. You can also find it in
my (Tangram
Solver)[https://github.com/lambdamikel/Common-Lisp-Tangram-Solver]
where it implements the save & load functionality.

The persistency maanger has successfully been used with LispWorks,
SBCL, Allegro, MCL, and Common Lisp.

Later, companies such as Franz Inc. (makers of Allegro Common Lisp)
were beginning to offer industrial-grade persistency packages, i.e.,
"AllegroCache". However, none of this was available to me in 1996, and
AllegroCache appeared much later. And honestly, I don't need it for
what I am developing. Something simpler is doing equally well (if not
better).

Also, I remember that [Nick Levine](https://www.nicklevine.org/) gave
a presentation about a *very similar software package* at the European
Common Lisp Meeting in my home town, Hamburg, in April 2006. I could
have given that talk 10 years earlier, but missed the opportunity -
too bad ;-)

For RacerPro, starting in 2005, we have used this sofware for out
license checker. Before becoming OpenSource, RacerPro was commercial,
until 2013. The `.lic` license file format was actually a
serialization produced by this persistency manager, but we obscured it
by using an higher-base number format for the encoding (I believe we
used the entire alphabet, so it was a base-26 encoding or something
like that!). No customer ever managed to crack or alter this license
scheme. It was just a serialized (but rather complex) 
`defpersistenclass license` object that was read in from the license
file, detailing the properties and permissions of the license
(e.g., unlocked features and expiriation date, etc.) 

