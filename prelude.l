;;; Lambda Standard Prelude
;;; =======================
;;; 
;;; Based on the book "Build Your Own Lisp" by Daniel Holden
;;; 
;;; http://www.buildyourownlisp.com/
;;; 
;;; Copyright (c) 2014 Daniel Holden
;;; Copyright (c) 2014 Joost Kremers
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES ; LOSS OF USE,
;;; DATA, OR PROFITS ; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(def '(nil) '())

;;; Functional Functions

;; Function definition
(def '(fn) (^ '(f & b)
              '(def (head f)
                    (\ (tail f) (last b))
                    (if (= (len b) 2)
                        (eval (nth 0 b)))))
     "(fn (name arg*) [\"doc string\"] (body))

Define <name> as a global function with arguments <arg>*. The documenation
string is optional.")

(def '(mac) (^ '(f & b)
               '(def (head f)
                     (^ (tail f) (last b))
                     (if (= (len b) 2)
                         (eval (nth 0 b)))))
     "(mac (name arg*) [\"doc string\"] (body))

Define <name> as a global macro with arguments <arg>*. The documenation
string is optional.")

;; Variable definition
(def '(var) (^ '(name val & doc)
               '(def name val (if (not (null doc))
                                  (eval (nth 0 doc)))))
     "(var sym val [\"doc string\"])

Define <sym> as a global variable with value <val>. The documentation
string is optional.")

;; Open new scope
(fn (let b)
    (((\ '(_) b) ())))

(fn (unpack f l)
    "(unpack func list)

Unpack the elements of <list> as of arguments to <func>:
(unpack + '(2 3 4)) ==> 9"
    (eval (join (list f) l)))

;; Unapply List to Function
(fn (pack f & xs)
    "(pack func & arg*)

Pack a series of <arg>* into a list and apply <func> to it.
(pack head 5 6 7) ==> '(5)"
    (f xs))

(def '(curry) unpack "(curry func list)")
(def '(uncurry) pack "(uncurry func arg*)")

(fn (do & l)
    "(do & clause*)

Evaluate each <clause> in sequence and return the result of the last one."
    (if (equal l nil)
        (nil)
      (last l)))


;;; Numeric Functions

(fn (min & xs)
    "(min & num*)

Return the smallest of a series of <num>*."
    (if (equal (tail xs) nil)
        (fst xs)
      (do
          (set '(rest) (unpack min (tail xs)))
          (set '(item) (fst xs))
        (if (< item rest) item rest))))

;; Maximum of Arguments
(fn (max & xs)
    "(max & num*)

Return the largest of a series of <num>*."
    (if (equal (tail xs) nil) '(fst xs)
      (do
          (set '(rest) (unpack max (tail xs)))
          (set '(item) (fst xs))
        (if (> item rest) (item) (rest)))))

;;; Conditional Functions

(mac (select & _cs)
     "(select & <clause>*)

Each clause is of the form (<expr> <body>), where <expr> is evaluated and
should return a boolean. For the first <expr> that returns true, <body> is
evaluated and its result returned."
     (if (equal _cs nil)
         (error "No Selection Found")
       (if (fst (fst _cs))
           (snd (fst _cs))
         (unpack select (tail _cs)))))

(mac (case _x & _cs)
     "(case <expr> & <clause>*)

Evaluate <expr> and choose among <clause>* based on that value. Each
<clause> is of the form (<val> <body>); <expr> is compared with <val> and
<body> is evaluated if they are equal."
     (if (equal _cs nil)
         (error "No Case Found")
       (if (equal (eval _x) (fst (fst _cs)))
           (snd (fst _cs))
         (unpack case (join (list _x) (tail _cs))))))

(def '(otherwise) true)


;;; Misc Functions

(fn (flip f a b)
    "(flip func a b)

Apply <func> with arguments a and b reversed."
    (f b a))

(fn (comp f g x)
    "(comp func1 func2 arg)

Apply <func2> to <arg> and apply the result to <func1>."
    (f (g x)))

;;; List Functions

(fn (fst l) "(fst list)

Return and evaluate first element of <list>."
    (nth 0 l))

(fn (snd l) "(snd list)

Return and evaluate second element of <list>."
    (nth 1 l))

(fn (trd l)
    "(trd list)

Return and evaluate third element of <list>."
    (nth 2 l))

(fn (map f l)
    "(map func list)

Apply <func> to each element in <list> and collect the results in a list."
    (if (equal l nil)
        nil
      (join (list (f (fst l))) (map f (tail l)))))

(fn (filter f l)
    "(filter pred list)

Test each element in <list> using <pred> and return a list of elements for
which <pred> returns true."
    (if (equal l nil)
        nil
      (join (if (f (fst l))
                (head l)
              nil)
            (filter f (tail l)))))

(fn (reverse l)
    "(reverse list)

Reverse <list>."
    (if (equal l nil)
        nil
      (join (reverse (tail l)) (head l))))

(fn (foldl f z l)
    "(foldl func val list)

Fold <list> from the left using <func> with <val> as the starting value."
    (if (equal l nil)
        z
      (foldl f (f z (fst l)) (tail l))))

;; Fold Right
(fn (foldr f z l)
    "(foldr func val list)

Fold <list> from the right using <func> with <val> as the starting value."
    (if (equal l nil)
        z
      (f (fst l) (foldr f z (tail l)))))

(fn (sum l)
    "(sum list)

Return the sum of the values in <list>."
    (foldl + 0 l))

(fn (product l)
    "(product list)

Return the product of the values in <list>."
    (foldl * 1 l))

;; Take N items
(fn (take n l)
    "(take num list)

Return the first <num> items of <list>."
    (if (= n 0)
        nil
      (join (head l) (take (- n 1) (tail l)))))

(fn (drop n l)
    "(drop num list)

Drop the first <num> items from <list> and return the rest."
    (if (= n 0)
        l
      (drop (- n 1) (tail l))))

(fn (split n l)
    "(split n list)

Split <list> at the <n>th element."
    (list (take n l) (drop n l)))

(fn (take-while f l)
    "(take-while pred list)

Collect elements from <list> as long as they satisfy <pred>."
    (if (not (unpack f (head l)))
        nil
      (join (head l) (take-while f (tail l)))))

(fn (drop-while f l)
    "(drop-while pred list)

Collect elements from <list> as long as they do not satisfy <pred>."
    (if (not (unpack f (head l)))
        l
      (drop-while f (tail l))))

(fn (elem x l)
    "(elem e list)

Return true if <e> is in <list>, false otherwise."
    (if (equal l nil)
        false
      (if (equal x (fst l))
          true
        (elem x (tail l)))))

(fn (assoc x l)
    "(assoc x list)

<list> must be a list of (<key> <value>) pairs. Return the first element of
<list> where <x> matches <key>."
    (if (equal l nil)
        (error "No Element Found")
      (do
          (set '(key) (fst (fst l)))
          (set '(val) (snd (fst l)))
        (if (equal key x) (val) (assoc x (tail l))))))

;; Zip two lists together into a list of pairs
(fn (zip x y)
    "(zip list1 list2)

Zip two lists:

(zip '(1 2 3) '(4 5 6)) ==> '('(1 4) '(2 5) '(3 6))"
    (if (or (equal x nil) (equal y nil))
        nil
      (join (list (join (head x) (head y))) (zip (tail x) (tail y)))))

;; Unzip a list of pairs into two lists
(fn (unzip l)
    "(unzip list)

Unzip a list of pairs into two lists:

(unzip '('(1 4) '(2 5) '(3 6)) ==> '('(1 2 3) '(4 5 6))"
    (if (equal l nil)
        (list nil nil)
      (do
          (set '(x) (fst l))
          (set '(xs) (unzip (tail l)))
        (list (join (head x) (fst xs)) (join (tail x) (snd xs))))))
