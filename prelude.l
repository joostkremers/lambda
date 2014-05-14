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

(def '(fn) (^ '(f & b)
              '(def (head f)
                    (\ (tail f) (last b))
                    (if (= (len b) 2)
                        (eval (nth 0 b)))))
     "Format: (fn (name arg*) [\"doc string\"] (body))
Description: Define <name> as a global function.")

(def '(mac) (^ '(f & b)
               '(def (head f)
                     (^ (tail f) (last b))
                     (if (= (len b) 2)
                         (eval (nth 0 b)))))
     "Format: (mac (name arg*) [\"doc string\"] (body))
Description: Define <name> as a global macro.")

(def '(var) (^ '(name val & doc)
               '(def name val (if (not (null doc))
                                  (eval (nth 0 doc)))))
     "Format: (var sym val [\"doc string\"])
Description: Define <sym> as a global variable with value <val>.")

;; Open new scope
(fn (let b)
    (((\ '(_) b) ())))

(fn (unpack f l)
    "Format: (unpack func list)
Description: Unpack the elements of <list> as of arguments to <func>.
Example: (unpack + '(2 3 4)) ==> 9"
    (eval (join (list f) l)))

(fn (pack f & xs)
    "Format: (pack func & arg*)
Description: Pack a series of <arg>* into a list and apply <func> to it.
Example: (pack head 5 6 7) ==> '(5)"
    (f xs))

(def '(curry) unpack "(curry func list)")
(def '(uncurry) pack "(uncurry func arg*)")

(fn (do & l)
    "Format: (do & clause*)
Description: Evaluate each <clause> in sequence and return the result of
the last one."
    (if (null l)
        nil
      (last l)))


;;; Numeric Functions

(fn (min & xs)
    "Format: (min & num*)
Description: Return the smallest of a series of <num>*."
    (if (null (tail xs))
        (fst xs)
      (do
          (set '(rest) (unpack min (tail xs)))
          (set '(item) (fst xs))
        (if (< item rest) item rest))))

(fn (max & xs)
    "Format: (max & num*)
Description: Return the largest of a series of <num>*."
    (if (null (tail xs)) '(fst xs)
      (do
          (set '(rest) (unpack max (tail xs)))
          (set '(item) (fst xs))
        (if (> item rest) (item) (rest)))))

(fn (odd n)
    "Format: odd n
Description: return true if <n> is odd."
    (/= (mod n 2) 0))

(fn (even n)
    "Format: even n
Description: return true if <n> is even."
    (= (mod n 2) 0))


;;; Conditional Functions

(mac (select & _cs)
     "Format: (select & <clause>*)
Description: Each clause is of the form (<expr> <body>), where <expr> is
evaluated and should return a boolean. For the first <expr> that returns
true, <body> is evaluated and its result returned."
     (if (null _cs)
         (error "No Selection Found")
       (if (fst (fst _cs))
           (snd (fst _cs))
         (unpack select (tail _cs)))))

(mac (case _x & _cs)
     "Format: (case <expr> & <clause>*)
Description: Evaluate <expr> and choose among <clause>* based on that
value. Each <clause> is of the form (<val> <body>); <expr> is compared with
<val> and <body> is evaluated if they are equal."
     (if (null _cs)
         (error "No Case Found")
       (if (equal (eval _x) (fst (fst _cs)))
           (snd (fst _cs))
         (unpack case (join (list _x) (tail _cs))))))

(def '(otherwise) true)


;;; Misc Functions

(fn (flip f a b)
    "Format: (flip func a b)
Description: Apply <func> with arguments a and b reversed."
    (f b a))

(fn ($ f g x)
    "Format: ($ func1 func2 arg)
Description: Apply <func2> to <arg> and apply the result to <func1>."
    (f (g x)))

;;; List Functions

(fn (fst l)
    "Format: (fst list)
Description: Return and evaluate first element of <list>."
    (nth 0 l))

(fn (snd l)
    "Format: (snd list)
Description: Return and evaluate second element of <list>."
    (nth 1 l))

(fn (trd l)
    "Format: (trd list)
Description: Return and evaluate third element of <list>."
    (nth 2 l))

(fn (map f l)
    "Format: (map func list)
Description: Apply <func> to each element in <list> and collect the results
in a list."
    (if (null l)
        nil
      (join (list (f (fst l))) (map f (tail l)))))

(fn (filter f l)
    "Format: (filter pred list)
Description: Test each element in <list> using <pred> and return a list of
elements for which <pred> returns true."
    (if (null l)
        nil
      (join (if (f (fst l))
                (head l)
              nil)
            (filter f (tail l)))))

(fn (reverse l)
    "Format: (reverse list)
Description: Reverse <list>."
    (if (null l)
        nil
      (join (reverse (tail l)) (head l))))

(fn (foldl f z l)
    "Format: (foldl func val list)
Description: Fold <list> from the left using <func> with <val> as the
starting value."
    (if (null l)
        z
      (foldl f (f z (fst l)) (tail l))))

(fn (foldr f z l)
    "Format: (foldr func val list)
Description: Fold <list> from the right using <func> with <val> as the
starting value."
    (if (null l)
        z
      (f (fst l) (foldr f z (tail l)))))

(fn (sum l)
    "Format: (sum list)
Description: Return the sum of the values in <list>."
    (foldl + 0 l))

(fn (product l)
    "Format: (product list)
Description: Return the product of the values in <list>."
    (foldl * 1 l))

(fn (take n l)
    "Format: (take num list)
Description: Return the first <num> items of <list>."
    (if (= n 0)
        nil
      (join (head l) (take (- n 1) (tail l)))))

(fn (drop n l)
    "Format: (drop num list)
Description: Drop the first <num> items from <list> and return the rest."
    (if (= n 0)
        l
      (drop (- n 1) (tail l))))

(fn (split n l)
    "Format: (split n list)
Description: Split <list> at the <n>th element."
    (list (take n l) (drop n l)))

(fn (take-while f l)
    "Format: (take-while pred list)
Description: Collect elements from <list> as long as they satisfy <pred>."
    (if (not (unpack f (head l)))
        nil
      (join (head l) (take-while f (tail l)))))

(fn (drop-while f l)
    "Format: (drop-while pred list)
Description: Collect elements from <list> as long as they do not satisfy
<pred>."
    (if (not (unpack f (head l)))
        l
      (drop-while f (tail l))))

(fn (break f l)
    "Format: (break pred list)
Description: break <list> at the first item that satisfies <pred>."
    (list (take-while ($ not f) l)
          (drop-while ($ not f) l)))

(fn (elem x l)
    "Format: (elem e list)
Description: Return true if <e> is in <list>, false otherwise."
    (if (null l)
        false
      (if (equal x (fst l))
          true
        (elem x (tail l)))))

(fn (assoc x l)
    "Format: (assoc x list)
Description: <list> must be a list of (<key> <value>) pairs. Return the
first element of <list> where <x> matches <key>."
    (if (null l)
        (error "No Element Found")
      (do
          (set '(key) (fst (fst l)))
          (set '(val) (snd (fst l)))
        (if (equal key x) (val) (assoc x (tail l))))))

(fn (zip x y)
    "Format: (zip list1 list2)
Description: Zip two lists
Example: (zip '(1 2 3) '(4 5 6)) ==> '('(1 4) '(2 5) '(3 6))"
    (if (or (null x) (null y))
        nil
      (join (list (join (head x) (head y))) (zip (tail x) (tail y)))))

(fn (unzip l)
    "Format: (unzip list)
Description: Unzip a list of pairs into two lists
Example: (unzip '('(1 4) '(2 5) '(3 6)) ==> '('(1 2 3) '(4 5 6))"
    (if (null l)
        nil
      (do
          (set '(x) (fst l))
          (set '(xs) (unzip (tail l)))
        (list (join (head x) (fst xs)) (join (tail x) (snd xs))))))

(mac (describe f)
     "Format: (describe func)
Description: Display the doc string of <func>."
     (do
         (puts "")
         (puts (doc !f))
       (puts "")))
