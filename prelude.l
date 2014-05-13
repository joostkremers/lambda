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
     "Define a function.

Format: (fn (<name> <arg>*) [<doc string>] <body>")

(def '(mac) (^ '(f & b)
               '(def (head f)
                     (^ (tail f) (last b))
                     (if (= (len b) 2)
                         (eval (nth 0 b)))))
     "Define a macro.

Format: (mac (<name> <arg>*) [<doc string>] <body>")

;; Variable definition
(def '(var) (^ '(name val & doc)
               '(def name val (if (not (null doc))
                                  (eval (nth 0 doc)))))
     "Define a variable.

Format: (var <sym> <val> [<doc string>])")

;; Open new scope
(fn (let b)
    (((\ '(_) b) ())))

;; Unpack List to Function
(fn (unpack f l)
    (eval (join (list f) l)))

;; Unapply List to Function
(fn (pack f & xs) (f xs))

;; Curried and Uncurried calling
(def '(curry) '(unpack))
(def '(uncurry) '(pack))

;; Perform Several things in Sequence
(fn (do & l)
    (if (equal l nil)
        (nil)
      (last l)))


;;; Numeric Functions

;; Minimum of Arguments
(fn (min & xs)
    (if (equal (tail xs) nil)
        (fst xs)
      (do
          (set '(rest) (unpack min (tail xs)))
          (set '(item) (fst xs))
        (if (< item rest) item rest))))

;; Maximum of Arguments
(fn (max & xs)
    (if (equal (tail xs) nil) '(fst xs)
      (do
          (set '(rest) (unpack max (tail xs)))
          (set '(item) (fst xs))
        (if (> item rest) (item) (rest)))))

;;; Conditional Functions

(mac (select & _cs)
     (if (equal _cs nil)
         (error "No Selection Found")
       (if (fst (fst _cs))
           (snd (fst _cs))
         (unpack select (tail _cs)))))

(mac (case _x & _cs)
     (if (equal _cs nil)
         (error "No Case Found")
       (if (equal (eval _x) (fst (fst _cs)))
           (snd (fst _cs))
         (unpack case (join (list _x) (tail _cs))))))

(def '(otherwise) true)


;;; Misc Functions

(fn (flip f a b) (f b a))
(fn (ghost & xs) (eval xs))
(fn (comp f g x) (f (g x)))

;;; List Functions

(fn (fst l) "Return first element of L." (nth 0 l))
(fn (snd l) "Return second element of L." (nth 1 l))
(fn (trd l) "Return third element of L." (nth 2 l))


;; Apply Function to List
(fn (map f l)
    (if (equal l nil)
        nil
      (join (list (f (fst l))) (map f (tail l)))))

;; Apply Filter to List
(fn (filter f l)
    (if (equal l nil)
        nil
      (join (if (f (fst l))
                (head l)
              nil)
            (filter f (tail l)))))

;; Reverse List
(fn (reverse l)
    (if (equal l nil)
        nil
      (join (reverse (tail l)) (head l))))

;; Fold Left
(fn (foldl f z l)
    (if (equal l nil)
        z
      (foldl f (f z (fst l)) (tail l))))

;; Fold Right
(fn (foldr f z l)
    (if (equal l nil)
        z
      (f (fst l) (foldr f z (tail l)))))

(fn (sum l) (foldl + 0 l))
(fn (product l) (foldl * 1 l))

;; Take N items
(fn (take n l)
    (if (= n 0)
        nil
      (join (head l) (take (- n 1) (tail l)))))

;; Drop N items
(fn (drop n l)
    (if (= n 0)
        l
      (drop (- n 1) (tail l))))

;; Split at N
(fn (split n l) (list (take n l) (drop n l)))

;; Take While
(fn (take-while f l)
    (if (not (unpack f (head l)))
        nil
      (join (head l) (take-while f (tail l)))))

;; Drop While
(fn (drop-while f l)
    (if (not (unpack f (head l)))
        l
      (drop-while f (tail l))))

;; Element of List
(fn (elem x l)
    (if (equal l nil)
        false
      (if (equal x (fst l))
          true
        (elem x (tail l)))))

;; Find element in list of pairs
(fn (assoc x l)
    (if (equal l nil)
        (error "No Element Found")
      (do
          (set '(key) (fst (fst l)))
          (set '(val) (snd (fst l)))
        (if (equal key x) (val) (assoc x (tail l))))))

;; Zip two lists together into a list of pairs
(fn (zip x y)
    (if (or (equal x nil) (equal y nil))
        nil
      (join (list (join (head x) (head y))) (zip (tail x) (tail y)))))

;; Unzip a list of pairs into two lists
(fn (unzip l)
    (if (equal l nil)
        (list nil nil)
      (do
          (set '(x) (fst l))
          (set '(xs) (unzip (tail l)))
        (list (join (head x) (fst xs)) (join (tail x) (snd xs))))))
