# LISP-TYPES

## Synopsis

Utilities dealing with CL types

## API

### Typecase API

* `reduced-typecase` -- 
* `auto-permute-typecase` -- 

### MDTD functions

* `mdtd-baseline` -- 
* `mdtd-bdd` -- 
* `mdtd-bdd-graph` -- 
* `mdtd-bdd-graph-strong` -- 
* `mdtd-bdd-graph-weak` -- 
* `mdtd-bdd-graph-weak-dynamic` -- 
* `mdtd-bdd-strong` -- 
* `mdtd-bdd-weak` -- 
* `mdtd-bdd-weak-dynamic` -- 
* `mdtd-graph` -- 
* `mdtd-graph-baker` -- 
* `mdtd-rtev2` -- 
* `mdtd-sat` -- 
* `parameterized-mdtd-bdd-graph` -- 

### ROBDD API

* `ltbdd` -- 
* `ltbdd-node` -- 
* `ltbdd-with-new-hash` -- 
* `lisp-type-bdd-node` -- 

### Subtype API

* `*subtypep*` -- 
* `ambiguous-subtype` -- 
* `smarter-subtypep` -- 
* `subtypep-wrapper` -- 
* `caching-types` -- 

### Type simplification API
* `reduce-lisp-type` -- Given a common lisp type designator such as `(AND A (or (not B) C))`, apply some
algebraic manipulations to reduce the expression to a cannonical form.  The general
cannonical form is an OR of ANDs such as `(OR A (not B) (AND C (not D)))`, but may
be even simpler in cases such as `(OR A B)`, or `(AND A B)`.  A few restrictions apply:
1. OR never appears with an AND block
2. neither AND nor OR appear inside a NOT block
3. OR never has fewer than 2 operands
4. AND never has fewer than 2 operands.



### Type API using BDDs

* `bdd-disjoint-types-p` -- Given two BDDs representing lisp type specifiers, determine their intersection is empty.
* `bdd-empty-type` -- Given a BDDs representing a lisp type specifier, determine if represents the empty type.
* `bdd-subtypep` -- Given two BDDs representing lisp type specifiers, determine if the first is a subtype of the second.
* `bdd-type-equal` -- Given two BDDs representing lisp type specifiers, determine if the types are equivalent.

### Other API

* `disjoint-types-p` --  Determine whether two types are disjoint, i.e., is their interseciton is empty?
* `equivalent-types-p` -- Determine whether two types are equivalent by asking whether each is a subtype of the other.  The subtype relation is determined by `smarter-subtypep`.
* `remove-subs` -- Given a list of types, return a new list with with all elements removed which specifies a subtype of something else in the list.  If the list contains two elements which specify the same type, only one of them is removed, unless it is a subtype of something else in the list in which case both are removed.
* `remove-supers` -- Given a list of types, return a new list with with all elements removed which specifies a supertype of something else in the list.  If the list contains two elements which specify the same type, only one of them is removed, unless it is a supertype of something else in the list in which case both are removed.
* `valid-type-p` --  Predicate to determine whether the given object is a valid type specifier.
* `*ambiguous-subtypes*` -- Special variable containing ambiguous subtype information which has already been warned about.
* `warn-ambiguous-subtype` --  Issue a warning, via `WARN`, if the given pair of types has a subtype
relation which cannot be determined by `SUBTYPEP`.  However, the warning
is supressed if this situation has happened already and memoized into
`*AMBIGUOUS-SUBTYPES*`.

    

## Code Examples

### DISJOINT_TYPES-P
```lisp
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p 'number '(not float)))))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p '(not float) 'number))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p '(not number) 'float))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p 'float '(not number)))))
```

### SMARTER-SUBTYPEP
```lisp
  (assert (equal '(t t) (smarter-subtypep '(eql :x) 'keyword)))
  (assert (equal '(t t) (smarter-subtypep '(not keyword) '(not (eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep 'keyword '(eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep '(not keyword) '(eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep '(not (eql :x)) 'keyword))))
```
### MDTD-BASELINE
```lisp
  (mdtd-baseline '(float integer bignum string seqeunce))
==>
  (string
   (and sequence (not string))
   bignum
   (and integer (not bignum))
   float)
```

### REDUCE-LISP-TYPE
```lisp
  (assert (equal (REDUCE-LISP-TYPE '(array (and integer number) (3)))
                      '(array integer (3))))
  (assert (equal (REDUCE-LISP-TYPE '(array * (3)))
                      '(array * (3))))

  ;; base-string
  (assert (equal (REDUCE-LISP-TYPE '(base-string *))
                      'base-string))

  ;; bit-vector
  (assert (equal (REDUCE-LISP-TYPE '(bit-vector *))
                      'bit-vector))

  (assert (equal (REDUCE-LISP-TYPE '(bit-vector 3))
                      '(bit-vector 3)))

  ;; complex
  (assert (equal (REDUCE-LISP-TYPE '(complex (and number real)))
                      '(complex real)))
  (assert (equal (REDUCE-LISP-TYPE '(complex *))
                      'complex ))

  ;; simple-array
  (assert (equal (REDUCE-LISP-TYPE '(simple-array (and number real) (3)))
                      '(simple-array real (3))))

  ;; vector
  (assert (equal (REDUCE-LISP-TYPE '(vector (and number real)))
                      '(vector real)))



  (assert (equal (REDUCE-LISP-TYPE '(cons (and float number) (or string (not string))))
                      '(cons float t)))
  (assert (equal (REDUCE-LISP-TYPE '(cons * *))
                      'cons))
  (assert (equal (REDUCE-LISP-TYPE '(cons (and float number) *))
                      '(cons float)))
  (assert (equal (REDUCE-LISP-TYPE '(cons * (and float number)))
                      '(cons * float)))

  (assert (equal (REDUCE-LISP-TYPE '(function (integer integer) integer))
                      '(function (integer integer) integer)))
  (assert (equal (REDUCE-LISP-TYPE '(function ((and integer integer) integer) integer))
                      '(function (integer integer) integer)))

  (assert (equal (REDUCE-LISP-TYPE '(function ((and integer integer) (and integer integer)) (and integer integer)))
                      '(function (integer integer) integer)))

  ;; test some optional arguments &optional &key &rest etc

  ;; &optional
  (assert (equal (REDUCE-LISP-TYPE '(function (&optional) (and list cons)))
                      '(function (&optional) cons)))

  (assert (equal (REDUCE-LISP-TYPE '(function (&optional (and integer number)) (and list cons)))
                      '(function (&optional integer) cons)))
  
  ;; &rest
  (assert (equal (REDUCE-LISP-TYPE '(function (&rest (and integer number)) (and list cons)))
                      '(function (&rest integer) cons)))


  ;; &key
  (assert (equal (REDUCE-LISP-TYPE '(function (&key) t))
                      '(function (&key) t)))

  (assert (equal (REDUCE-LISP-TYPE '(function (&key (x (and integer number))) (and list cons)))
                      '(function (&key (x integer)) cons)))

  ;; combining &optional &key &rest
  (assert (equal (REDUCE-LISP-TYPE
                       '(function ((and integer number)
                                   &optional (and integer number) (and integer number)
                                   &rest (and integer number)
                                   &key (x (and integer number)) (y (and integer number)))
                         (and list cons)))
                      '(function (integer
                                  &optional integer integer
                                  &rest integer
                                  &key (x integer) (y integer))
                        cons)))

```


## License

```
Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```