## Synopsis

Utilities dealing with CL types

## Code Examples
### LISP-TYPES

#### DISJOINT_TYPES-P
```lisp
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p 'number '(not float)))))
  (assert-true (equal '(nil t) (multiple-value-list (disjoint-types-p '(not float) 'number))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p '(not number) 'float))))
  (assert-true (equal '(t t) (multiple-value-list (disjoint-types-p 'float '(not number)))))
```

#### SMARTER-SUBTYPEP
```lisp
  (assert (equal '(t t) (smarter-subtypep '(eql :x) 'keyword)))
  (assert (equal '(t t) (smarter-subtypep '(not keyword) '(not (eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep 'keyword '(eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep '(not keyword) '(eql :x)))))
  (assert (equal '(nil t) (multiple-value-list (smarter-subtypep '(not (eql :x)) 'keyword))))
```
#### MDTD-BASELINE
```lisp
  (mdtd-baseline '(float integer bignum string seqeunce))
==>
  (string
   (and sequence (not string))
   bignum
   (and integer (not bignum))
   float)
```

#### REDUCE-LISP-TYPE
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