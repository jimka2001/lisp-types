## Synopsis

Slime-friendly Unit testing package, based loosely on lisp-unit (https://github.com/OdonataResearchLLC/lisp-unit). 

* define-test -- defines a test and a 0-ary function of the same name.  This and other tests may be run by calling `(run-tests)`
* assert-true -- assert that a single expression returns non-nil.  E.g.,   
`(assert-true (= (+ a b) (- c d)))`
* assert-false -- assert that a single expression returns non-nil.  E.g.,   
`(assert-false (= (+ a b) (- c d)))`
* assert-error -- assert that evaluating a given expression signals a named condition   E.g.,   
`(assert-error error (= (f a b) (g c d)))`  
`(assert-error my-condition (format (f a b) (g c d)))`
* run-tests -- runs all loaded tests by default.  `:tests` specifies a test-name or list thereof to run. `:break-on-error` specifies to go into the debugger (or otherwise default error handler) if an error condition is triggered.  Otherwise test is simply marked as failed and reported later.
* run-1-test -- like `run-tests` but runs a single test. `:break-on-error` can be used as well
* run-package-tests -- run tests whose name is the designated package or list of packages.  E.g.,  
  `(run-package-tests "MY-PACKAGE")`  
  `(run-package-tests '(:pack1 :pack2) :break-on-error t)`

When the tests run, output such as the following is printed to `*standard-output*`:
```
Summary of tests:
PACKAGES: (JIMKA-TEST 2D-ARRAY-TEST DISPATCH-TEST CL-ROBDD-TEST LISP-TYPES-TEST
           LISP-TYPES-BAKER-ANALYSIS NDFA-TEST RTE-REGEXP-TEST RTE-TEST)
TOTAL TESTS: 181
ASSERTIONS PASSED: 1867
ASSERTIONS FAILED: 4
  1 failed assertions in LISP-TYPES-BAKER-ANALYSIS::BAKER/DECOMPOSE-5
  1 failed assertions in LISP-TYPES-BAKER-ANALYSIS::BAKER/DECOMPOSE-2
  2 failed assertions in JIMKA-TEST::JIMKA-TEST-1
ERRORS: 1
  JIMKA-TEST::JIMKA-TEST-1
ELAPSED TIME: 12 minutes 22 seconds
```
While tests are running, incremental output will be printed to `*standard-output*` such as the following.
```
Running tests from packages: (JIMKA-TEST 2D-ARRAY-TEST DISPATCH-TEST
                              CL-ROBDD-TEST LISP-TYPES-TEST
                              LISP-TYPES-BAKER-ANALYSIS NDFA-TEST
                              RTE-REGEXP-TEST RTE-TEST)
Starting: Thu Sep 20 12:06:59 2018
Running: 1/181 RTE-TEST::TEST/DESTRUCTURING-CASE-4
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 2/181 RTE-TEST::TEST/DESTRUCTURING-CASE-3
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 3/181 RTE-TEST::TEST/DESTRUCTURING-CASE-2-C
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 4/181 RTE-TEST::TEST/DESTRUCTURING-CASE-2-B
Finished: Thu Sep 20 12:06:59 2018
Starting: Thu Sep 20 12:06:59 2018
Running: 5/181 RTE-TEST::TEST/DESTRUCTURING-CASE-2-A
Finished: Thu Sep 20 12:06:59 2018
```

Here is an example of the output if a test assertion fails.
```
(define-test jimka-test-1
  (assert-false (= 1 2))
  (assert-false (= 1 3))
  (assert-true (= 1 1))
  (assert-true (= 2 2))
  (assert-false (= 2 3))
  (assert-error division-by-zero (/ 1 0))
  (let ((a 2)
	(b 1))
    (assert-false (< a b)))
  (let ((a "abc")
	(b "ABCD"))
    (assert-true (string-equal a b))
    (assert-true (string-equal (concatenate 'string a b)
			       (format nil "~A~A" b a))))
  (assert-error error
		(error "some error")))
```
The test can be run with `(run-1-test 'jimka-test-1)` to see the following output.  Notice that when test assertions fail, not only the unevaluated arguments are shown but their values, if different.
```
Running tests from packages: (JIMKA-TEST)
Starting: Thu Sep 20 12:18:25 2018
Running: 1/1 JIMKA-TEST::JIMKA-TEST-1
  Failed: (STRING-EQUAL JIMKA-TEST::A JIMKA-TEST::B)
      JIMKA-TEST::A
        => "abc"
      JIMKA-TEST::B
        => "ABCD"
    Expected: T
    Got:      NIL
  Failed: (STRING-EQUAL (CONCATENATE 'STRING JIMKA-TEST::A JIMKA-TEST::B)
                        (FORMAT NIL "~A~A" JIMKA-TEST::B JIMKA-TEST::A))
      (CONCATENATE 'STRING JIMKA-TEST::A JIMKA-TEST::B)
        => "abcABCD"
      (FORMAT NIL "~A~A" JIMKA-TEST::B JIMKA-TEST::A)
        => "ABCDabc"
    Expected: T
    Got:      NIL
Finished: Thu Sep 20 12:18:25 2018
------------------
Summary of tests:
PACKAGES: (JIMKA-TEST)
TOTAL TESTS: 1
ASSERTIONS PASSED: 8
ASSERTIONS FAILED: 2
  2 failed assertions in JIMKA-TEST::JIMKA-TEST-1
ERRORS: 0
ELAPSED TIME: 0 seconds
```

I found my unit-testing needs were diverging from what was offered in lisp-unit, and the lisp-unit maintainers for whatever reason did not accomodate my pull requests.  On the other hand my unit-testing needs were pretty simply, so I built my own unit-testing package using the same API which I was using from lisp-unit.
