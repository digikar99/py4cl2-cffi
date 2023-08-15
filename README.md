# Finally, a CFFI approach to python interfacing in Common Lisp

Previous Common Lisp attempts: [burgled-batteries3](https://github.com/snmsts/burgled-batteries3)

Non Common Lisp approaches
- see [this reddit thread](https://www.reddit.com/r/lisp/comments/yuipy7/pyffi_use_python_from_racket/) for PyFFI in racket, as well as Gambit Scheme
- [PyCall in Julia](https://github.com/JuliaPy/PyCall.jl)

### Caveats

Unlike `py4cl` and `py4cl2`, `py4cl2-cffi` can only use one python version in a running lisp image. In addition, while the author has been successful in running the [py4cl2-cffi-tests](https://github.com/digikar99/py4cl2-cffi-tests) without segmentation faults, the project is still in beta stage, so be prepared to run into segmentation faults.

I have been successful in loading `py4cl2-cffi` on CCL and ECL, I have been unsuccessful in loading or running the tests successfully on the CI.

### Configuration

```lisp
CL-USER> (ql:quickload "py4cl2-cffi/config")
To load "py4cl2-cffi/config":
  Load 1 ASDF system:
    py4cl2-cffi/config
; Loading "py4cl2-cffi/config"
[package py4cl2-cffi/config]
("py4cl2-cffi/config")
```

For the most part, configuration happens automatically while loading `py4cl2-cffi/config`. This requires that `python3-config` points to the right program in the shell environment in which the lisp is run. Loading `py4cl2-cffi/config` sets the following variables in the `py4cl2-cffi/config` package:

- `*python-ldflags*`
- `*python-includes*`

In addition, `py4cl2-cffi/config` also exports the following useful symbols:

- `print-configuration`: It is fbound to a function which prints the ldflags and includes that will be used for the compilation of the utility shared object/library that bridges the python C-API with lisp.
- `shared-library-from-lflag`: This is fbound to a generic function which takes in two arguments. The first argument is an ldflag (like `-lpython3.10`) and the second argument is the `(software-type)` as a keyword to be used for specialization on the users systems. Each method should return the shared library name associated with that ldflag and software type. For example, when `(intern (string-upcase (software-type)) :keyword)` is `:linux`, the relevant method should return `python3.10.so`.

### Status

- [x] garbage collection touches
    - An effort has been made o keep a track of reference counts; but if something is missed, and users notice a memory leak, feel free to [raise an issue](https://github.com/digikar99/py4cl2/issues/new)!
  - trivial-garbage:finalize is used to establish the decref process for the pointer corresponding to the pyobject. However, this requires holding the GIL, and so, the user might need to evaluate `(py4cl2-cffi::pygil-release)` at the top level to release the GIL of the current thread, so that the finalizer thread can then acquire it.
- [x] function return-values
- [x] function arguments
- [x] integers
- [x] strings with SBCL/Unicode
- [x] tuples
- [x] lists
- [x] dicts
- [x] double floats
- [x] numpy arrays to CL arrays
- [x] output (partially)
- [x] error output (partially)
- [x] python variable values
- [x] object slots
- [x] methods
- [x] python stdout to lisp stdout (asynchronous, make sure to `sys.stdout.flush()`)
- [x] `with-python-output`
- [x] lisp callbacks
- [x] numpy arrays to non-CL arrays
- [x] arbitrary module import
- [x] numpy floats
- [ ] optimizing pythonizers and lispifiers using static-dispatch

... and much more ...

### Limitations

Goals are less ambitious than burgled-batteries. We aim to get "most" libraries working, with a special focus on functional python.
- Only specialized arrays can be passed by reference. Other values will be passed by value.
- The goal is getting the functional aspects of python - those python functions that do not modify their inputs should "work". Non-functional python functions can only work with arrays. Other functions that modify their inputs will not work.

Tested only on Ubuntu 20.04 (CI) and Ubuntu 18.04 (personal machine). Porting to Windows does not look trivial, but someone could prove me wrong (at least provide some pointers!).

### Why

[py4cl2](https://github.com/digikar99/py4cl2) has gotten the work done for the past few years. But it has the overhead of (i) stream-based inter-process-communication (ii) eval. That's as worse as one could get.

However, when capable, the CFFI approach can be a 50 times faster than py4cl2.

```lisp
CL-USER> (py4cl2-cffi:raw-pyexec "def foo(): return str(1)")
0
CL-USER> (time (dotimes (i 10000)
                 (py4cl2-cffi:pycall "foo")))
Evaluation took:
  0.080 seconds of real time
  0.079740 seconds of total run time (0.079740 user, 0.000000 system)
  100.00% CPU
  174,443,654 processor cycles
  3,045,440 bytes consed

NIL
CL-USER> (py4cl2:raw-pyexec "def foo(): return str(1)")
; No value
CL-USER> (time (dotimes (i 10000)
                 (py4cl2:pycall "foo")))
Evaluation took:
  1.051 seconds of real time
  0.482699 seconds of total run time (0.304186 user, 0.178513 system)
  45.96% CPU
  20,000 forms interpreted
  2,327,364,348 processor cycles
  5,760,832 bytes consed

NIL
```

#### Passing arrays by reference:

```lisp
PY4CL2-CFFI> (ql:quickload "array-operations")
To load "array-operations":
  Load 1 ASDF system:
    array-operations
; Loading "array-operations"

("array-operations")
PY4CL2-CFFI> (let ((a (aops:rand* 'single-float 10))
                   (b (aops:rand* 'single-float 10)))
               (print a)
               (print b)
               (pycall "numpy.add" a b :out a)
               a)

#(0.5093733 0.615062 0.5520501 0.4115485 0.35940528 0.0056368113 0.31019592
  0.4214077 0.32522345 0.2879219)
#(0.23799527 0.9120656 0.99672806 0.54783416 0.91948783 0.14750922 0.68077135
  0.75351477 0.17053545 0.6163509)
#(0.7473686 1.5271276 1.5487782 0.95938265 1.2788931 0.15314603 0.9909673
1.1749225 0.4957589 0.9042728)

PY4CL2-CFFI> (let ((a (aops:rand* 'double-float '(3 3))))
               (print a)
               (pycall "numpy.linalg.svd" a))
#2A((0.8441753387451172d0 0.3109557628631592d0 0.34773027896881104d0)
    (0.3423733711242676d0 0.6038261651992798d0 0.41209208965301514d0)
    (0.5945597887039185d0 0.06366562843322754d0 0.6331008672714233d0))
(#2A((-0.6544319939294636d0 0.16762183299479871d0 -0.7373070503019551d0)
     (-0.49635147614988473d0 -0.8308400431254119d0 0.2516744620798731d0)
     (-0.5703980868177386d0 0.5306672628331492d0 0.6269276503009229d0))
 #(1.4307829399574157d0 0.45313956971575653d0 0.296282569712987d0)
 #2A((-0.7419215134897116d0 -0.37708316610163206d0 -0.5544012569104664d0)
     (0.380805690816161d0 -0.917542287614819d0 0.11446910622779849d0)
     (-0.5518509705193625d0 -0.12619206108679556d0 0.8243397782804759d0)))
```

#### Callbacks

```lisp
PY4CL2-CFFI> (raw-pyexec "def foo(fn, *args, **kwargs): return fn(*args, **kwargs)")

; No value
PY4CL2-CFFI> (pycall "foo" (lambda (d e &rest args &key a b &allow-other-keys)
                             (declare (ignore a b))
                             (list* d e args))
                     8 9 :a 2 :b 3 :d 5)
(8 9 "d" 5 "b" 3 "a" 2)
```

#### A quick and dirty import-module as a function

```lisp
PY4CL2-CFFI> (import-module "matplotlib.pyplot" :as "plt")
T
PY4CL2-CFFI> (pycall "plt.plot"
                     (iota 10)
                     (mapcar (lambda (x) (* x x))
                             (iota 10)))
#(#<PYTHON-OBJECT :type <class 'matplotlib.lines.Line2D'>
  Line2D(_line0)
 {1006670F83}>)
PY4CL2-CFFI> (pycall "plt.show")
#<PYTHON-OBJECT :type <class 'NoneType'>
  None
 {1006672273}>
```

<img margin="auto" width="75%" src="./plt-example.png"></img>

#### numpy

```lisp
PY4CL2-CFFI> (defpymodule "numpy" t :silent t)
T
PY4CL2-CFFI> (numpy.random:random '(2 3 4))
#3A(((0.9556724994386294d0 0.9207667929741092d0 0.38080996781642207d0
      0.36058417847643864d0)
     (0.1939761803809288d0 0.052707969761970785d0 0.5641774015926598d0
      0.34218703751890367d0)
     (0.663085466238284d0 0.8208948328437302d0 0.768715035806218d0
      0.8225795094037658d0))
    ((0.9523448513613038d0 0.8293149376922084d0 0.6616993552816121d0
      0.560839589292125d0)
     (0.004265522613073891d0 0.8874616779694773d0 0.45500882951834853d0
      0.34081255137211874d0)
     (0.3041085477740366d0 0.4351811902627044d0 0.031589664841209175d0
      0.6375274178283377d0)))
PY4CL2-CFFI> (numpy:sum * :axis '(0 2))
#(5.622032172332849d0 2.8405971707274817d0 4.483681664998286d0)
PY4CL2-CFFI> (with-lispifiers ((array (lambda (o)
                                        (magicl:from-array o (array-dimensions o)))))
               (numpy.random:random '(3 4)))
#<MAGICL:MATRIX/DOUBLE-FLOAT (3x4):
   0.814     0.278     0.330     0.782
   0.858     0.342     0.282     0.225
   0.806     0.144     0.543     0.215>
```

### Developer Thoughts on Garbage Collection

If you are working with raw pointers, then all bets are off about handling garbage collection.

Thus, the only time garbage collection should "work correctly" aka - without (i) segmentation faults (ii) memory leaks - is when you are *not* working with raw pointers. In other words, functions that return lisp values *must* perform garbage collection.

An exhaustive list of functions that return lisp values include:

- pyvalue
- pyslot-value
- pymethod
- pycall
- pyhelp
- pyslot-list
- pymethod-list

Even amongst these, GC should not take place until the top level call has done its processing. To be more intrincate seems to require a merge of the python interpreter with the lisp interpreter.

The single place which decides what to *not* collect is the function "lispify" when it returns a python-object struct-wrapper around the pyobject. In these cases, we PYUNTRACK the pointers.

During DecRef-ing through an object finalizer, one needs to hold the GIL, because at least on SBCL, the finalizer may be called through any thread. DecRef-ing without holding the GIL results in segmentation faults.
