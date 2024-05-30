py4cl2-cffi - a CFFI approach to python interfacing in Common Lisp
---

> Despite being featureful, I won't recommend using py4cl2-cffi in production or in long-term projects *yet*.

Previous Common Lisp attempts: [burgled-batteries3](https://github.com/snmsts/burgled-batteries3) and [cl-python](https://github.com/metawilm/cl-python).

New Common Lisp / SBCL attempt: [ouroboros](https://github.com/marcoheisig/ouroboros).

Non Common Lisp approaches
- see [this reddit thread](https://www.reddit.com/r/lisp/comments/yuipy7/pyffi_use_python_from_racket/) for PyFFI in racket, as well as Gambit Scheme
- [PyCall in Julia](https://github.com/JuliaPy/PyCall.jl)

See [this publication](https://zenodo.org/records/10997435) for the broad design.

# Table of Contents

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [py4cl2-cffi - a CFFI approach to python interfacing in Common Lisp](#py4cl2-cffi---a-cffi-approach-to-python-interfacing-in-common-lisp)
- [Table of Contents](#table-of-contents)
- [Why](#why)
- [Limitations](#limitations)
- [Caveats](#caveats)
- [Status](#status)
- [Tutorial](#tutorial)
    - [Installation](#installation)
        - [Lisp Package Managers](#lisp-package-managers)
        - [Barebones + quicklisp](#barebones--quicklisp)
    - [Configuration](#configuration)
    - [Basic Usage](#basic-usage)
    - [Passing arrays by reference:](#passing-arrays-by-reference)
    - [Callbacks](#callbacks)
    - [Numpy](#numpy)
    - [Plotting](#plotting)
- [Developer Thoughts on Garbage Collection](#developer-thoughts-on-garbage-collection)
- [API Reference](#api-reference)
    - [-](#-)
    - [\*defpymodule-silent-p\*](#defpymodule-silent-p)
    - [\*internal-features\*](#internal-features)
    - [\*lispifiers\*](#lispifiers)
    - [\*print-pyobject\*](#print-pyobject)
    - [\*print-pyobject-wrapper-identity\*](#print-pyobject-wrapper-identity)
    - [\*pygc-threshold\*](#pygc-threshold)
    - [\*pythonizers\*](#pythonizers)
    - [+py-empty-tuple+](#py-empty-tuple)
    - [+py-empty-tuple-pointer+](#py-empty-tuple-pointer)
    - [+py-none+](#py-none)
    - [+py-none-pointer+](#py-none-pointer)
    - [chain](#chain)
    - [chain\*](#chain)
    - [define-lispifier](#define-lispifier)
    - [defpyfun](#defpyfun)
    - [defpymodule](#defpymodule)
    - [disable-pygc](#disable-pygc)
    - [enable-pygc](#enable-pygc)
    - [export-function](#export-function)
    - [import-function](#import-function)
    - [import-module](#import-module)
    - [pycall](#pycall)
    - [pyerror](#pyerror)
    - [pyeval](#pyeval)
    - [pyexec](#pyexec)
    - [pygenerator](#pygenerator)
    - [pyhelp](#pyhelp)
    - [pymethod](#pymethod)
    - [pymethod-list](#pymethod-list)
    - [pyobject-wrapper](#pyobject-wrapper)
    - [pyobject-wrapper-eq](#pyobject-wrapper-eq)
    - [pyobject-wrapper-eq\*](#pyobject-wrapper-eq)
    - [pyref](#pyref)
    - [pyslot-list](#pyslot-list)
    - [pyslot-value](#pyslot-value)
    - [pystart](#pystart)
    - [pystop](#pystop)
    - [python-alive-p](#python-alive-p)
    - [python-getattr](#python-getattr)
    - [python-setattr](#python-setattr)
    - [python-start-if-not-alive](#python-start-if-not-alive)
    - [pythonize](#pythonize)
    - [pyvalue](#pyvalue)
    - [pyversion-info](#pyversion-info)
    - [raw-pyeval](#raw-pyeval)
    - [raw-pyexec](#raw-pyexec)
    - [with-lispifiers](#with-lispifiers)
    - [with-pygc](#with-pygc)
    - [with-python-error-output](#with-python-error-output)
    - [with-python-output](#with-python-output)
    - [with-pythonizers](#with-pythonizers)
    - [with-remote-objects](#with-remote-objects)
    - [with-remote-objects\*](#with-remote-objects)

<!-- markdown-toc end -->

# Why

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

# Limitations

Goals are less ambitious than burgled-batteries. We aim to get "most" libraries working, with a special focus on functional python.
- Only specialized arrays can be passed by reference. Other values will be passed by value.
- The goal is getting the functional aspects of python - those python functions that do not modify their inputs should "work". Non-functional python functions can only work with arrays. Other functions that modify their inputs will not work.

Tested only on Ubuntu 20.04 (CI) and Ubuntu 18.04 (personal machine). Porting to Windows does not look trivial, but someone could prove me wrong (at least provide some pointers!).

# Caveats

Unlike `py4cl` and `py4cl2`, `py4cl2-cffi` can only use one python version in a running lisp image. In addition, while the author has been successful in running the [py4cl2-cffi-tests](https://github.com/digikar99/py4cl2-cffi-tests) without segmentation faults, the project is still in beta stage, so be prepared to run into segmentation faults.

The project is being tested on

- CCL, ECL for Linux on Github Actions.
- SBCL for MacOS / amd64
- SBCL for MacOS / arm64 (M* macs)

# Status

- [x] garbage collection touches
    - An effort has been made to keep track of reference counts; but if something is missed, and users notice a memory leak, feel free to [raise an issue](https://github.com/digikar99/py4cl2/issues/new)!
    - trivial-garbage:finalize is used to establish the decref process for the pointer corresponding to the pyobject. However, this requires holding the GIL, and so, the user might need to evaluate `(py4cl2-cffi::pygil-release)` at the top level to release the GIL of the current thread, so that the finalizer thread can then acquire it.
- [ ] documentation: see the docstrings for the moment; these need to be collected into a more user-friendly reference along with a couple of other things.
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
- [ ] optimization ~~[PyCall.jl](https://github.com/JuliaPy/PyCall.jl) is just about 2x slower than native CPython, while we are 10-20x as slow!~~ (See [./perf-compare/README.org](./perf-compare/README.org).)
- [ ] unloading python libraries to allow reloading python without restarting lisp (?)
- [ ] playing nice with dumping a lisp image
- [x] single threaded mode: some python libraries (including matplotlib) hate multithreaded environments

... and much more ...

# Tutorial

## Installation

### Lisp Package Managers

Use [ultralisp](https://ultralisp.org/) or [ocicl](https://github.com/ocicl/ocicl).

### Barebones + quicklisp

Locally clone the repository and quickload against the latest quicklisp dist.

```
git clone https://github.com/digikar99/py4cl2-cffi
```


## Configuration

```lisp
CL-USER> (ql:quickload "py4cl2-cffi/config")
To load "py4cl2-cffi/config":
  Load 1 ASDF system:
    py4cl2-cffi/config
; Loading "py4cl2-cffi/config"
[package py4cl2-cffi/config]
("py4cl2-cffi/config")
```

Loading `py4cl2-cffi/config` sets the following variables of interest in the `py4cl2-cffi/config` package:

- `*python-ldflags*`
- `*python-includes*`
- `*python-executable-path*`
- `*python-site-packages-path*`

In addition, `py4cl2-cffi/config` also exports the following useful symbols:

- `print-configuration`: It is fbound to a function which prints the ldflags and includes that will be used for the compilation of the utility shared object/library that bridges the python C-API with lisp.
- `shared-library-from-lflag`: This is fbound to a generic function which takes in two arguments. The first argument is an ldflag (like `-lpython3.10`) and the second argument is the `(uiop:operating-system)` as a keyword to be used for specialization on the users systems. Each method should return the shared library name associated with that ldflag and software type. For example, when `(uiop:operating-system)` is `:linux`, the relevant method should return `python3.10.so`.

For the most part, configuration happens automatically while loading `py4cl2-cffi/config`. This requires that `python3` and `python3-config` point to the right programs in the environment in which the lisp is run. Configuration in global or conda environments should be automatic then.

However, pyvenv environments require that users set `*python-executable-path*` manually, before loading `py4cl2-cffi`.

## Basic Usage

```lisp
(defpackage :python-lisp-user
  (:use :cl :py4cl2-cffi))

(in-package :python-lisp-user)
```

Start the embedded python by `(pystart)`. If successful, `(python-alive-p)` should return `T`.

At the heart of py4cl2-cffi are [pyvalue](#pyvalue) and [pycall](#pycall). This is in contrast to py4cl2. In py4cl2, [raw-pyeval](#raw-pyeval) and [raw-pyexec](#raw-pyexec) were the heart. There, pycall ultimately resulted in calls to raw-pyeval. However, py4cl2-cffi has the opposite behavior. Here, raw-pyeval and raw-pyexec ultimately depend on pyvalue and pycall. raw-pyeval and raw-pyexec are not only slow but also ugly in their implementations. Thus, whereever possible, you should try to use pyvalue, (setf pyvalue), and pycall.

`pyvalue` lets you obtain the value corresponding to a python name. It takes a string denoting a python name and returns a value corresponding to that name. For example,

```lisp
(pyvalue "sys")
;=> #<PYOBJECT-WRAPPER :type <class 'module'>
;    <module 'sys' (built-in)>
;   {1004AAA1C3}>
```

An attempt will be made to convert the return values to lisp objects.

```lisp
(pyvalue "answer_to_everything")
; Evaluation aborted on #<PY4CL2-CFFI:PYERROR {100514C153}>.
(setf (pyvalue "answer_to_everything") 42)
;=> 42
(pyvalue "answer_to_everything")
;=> 42
```

But if such a conversion is not possible (see [define-lispifier](#define-lispifiers) and [pythonize](#pythonize)), an instance of [pyobject-wrapper](#pyobject-wrapper) will be returned. Note that pyvalue also has a `(setf pyvalue)` which can be used to set the value of a python name, as we did in the above example.

The printing of a `pyobject-wrapper`-instance can be made less ugly by setting [\*print-pyobject-wrapper-identity\*](#print-pyobject-wrapper-identity) to NIL.

```lisp
(pyvalue "sys")
;=> #<PYOBJECT-WRAPPER :type <class 'module'>
;    <module 'sys' (built-in)>
;   {1004AAA1C3}>
(setf *print-pyobject-wrapper-identity* nil)
;=> NIL
(pyvalue "sys")
;=> <module 'sys' (built-in)>
```

A python callable, that is, a python function, class, or anything that can be called, can be called using [pycall](#pycall). The first argument of `pycall` is usually the name of a python callable.

```lisp
(pyvalue "str")
;=> <class 'str'>
(pycall "str" 42)
;=> "42"
```

But it can also be
- a `cffi:foreign-pointer` to the python callable,
- a `pyobject-wrapper`-instance wrapping the foreign pointer,
- a string, which on passing to [raw-pyeval](#raw-pyeval) returns a python callable
- or, any object that can be [pythonize](#pythonize)-d to a python callable.

Like [pyvalue](#pyvalue), `pycall` too will attempt to convert the return value to a lisp object. But if that is not possible, it will return a `pyobject-wrapper`-instance.

Python modules can be imported using the lightweight [import-module](#import-module). The values from the module can then be accessed using pyvalue, and callables can be called using pycall.

```lisp
(import-module "math") ;=> T
(pyvalue "math.pi") ;=> 3.141592653589793d0
(pycall "math.hypot" 3 4) ;=> 5.0d0
```

A particular name from a particular python module can be imported using [import-function](#import-function).

```lisp
(import-function "hypot" "math") ;=> T
(pycall "hypot" 12 5) ;=> 13.0d0
```

[defpyfun](#defpyfun) and [defpymodule](#defpymodule) are heavyweight alternatives to import-function and import-module. defpyfun defines a lisp function that calls the python callable. defpymodule defines lisp package(s) that holds certain symbols. These symbols are fbound to lisp functions which call corresponding python callables. For example, the below `(defpymodule "math")` form defines a lisp-package with name `MATH`.

```lisp
(defpymodule "math")
; Defining MATH for accessing python package math..
;=> T
```
The lisp package `MATH` contains lisp functions that call corresponding python callables. For example the lisp function `math:hypot` below calls the python callable `math.hypot`.

```lisp
(math:hypot 7 24) ;=> 25.0d0
```

## Passing arrays by reference:

```lisp
PYTHON-LISP-USER> (ql:quickload "array-operations")
To load "array-operations":
  Load 1 ASDF system:
    array-operations
; Loading "array-operations"

("array-operations")
PYTHON-LISP-USER> (let ((a (aops:rand* 'single-float 10))
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

PYTHON-LISP-USER> (let ((a (aops:rand* 'double-float '(3 3))))
                    (print a)
                    (pycall "numpy.linalg.svd" a))
#2A((0.8441753387451172d0 0.3109557628631592d0 0.34773027896881104d0)
    (0.3423733711242676d0 0.6038261651992798d0 0.41209208965301514d0)
    (0.5945597887039185d0 0.06366562843322754d0 0.6331008672714233d0))
SVDResult(U=array([[-0.65892971,  0.44979069, -0.60290959],
       [-0.65302311, -0.73986678,  0.16173425],
       [-0.37332622,  0.50028539,  0.78124392]]), S=array([1.41737039, 0.5804025 , 0.01089903]), Vh=array([[-0.53068899, -0.75714703, -0.38091676],
       [-0.6309116 ,  0.65299712, -0.41898128],
       [ 0.56596798,  0.01797605, -0.82423122]]))
```

## Callbacks

```lisp
PYTHON-LISP-USER> (raw-pyexec "def foo(fn, *args, **kwargs): return fn(*args, **kwargs)")

; No value
PYTHON-LISP-USER> (pycall "foo" (lambda (d e &rest args &key a b &allow-other-keys)
                                  (declare (ignore a b))
                                  (list* d e args))
                          8 9 :a 2 :b 3 :d 5)
(8 9 "d" 5 "b" 3 "a" 2)
```

<img margin="auto" width="75%" src="./plt-example.png"></img>

## Numpy

```lisp
PYTHON-LISP-USER> (defpymodule "numpy" t :silent t)
T
PYTHON-LISP-USER> (numpy.random:random '(2 3 4))
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
PYTHON-LISP-USER> (numpy:sum * :axis '(0 2))
#(5.622032172332849d0 2.8405971707274817d0 4.483681664998286d0)
PYTHON-LISP-USER> (with-lispifiers ((array (lambda (o)
                                             (magicl:from-array o (array-dimensions o)))))
                    (numpy.random:random '(3 4)))
#<MAGICL:MATRIX/DOUBLE-FLOAT (3x4):
   0.814     0.278     0.330     0.782
   0.858     0.342     0.282     0.225
   0.806     0.144     0.543     0.215>
```

## Plotting

See https://github.com/digikar99/py4cl2-cffi/issues/11

```lisp
PYTHON-LISP-USER> (import-module "matplotlib.pyplot" :as "plt")
T
PYTHON-LISP-USER> (pycall "plt.plot"
                     (alexandria:iota 10)
                     (mapcar (lambda (x) (* x x))
                             (alexandria:iota 10)))
#(#<PYTHON-OBJECT :type <class 'matplotlib.lines.Line2D'>
  Line2D(_child0)
 {1006670F83}>)
PYTHON-LISP-USER> (float-features:with-float-traps-masked t
                    (pycall "plt.show"))
#<PYTHON-OBJECT :type <class 'NoneType'>
  None
 {1006672273}>
```

<img margin="auto" width="75%" src="./plt-example.png"></img>

# Developer Thoughts on Garbage Collection

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


# API Reference

### \*additional-init-codes\*

```lisp
Variable
Default Value: NIL
```

A list of strings each of which should be python code. All the code
will be executed by [pystart](#pystart).

### \*defpymodule-silent-p\*

```lisp
Variable
Default Value: NIL
```

[defpymodule](#defpymodule) avoids printing progress if this is T.

### \*internal-features\*

```lisp
Variable
Default Value: (:TYPED-ARRAYS :WITH-PYTHON-OUTPUT)
```

A list of PY4CL2 features available on the system. (Support for :ARRAYS
requires numpy and is only determinable after python process has started.)

The list can include one or more of:

  [:with-python-output](#with-python-output)
  :TYPED-ARRAYS


### \*lispifiers\*

```lisp
Variable
Default Value: NIL
```

Each entry in the alist *LISPIFIERS* maps from a lisp-type to
a single-argument lisp function. This function takes as input the "default" lisp
objects and is expected to appropriately parse it to the corresponding lisp object.

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

### \*print-pyobject\*

```lisp
Variable
Default Value: T
```

If non-NIL, python's 'str' is called on the python-object before printing.

### \*print-pyobject-wrapper-identity\*

```lisp
Variable
Default Value: T
```

If non-NIL, print's the lisp type and identity of the pyobject-wrapper.

### \*pygc-threshold\*

```lisp
Variable
Default Value: 1000
```

Number of references in *PYTHON-NEW-REFERENCES* after which PYGC manipulates reference counts.

### \*pythonizers\*

```lisp
Variable
Default Value: NIL
```

Each entry in the alist *PYTHONIZERS* maps from a lisp-type to
a single-argument PYTHON-FUNCTION-DESIGNATOR. This python function takes as input the
"default" python objects and is expected to appropriately convert it to the corresponding
python object.

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

### +py-empty-tuple+

No documentation found for `+py-empty-tuple+`

### +py-empty-tuple-pointer+

No documentation found for `+py-empty-tuple-pointer+`

### +py-none+

No documentation found for `+py-none+`

### +py-none-pointer+

No documentation found for `+py-none-pointer+`

### chain

```lisp
Macro: (chain &rest chain)
```

### chain\*

```lisp
Function: (chain* &rest chain)
```

### define-lispifier

```lisp
Macro: (define-lispifier name (pyobject-var) &body body)
```

### defpyfun

```lisp
Macro: (defpyfun fun-name &optional pymodule-name &key (as fun-name) (cache t)
        (lisp-fun-name (lispify-name as)) (lisp-package *package*) (safety t))
```


Defines a function which calls python
Example
  (py4cl:pyexec "import math")
  (py4cl:defpyfun "math.sqrt")
  (math.sqrt 42) -> 6.4807405

Arguments:

  FUN-NAME: name of the function in python, before import
  PYMODULE-NAME: name of the module containing `fun-name`

  AS: name of the function in python, after import
  CACHE: if non-NIL, constructs the function body at macroexpansion time
  LISP-FUN-NAME: name of the lisp symbol to which the function is bound*
  LISP-PACKAGE: package (not its name) in which `lisp-fun-name` will be interned
  SAFETY: if T, adds an additional line in the function asking to import the
    package or function, so that the function works even after [pystop](#pystop) is called.
    However, this increases the overhead of stream communication, and therefore,
    can reduce speed.


### defpymodule

```lisp
Macro: (defpymodule pymodule-name &optional (import-submodules NIL) &key
        (cache t) (continue-ignoring-errors t)
        (lisp-package (lispify-name pymodule-name)) (reload t)
        (recompile-on-change NIL) (safety t) (silent *defpymodule-silent-p*))
```


Import a python module (and its submodules) as a lisp-package(s).
Example:
  (py4cl:defpymodule "math" :lisp-package "M")
  (m:sqrt 4)   ; => 2.0

Arguments:

Import a python module (and its submodules) as a lisp-package(s).
Example:
  (py4cl:defpymodule "math" :lisp-package "M")
  (m:sqrt 4)   ; => 2.0

Arguments:

- PYMODULE-NAME: name of the module in python, before importing
- IMPORT-SUBMODULES: leave nil for purposes of speed, if you won't use the
    submodules

- CONTINUE-IGNORING-ERRORS: This is set to non-NIL for convenience.
    Set to NIL while debugging. When this is NIL, any and all kinds of errors
    will be signalled instead of being suppressed silently.

-  CACHE: if non-NIL, produces the DEFPACKAGE and DEFUN forms at macroexpansion time to speed-up future reloads of the system
- LISP-PACKAGE: lisp package, in which to intern (and export) the callables
- RECOMPILE-ON-CHANGE: the name of the ASDF system to recompile if the python version of
    PYMODULE-NAME changes; this only has effect if CACHE is non-NIL
- RELOAD: redefine the LISP-PACKAGE if T
- SAFETY: value of safety to pass to defpyfun; see defpyfun
- SILENT: prints "status" lines when NIL

### disable-pygc

```lisp
Macro: (disable-pygc)
```

### enable-pygc

```lisp
Macro: (enable-pygc)
```

### export-function

```lisp
Function: (export-function function python-name)
```

Makes a lisp `function` available in python process as `python-name`

### import-function

```lisp
Function: (import-function name from &key (as NIL))
```

### import-module

```lisp
Function: (import-module name &key (as NIL))
```

### pycall

```lisp
Function: (pycall python-callable &rest args)
```

If `python-callable` is a string or symbol, it is treated as the name of a
python callable, which is then retrieved using PYVALUE*

### pyerror

```lisp
Condition
```


### pyeval

```lisp
Function: (pyeval &rest args)
```

### pyexec

```lisp
Function: (pyexec &rest args)
```

### pygenerator

```lisp
Function: (pygenerator function stop-value)
```

### pyhelp

```lisp
Function: (pyhelp string-or-python-callable)
```

### pymethod

```lisp
Function: (pymethod object method-name &rest args)
```

### pymethod-list

```lisp
Function: (pymethod-list pyobject &key (as-vector NIL))
```

### pyobject-wrapper

```lisp
Structure
```

A wrapper around a pointer to a python object.
LOAD-FORM is used if the pyobject-wrapper is dumped into a compiled lisp file.


### pyobject-wrapper-eq

```lisp
Function: (pyobject-wrapper-eq o1 o2)
```

Returns T if `o1` and `o2` are both [pyobject-wrapper](#pyobject-wrapper) with the same pointer, or
the same lisp objects which are EQ to each other. Returns NIL in all other cases.

### pyobject-wrapper-eq\*

```lisp
Function: (pyobject-wrapper-eq* o1 o2)
```

Like [pyobject-wrapper-eq](#pyobject-wrapper-eq) but assumes that `o1` and `o2` are [pyobject-wrapper](#pyobject-wrapper) each.

### pyref

```lisp
Function: (pyref object &rest indices)
```

### pyslot-list

```lisp
Function: (pyslot-list pyobject &key (as-vector NIL))
```

### pyslot-value

```lisp
Function: (pyslot-value object slot-name)
```

### pystart

```lisp
Function: (pystart)
```

### pystop

```lisp
Function: (pystop)
```

### python-alive-p

```lisp
Function: (python-alive-p)
```

### python-getattr

```lisp
Generic Function: (python-getattr object slot-name)
```

Called when python accesses an object's slot (__getattr__)

### python-setattr

```lisp
Generic Function: (python-setattr object slot-name value)
```

Called when python sets an object's slot (__setattr__)

### python-start-if-not-alive

```lisp
Function: (python-start-if-not-alive)
```

### pythonize

```lisp
Generic Function: (pythonize lisp-value-or-object)
```

Given a lisp object, return a CFFI:FOREIGN-POINTER pointing to the python object corresponding to the given lisp object.

The implemented methods are expected to return a new (strong) reference
to the python object. The method is also expected to call PYTRACK
to notify the PYGC functionality to delete the reference once the object
is no longer needed.

See the documentation for PYGC to understand when reference deletion
takes place.

### pyvalue

```lisp
Function: (pyvalue python-name-or-variable)
```

Get the value of a python-name-or-variable.
Example:

(pyvalue "sys") ;=> <module 'sys' (built-in)>
(pyvalue "sys.path")
;=>
  #("/home/user/miniconda3/lib/python310.zip"
    "/home/user/miniconda3/lib/python3.10"
    "/home/user/miniconda3/lib/python3.10/lib-dynload"
    "/home/user/miniconda3/lib/python3.10/site-packages")


### pyversion-info

```lisp
Function: (pyversion-info)
```

Return a list, using the result of python's sys.version_info.

### raw-pyeval

```lisp
Function: (raw-pyeval &rest code-strings)
```


Unlike PY4CL or PY4CL2, the use of RAW-PY, `raw-pyeval` and [raw-pyexec](#raw-pyexec),
[pyeval](#pyeval), [pyexec](#pyexec) should be avoided unless necessary.
Instead, use [pycall](#pycall), [pyvalue](#pyvalue), (SETF [pyvalue](#pyvalue)), [pyslot-value](#pyslot-value), (SETF [pyslot-value](#pyslot-value)), and [pymethod](#pymethod).

RAW-PY, `raw-pyeval`, [raw-pyexec](#raw-pyexec) are only provided for backward compatibility.

### raw-pyexec

```lisp
Function: (raw-pyexec &rest code-strings)
```


Unlike PY4CL or PY4CL2, the use of RAW-PY, [raw-pyeval](#raw-pyeval) and `raw-pyexec`,
[pyeval](#pyeval), [pyexec](#pyexec) should be avoided unless necessary.
Instead, use [pycall](#pycall), [pyvalue](#pyvalue), (SETF [pyvalue](#pyvalue)), [pyslot-value](#pyslot-value), (SETF [pyslot-value](#pyslot-value)), and [pymethod](#pymethod).

RAW-PY, [raw-pyeval](#raw-pyeval), `raw-pyexec` are only provided for backward compatibility.

### with-lispifiers

```lisp
Macro: (with-lispifiers (&rest overriding-lispifiers) &body body)
```

Each entry of `overriding-lispifiers` is a two-element list of the form
  (TYPE LISPIFIER)
Here, TYPE is unevaluated, while LISPIFIER will be evaluated; the LISPIFIER is expected
to take a default-lispified object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

  (raw-pyeval "[1, 2, 3]") ;=> #(1 2 3) ; the default lispified object
  (with-lispifiers ((vector (lambda (x) (coerce x 'list))))
    (print (raw-pyeval "[1,2,3]"))
    (print (raw-pyeval "5")))
  ; #(1 2 3) ; default lispified object
  ; (1 2 3)  ; coerced to LIST by the lispifier
  ; 5        ; lispifier uncalled for non-VECTOR
  5

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

### with-pygc

```lisp
Macro: (with-pygc &body body)
```

Code surrounded by `with-pygc` performs garbage collection
only after executing all of `body`.

### with-python-error-output

```lisp
Macro: (with-python-error-output &body forms-decl)
```

Gets the output of the python program executed in `forms-decl` in the form a string.

### with-python-output

```lisp
Macro: (with-python-output &body forms-decl)
```

Gets the output of the python program executed in `forms-decl` in the form a string.

### with-pythonizers

```lisp
Macro: (with-pythonizers (&rest overriding-pythonizers) &body body)
```

Each entry of `overriding-pythonizers` is a two-element list of the form
  (TYPE PYTHONIZER)
Here, TYPE is unevaluated, while PYTHONIZER will be evaluated; the PYTHONIZER is expected
to take a default-pythonized object (see lisp-python types translation table in docs)
and return the appropriate object user expects.

For example,

  ; A convenience function
  (defun pyprint (object)
    (pycall "print" object)
    (pycall "sys.stdout.flush")
    (values))

  (pyprint #(1 2 3)) ; prints [1, 2, 3] ; the default object
  (with-pythonizers ((vector "tuple"))
    (pyprint #(1 2 3))
    (pyprint 5))
  ; (1, 2, 3) ; coerced to tuple by the pythonizer
  ; 5         ; pythonizer uncalled for non-VECTOR
  5

NOTE: This is a new feature and hence unstable; recommended to avoid in production code.

### with-remote-objects

```lisp
Macro: (with-remote-objects &body body)
```

Ensures that all values returned by python functions
and methods are kept in python, and only pointers are returned to lisp.
This is useful if performing operations on large datasets.

### with-remote-objects\*

```lisp
Macro: (with-remote-objects* &body body)
```

Ensures that all values returned by python functions
and methods are kept in python, and only handles returned to lisp.
This is useful if performing operations on large datasets. Unlike
with-remote-objects, evaluates the last result and returns not just a handle.
