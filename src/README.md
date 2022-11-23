# Finally, a CFFI approach to python interfacing in Common Lisp

Previous Common Lisp attempts: [burgled-batteries3](https://github.com/snmsts/burgled-batteries3)

Non Common Lisp approaches
- see [this reddit thread](https://www.reddit.com/r/lisp/comments/yuipy7/pyffi_use_python_from_racket/) for PyFFI in racket, as well as Gambit Scheme
- [PyCall in Julia](https://github.com/JuliaPy/PyCall.jl)

### Configuration

```lisp
CL-USER> (ql:quickload "py4cl2/cffi-config")
To load "py4cl2/cffi-config":
  Load 1 ASDF system:
    py4cl2/cffi-config
; Loading "py4cl2/cffi-config"
[package py4cl2/cffi-config]
("py4cl2/cffi-config")
```

Set the various configuration parameters in the package `py4cl2/cffi-config` (optionally, first start the lisp process in the virtual environment and then set the configuration parameters):

- `*python-additional-libraries*`
- `*python-additional-libraries-search-path*`
- `*python-include-path*`
- `*python-shared-object-path*`

One can use the `python3-config` or equivalent to find these parameters. (See [../.github/workflows/CI-cffi.yml](../.github/workflows/CI-cffi.yml) For instance, the following lisp command sets the parameters to their appropriate values on the author's PC using a miniconda environment:

```lisp
PY4CL2/CFFI-CONFIG> (setq *python-shared-object-path* #P"/home/user/miniconda3/lib/libpython3.8.so"
                          *python-include-path* #P"/home/user/miniconda3/include/python3.8/"
                          *python-additional-libraries-search-path* #P"/home/user/miniconda3/lib/")
#P"/home/user/miniconda3/lib/"
```

### Status

- [ ] garbage collection touches (partial) [important]
- [x] integers
- [x] strings with SBCL/Unicode
- [x] tuples
- [x] lists
- [x] dicts
- [x] double floats
- [x] function return-values
- [x] function arguments
- [x] output (partially)
- [x] error output (partially)
- [x] python variable values
- [ ] arbitrary module import (partial) [important]
- [ ] numpy and non-numpy arrays
- [ ] numpy floats
- [ ] lisp callbacks

... and much more ...

### Why

py4cl2 has gotten the work done for the past few years. But it has the overhead of (i) stream-based inter-process-communication (ii) eval. That's as worse as one could get.

However, when capable, the CFFI approach can be a 50 times faster than py4cl2.

```lisp
CL-USER> (py4cl2/cffi:raw-py "def foo(): return str(1)")
0
CL-USER> (time (dotimes (i 10000)
                 (py4cl2/cffi:pycall "foo")))
Evaluation took:
  0.024 seconds of real time
  0.023645 seconds of total run time (0.022448 user, 0.001197 system)
  100.00% CPU
  52,197,588 processor cycles
  983,040 bytes consed

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



