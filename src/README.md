# Finally, a CFFI approach to python interfacing in Common Lisp

Previous Common Lisp attempts: [burgled-batteries3](https://github.com/snmsts/burgled-batteries3)

Non Common Lisp approaches
- see [this reddit thread](https://www.reddit.com/r/lisp/comments/yuipy7/pyffi_use_python_from_racket/) for PyFFI in racket, as well as Gambit Scheme
- [PyCall in Julia](https://github.com/JuliaPy/PyCall.jl)

### Status

[x] integers
[x] strings with SBCL/Unicode
[x] tuples
[x] lists
[x] dicts
[x] function return-values
[] function arguments
[] python variable values
[] error handling
[] garbage collection touches
[] numpy and non-numpy arrays
[] arbitrary module import
[] lisp callbacks

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
  0.012 seconds of real time
  0.012838 seconds of total run time (0.012739 user, 0.000099 system)
  108.33% CPU
  28,338,236 processor cycles
  983,040 bytes consed

NIL
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



