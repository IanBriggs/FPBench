![FPBench](logo.png)

FPBench provides benchmarks, compilers, and standards for the floating-point research community. 

[![Build Status](https://travis-ci.org/FPBench/FPBench.svg?branch=master)](https://travis-ci.org/FPBench/FPBench)

Benchmarks
----------

The FPBench benchmarks are located in `benchmarks/` in FPCore format.

FPBench contains 98 benchmarks from four sources (FPTaylor, Herbie,
Salsa, and Rosa) covering a variety of application domains and the
full complement of FPCore features.

Compilers
---------

FPBench develops two compiler tools for FPCore programs:

 - `export.rkt`, which exports FPCore computations to languages
   like C or JavaScript and to input formats for tools like Daisy,
   Gappa, and FPTaylor.
 - `transform.rkt`, which applies transformations FPCore computations,
   such as sipmlifying preconditions, unrolling loops, or expanding
   syntax sugar.

These tools are documented [online](http://fpbench.org/tools.html).

Standards
---------

The FPBench standards are located [online](http://fpbench.org/spec/).
FPBench contains standards for:

 - The *FPCore format* for floating-point computations. FPCore is a
   simple S-expression functional programming language and can
   represent arithmetic, transcendental operators, loops, and
   conditionals.
 - *Metadata* to describe the provenance and interpretation of FPCore
   computations.
 - *Measures* to describe accuracy measurements for FPCore computations.
   Several measures are standardized.

Each standard has achieved 1.1 status and can be used by implementations.
The standards are maintained in [another repository on Github](https://github.com/FPBench/FPBench.github.io/).

Papers
------

*Toward a Standard Benchmark Format and Suite for Floating-Point
Analysis*, at NSV’16, by N. Damouche, M. Martel, P. Panchekha, C.
Qiu, A. Sanchez-Stern, and Z. Tatlock

> Described the FPBench standards project, including the FPCore
> standard. The standards (in `www/spec`) continue this work.

*Combining Tools for Optimization and Analysis of Floating-Point
Computations*, at FM’18, by H. Becker, P. Panchekha, E. Darulova, and
Z. Tatlock

> Described experiments combining
> [Daisy](https://gitlab.mpi-sws.org/AVA/daisy-public) and
> [Herbie](https://herbie.uwplse.org) using the FPBench format and
> tools. All scripts are available in the `daisy_herbie` branch.


Helping Out
-----------

FPBench is organized on our
[mailing list](https://mailman.cs.washington.edu/mailman/listinfo/fpbench)
where we discuss work in progress, edit proposed standards, and
announce major
improvements. [Email us](mailto:fpbench.cs.washington.edu) if you have
questions or would like to get involved!
