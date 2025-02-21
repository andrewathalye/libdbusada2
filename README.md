libdbusada2
===========

A reimplementation of the reference D-Bus libary for Ada.

Why not D\_Bus/Ada?
----------------
The reference D-Bus library doesn’t support properties and its mechanisms for object registration and signals don’t mesh well with Ada constructs.

This rewrite isn’t based on the original code, but rather on the specification.
It aims to fully implement the D-Bus specification in Ada 2012 code.

Documentation
-------------
(WIP)
Development plans live in /plans.txt

When compiling for Mac, specify -XOS=Darwin on the command line.
Otherwise it will assume 'unix' and fail to find a machine id

TODO we need to remove Ada 2022 code where not necessary, since
compiler support is not there yet.
