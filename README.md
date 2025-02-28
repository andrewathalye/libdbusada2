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

Status
------
On Linux you can now connect to a message bus via UNIX / Autolaunch and exchange messages.
Not yet tested is TCP / Systemd / Launchd transports
In progress is a dispatch table implementation that supports signals
In progress also is type interning, right now we’re leaking memory
Not yet implemented is file descriptor passing, but the infrastructure is there (~30 min of work max)
Final step is adding support for the previously-written dbus binding generator for high level uses.
This library intentionally is low-level except for type handling.
