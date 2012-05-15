

#Module ?module#

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Hotswap current application.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.2.1

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__
* This command makes use of the `pose_code` module of the
[POSIX-like interface
emulation](http://github.com/beadsland/pose).
<a name="description"></a>

##Description##


 This command module affords the
ability to hotswap the modules of the currently active application.



When run from the [`nosh`](http://github.com/beadsland/nosh)
commandline, `hot` attempts to compile and load any `nosh` modules
which changed source files.  Old module code is purged, current module
code is made old, and the newly compiled binaries are made current.

Developers wishing to make use of this functionality are advised to
make all calls to `loop/*` and other recursively iterative functions
fully qualified (_i.e._, `?MODULE:loop(...)`) so as to protect
active processes from being killed for lingering in old code.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td>Run hotswap as pose command.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


<pre>run(IO::#std{}, ARG::#arg{}, ENV::#env{}) -&gt; no_return()</pre>
<br></br>


Run hotswap as pose command.