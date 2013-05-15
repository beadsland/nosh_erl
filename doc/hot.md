

#Module hot#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Hotswap current application.

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.2.3

__Behaviours:__ [`gen_command`](gen_command.md).

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__* This command makes use of the `pose_code` module of the
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
active processes from being killed for lingering in old code.
<a name="types"></a>

##Data Types##




###<a name="type-env_prop">env_prop()</a>##



	env_prop() = atom() | {atom(), string()}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


	run(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}, ENV::#env{plist = [<a href="#type-env_prop">env_prop()</a>]}) -> no_return()
<br></br>


<a name="start-0"></a>

###start/0##


	start() -&gt; no_return()
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


	start(Param::[atom()]) -&gt; no_return()
<br></br>


Start as a blocking function.