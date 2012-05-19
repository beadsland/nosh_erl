

#Module ?module#

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Run underlying OS subshell command as a `pose` command.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.2.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-bang_error">bang_error()</a>##



<pre>bang_error() = failed_open_port | {exit, any()}</pre>



###<a name="type-bang_status">bang_status()</a>##



<pre>bang_status() = ok | {status, integer()}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td>Run an OS command as a pose command.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start bang as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


<pre>run(IO::#std{}, ARG::#arg{}, ENV::#env{}) -&gt; no_return()</pre>
<br></br>


Run an OS command as a pose command.<a name="start-1"></a>

###start/1##


<pre>start(Line::[atom()]) -> <a href="#type-bang_status">bang_status()</a> | <a href="#type-bang_error">bang_error()</a></pre>
<br></br>


Start bang as a blocking function.