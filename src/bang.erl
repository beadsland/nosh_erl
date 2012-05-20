%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Run underlying OS subshell command as a `pose' command.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.2.0

-define(module, bang).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.2.0").

%%
%% Include files
%%

-define(debug, true).
-include("pose/include/interface.hrl").

-import(string).
-import(erlang).
-import(io).

%%
%% Exported Functions
%%

% API export
-export([start/1, run/3]).

% hidden
-export([bang_loop/3]).

-export([loop/2]).

%%
%% API Functions
%%

-type bang_error() :: {error, any()} | {exit, any()}.
-type bang_status() :: ok | {status, integer()}.
-spec start([atom()]) -> bang_status() | bang_error().
%% @doc Start bang as a blocking function.
start(Line) ->
  IO = ?IO(self()),
  RunPid = spawn_link(?MODULE, run, [IO, ?ARG(?MODULE, Line), ?ENV]),
  ?MODULE:loop(IO, RunPid).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% @doc Run an OS command as a pose command.
run(IO, ARG, ENV) ->
  ?INIT_POSE,
  ?DEBUG("Bang!\n"),
  Words = [atom_to_list(X) || X <- ARG#arg.v],
  Line = string:join(Words, " "),
  Timeout = 1000 * 60 * 5,
  Status = do_bang(IO, Line, Timeout),
  exit(Status).

%%
%% Local Functions
%%

%% @todo write stderr to file and tail same
do_bang(IO, Command, Timeout) ->
  ENV = ?ENV,
  ?INIT_POSE,
  Opts = [stderr_to_stdout, exit_status, {line, 500}],
  case catch erlang:open_port({spawn, Command}, Opts) of
    {'EXIT', Reason}    -> ?DEBUG("port error\n"),
                           {error, Reason};
    Port                -> ?MODULE:bang_loop(IO, Port, Timeout)
  end.

% @hidden Exported for fully qualified calls.
bang_loop(IO, Port, Timeout) ->
  receive
    {Port, {data, {eol, Line}}}     -> ?STDOUT("~s~n", [Line]),
                                       ?MODULE:bang_loop(IO, Port, Timeout);
    {Port, {data, {noeol, Line}}}   -> ?STDOUT("~s", [Line]),
                                       ?MODULE:bang_loop(IO, Port, Timeout);
    {Port, {exit_status, 0}}        -> exit(ok);
    {Port, {exit_status, Status}}   -> exit({status, Status});
    {'EXIT', Port, normal}          -> exit(ok);
    {'EXIT', Port, Reason}          -> exit({exit, Reason});
    Noise                           -> ?STDERR("noise: ~p ~p~n",
                                               [Noise, self()]),
                                       ?MODULE:bang_loop(IO, Port, Timeout)
  after Timeout ->
      exit(timeout)
  end.

%%%
% Start loop
%%%

% @hidden Export to allow for hotswap.
loop(IO, RunPid) ->
  receive
    {purging, _Pid, _Mod}           -> ?MODULE:loop(IO, RunPid);
    {'EXIT', RunPid, ok}            -> ok;
    {'EXIT', RunPid, {ok, What}}    -> do_output(erlout, What);
    {'EXIT', RunPid, Reason}        -> do_output(erlerr, Reason);
    {MsgTag, RunPid, Line}          -> do_output(MsgTag, Line),
                                       ?MODULE:loop(IO, RunPid);
    Noise                           -> do_noise(Noise),
                                       ?MODULE:loop(IO, RunPid)
  end.

% Handle stderr and stdout messages.
do_output(MsgTag, Output) ->
  case MsgTag of
    stdout  -> io:format("~s", [Output]);
    erlout  -> io:format("~p: data: ~p~n", [?MODULE, Output]);
    erlerr  -> Erlerr = ?FORMAT_ERLERR(Output),
               io:format(standard_error, "** ~s~n", [Erlerr]);
    stderr  -> io:format(standard_error, "** ~s", [Output]);
    debug   -> io:format(standard_error, "-- ~s", [Output])
  end.

% Handle message queue noise.
do_noise(Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]).