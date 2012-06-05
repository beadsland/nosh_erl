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

%% @version 0.2.1

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

-version("0.2.1").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(gen_command).
-import(string).
-import(erlang).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Private callbacks
-export([do_run/2]).

% Private fully-qualified loop
-export([loop/3]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, ARG) ->
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
  try erlang:open_port({spawn, Command}, Opts) of
    Port            -> ?MODULE:loop(IO, Port, Timeout)
  catch
    error:Reason    -> ?DEBUG("port error\n"),
                       {error, Reason}
  end.

% @private Exported for fully qualified calls.
loop(IO, Port, Timeout) ->
  receive
    {Port, {data, {eol, Line}}}     -> ?STDOUT("~s~n", [Line]),
                                       ?MODULE:loop(IO, Port, Timeout);
    {Port, {data, {noeol, Line}}}   -> ?STDOUT("~s", [Line]),
                                       ?MODULE:loop(IO, Port, Timeout);
    {Port, {exit_status, 0}}        -> exit(ok);
    {Port, {exit_status, Status}}   -> exit({status, Status});
    {'EXIT', Port, normal}          -> exit(ok);
    {'EXIT', Port, Reason}          -> exit({exit, Reason});
    Noise                           -> ?DEBUG("noise: ~p ~p~n",
                                               [Noise, self()]),
                                       ?MODULE:loop(IO, Port, Timeout)
  after Timeout ->
      exit(timeout)
  end.