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

-import(strings).
-import(erlang).

%%
%% Exported Functions
%%

% API export
-export([run/3]).

% hidden
-export([bang_loop/3]).

%%
%% API Functions
%%

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% @doc Run an OS command as a pose command.
run(IO, ARG, _ENV) ->
  ?INIT_POSE,
  ?DEBUG("Bang!\n"),
  Line = strings:join([ARG#arg.cmd | ARG#arg.v], " "),
  Timeout = 1000 * 60 * 5,
  Status = do_bang(IO, Line, Timeout),
  exit(Status).

%%
%% Local Functions
%%

%% @todo write stderr to file and tail same
do_bang(IO, Command, Timeout) ->
  ?INIT_POSE,
  Opts = [stderr_to_stdout, exit_status, {line, 500}],
  Port = erlang:open_port({spawn, Command}, Opts),
  if is_port(Port)  -> ?MODULE:bang_loop(IO, Port, Timeout);
     true           -> exit(failed_open_port)
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
    {'EXIT', Port, Reason}          -> exit(Reason);
    Noise                           -> ?STDERR("noise: ~p ~p~n",
                                               [Noise, self()]),
                                       ?MODULE:bang_loop(IO, Port, Timeout)
  after Timeout ->
      exit(timeout)
  end.