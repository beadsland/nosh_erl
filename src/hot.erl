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

%% @reference This command makes use of the `pose_code' module of the
%% <a href="http://github.com/beadsland/pose">POSIX-like interface
%% emulation</a>.
%% @end
%% @doc Hotswap current application.  This command module affords the
%% ability to hotswap the modules of the currently active application.
%%
%% When run from the <a href="http://github.com/beadsland/nosh">`nosh'</a>
%% commandline, `hot' attempts to compile and load any `nosh' modules
%% which changed source files.  Old module code is purged, current module
%% code is made old, and the newly compiled binaries are made current.
%%
%% Developers wishing to make use of this functionality are advised to
%% make all calls to `loop/*' and other recursively iterative functions
%% fully qualified (<i>i.e.</i>, `?MODULE:loop(...)') so as to protect
%% active processes from being killed for lingering in old code.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.2.3

-define(module, hot).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.2.3").

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

-import(gen_command).
-import(pose).
-import(pose_command).

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% private callbacks
-export([do_run/2]).

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
%% @todo make hotswap list env derived
do_run(IO, _ARG) ->
  ?STDOUT("Hotswapping nosh modules\n"),
  Status = do_hot(IO, [noterm, nosh, pose]),
  exit(Status).

%%
%% Local Functions
%%

do_hot(_IO, []) -> ok;
do_hot(IO, [Head | Tail]) ->
  case gen_command:load_command(IO, Head) of
    {module, _Module}   -> do_hot(IO, Tail);
    {error, What}       -> ?STDERR("~s~n", ?FORMAT_ERLERR(What)),
                           {error, What}
  end.
