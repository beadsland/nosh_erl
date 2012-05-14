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

%% @doc Hotswap current application.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.2.1

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

-version("0.2.1").

%%
%% Include files
%%

-define(debug, true).
-include("pose/include/interface.hrl").

-import(pose_code).
-import(re).

%%
%% Exported Functions
%%

-export([run/3]).

%%
%% API Functions
%%

%% @doc Run hotswap as pose command.
-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
run(IO) -> hotswap_run(IO, ignore_param).

%%
%% Local Functions
%%

hotswap_run(IO, _Line) ->
  ?INIT_POSE,
  ?STDOUT("Hotswapping nosh modules\n"),
  hotswap(IO, noterm),
  hotswap(IO, nosh),
  hotswap_nosh(IO, code:all_loaded()),
  exit(ok).

hotswap_nosh(_IO, []) -> ok;
hotswap_nosh(IO, [{Module, _Path} | Tail]) ->
  {ok, MP} = re:compile("^nosh_"),
  case re:run(atom_to_list(Module), MP, [{capture, none}]) of
    match   -> ?DEBUG("see ~p~n", [Module]),
               hotswap(IO, Module);
    nomatch -> true
  end,
  hotswap_nosh(IO, Tail).

% @todo refactor this given new pose_code implementation
hotswap(IO, Module) ->
  try
    pose_code:load(Module)
  catch
    {Error, Detail} ->
        ?STDERR("~p: ~p~nDetail: ~p~n", [Module, Error, Detail])
  end.