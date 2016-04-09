%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Scott Lystig Fritchie.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(hcl).
-export([setup/0, setup/1]).

setup() ->
    setup(erlang:now()).

setup(Seed) ->
    ok = start_partition_simulator(Seed).

start_partition_simulator(Seed) ->
    _ = (catch machi_partition_simulator:stop()),
    {ok, _} = machi_partition_simulator:start_link(Seed, 100, 100),
    ok.

