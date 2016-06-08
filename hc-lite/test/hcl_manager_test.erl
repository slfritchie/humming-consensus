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

-module(hcl_manager_test).

-ifdef(TEST).
-ifdef(EQC).
-compile(export_all).

-include("hcl_manager.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(M, hcl_manager).

smoke_test() ->
    {false, undefined, [b], [a]} =
        ?M:classify_latest_projections([{a, {error,timeout}},
                                        {b, not_written}], [a,b]),
    Ps = [a,b,c],
    C1a = #config_w{epoch=1, participants=Ps,      opaque=a},
    C1b = #config_w{epoch=1, participants=Ps,      opaque=b},
    C1x = #config_w{epoch=1, participants=Ps++[d], opaque=a},
    C2a = #config_w{epoch=2, participants=Ps,      opaque=a},
    C2b = #config_w{epoch=2, participants=Ps,      opaque=b},
    C2x = #config_w{epoch=2, participants=Ps++[d], opaque=a},

    {false, C1a, [b,c], []} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, not_written},
                                        {c, not_written}], [a,b,c]),
    {false, _, [c], []} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, {ok,C1b}},
                                        {c, not_written}], [a,b,c]),
    {false, _, [], [c]} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, {ok,C1b}},
                                        {c, {error,timeout}}], [a,b,c]),
    {true, C1a, [], [c]} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, {ok,C1a}},
                                        {c, {error,timeout}}], [a,b,c]),
    %% If *any* results are not_written, then unanimous calc must be false.
    {false, C1a, [c], []} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, {ok,C1a}},
                                        {c, not_written}], [a,b,c]),

    {false, C2a, [c], []} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, {ok,C2a}},
                                        {c, not_written}], [a,b,c]),
    %% TODO: fix this test failure!
    {false, C2a, [], []} =
        ?M:classify_latest_projections([{a, {ok,C1a}},
                                        {b, {ok,C2a}},
                                        {c, {ok,C2a}}], [a,b,c]),
    {true, C2b, [], []} =
        ?M:classify_latest_projections([{a, {ok,C1b}},
                                        {b, {ok,C2b}},
                                        {c, {ok,C2b}}], [a,b,c]),

    ok.

-endif. % EQC
-endif. % TEST
