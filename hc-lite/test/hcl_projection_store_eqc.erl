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

-module(hcl_projection_store_eqc).

-ifdef(TEST).
-ifdef(EQC).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

-define(MUT, hcl_projection_store).             % Module Under Test
-define(TESTDIR, "./eqc-test-data-dir").

-record(state, {
          name             :: atom(),
          pid              :: 'undefined' | pid(),
          dict=dict:new()  :: dict:dict()
         }).

%% EUNIT TEST DEFINITION
eqc_test_() ->
    PropTimeout = case os:getenv("EQC_TIME") of
                      false -> 1;
                      V     -> list_to_integer(V)
                  end,
    {timeout, PropTimeout*2 + 30,
     {spawn,
      [
       ?_assertEqual(true, eqc:quickcheck(eqc:testing_time(PropTimeout, ?QC_OUT(
prop_ok()))))
      ]
     }}.

%% SHELL HELPERS
test() ->
    test({num,100}).

test({num,N}) ->
    eqc:quickcheck(eqc:numtests(N, prop_ok()));
test({time,T}) ->
    eqc:quickcheck(eqc:testing_time(T, prop_ok())).

check() ->
    eqc:check(prop_ok(), eqc:current_counterexample()).

check(CounterExample) ->
    eqc:check(prop_ok(), CounterExample).

prop_ok() ->
    filelib:ensure_dir(?TESTDIR ++ "/unused-file-name"),
    Name = sut,
    catch cleanup_stop(Name),
    cleanup(),
    ?FORALL(Cmds, parallel_commands(?MODULE, initial_state(Name)),
            begin
                {H, PH, Res} = run_parallel_commands(?MODULE, Cmds),
                catch cleanup_stop(Name),
                cleanup(),
                pretty_commands(?MODULE, Cmds, {H, PH, Res},
                                aggregate(command_names(Cmds), Res == ok))
            end).

%% GENERATORS etc

initial_state(Name) ->
    #state{name=Name
          }.
weight(_S, write)               ->  4;
weight(_S, read)                ->  4;
weight(_S, get_latest_epochid)  ->  4;
weight(_S, _)                   ->  1.

gen_config() ->
    make_ref().

gen_epoch() ->
    ?LET(I, int(), erlang:abs(I) + 1).

gen_priv_pub() ->
    oneof([public, private]).

%% COMMANDs etc

start_pre(#state{pid=Pid}) ->
    Pid == undefined.

start_args(#state{name=Name}) ->
    [Name].

start(Name) ->
    io:format(user, "S", []),
    {ok, Pid} = ?MUT:start_link(Name, ?TESTDIR),
    Pid.

start_next(S, Pid, _) ->
    S#state{pid=Pid}.

stop_pre(#state{pid=Pid}) ->
    Pid /= undefined.

stop_args(#state{name=Name}) ->
    [Name].

stop(Name) ->
    io:format(user, "s", []),
    ok = ?MUT:stop(Name).

stop_next(S, _Res, _) ->
    S#state{pid=undefined}.

write_pre(#state{pid=Pid}) ->
    Pid /= undefined.

write_args(#state{name=Name}) ->
    Epoch = gen_epoch(),
    ConfigType = gen_priv_pub(),
    Config = gen_config(),
    [Name, ConfigType, Epoch, Config].

write(Name, ConfigType, Epoch, Config) ->
    io:format(user, "w", []),
    ?MUT:write(Name, ConfigType, Epoch, Config).

write_post(S, [Name, ConfigType, Epoch, _Config], ok) ->
    not is_written(S, Name, ConfigType, Epoch);
write_post(S, [Name, ConfigType, Epoch, _Config], written) ->
    is_written(S, Name, ConfigType, Epoch).

write_next(S = #state{}, _Val, [Name, ConfigType, Epoch, Config]) ->
    case is_written(S, Name, ConfigType, Epoch) of
        true ->
            S;
        false ->
            dict_add(S, Name, ConfigType, Epoch, Config)
    end.

read_pre(#state{pid=Pid}) ->
    Pid /= undefined.

read_args(#state{name=Name}) ->
    Epoch = gen_epoch(),
    ConfigType = gen_priv_pub(),
    [Name, ConfigType, Epoch].

read(Name, ConfigType, Epoch) ->
    io:format(user, "r", []),
    ?MUT:read(Name, ConfigType, Epoch).

read_post(S, [Name, ConfigType, Epoch], {ok, V}) ->
    is_written(S, Name, ConfigType, Epoch) andalso
        get_value(S, Name, ConfigType, Epoch) == V;
read_post(S, [Name, ConfigType, Epoch], not_written) ->
    not is_written(S, Name, ConfigType, Epoch).

read_next(S = #state{}, _Val, [Name, ConfigType, Epoch]) ->
    S.

get_latest_epochid_pre(#state{pid=Pid}) ->
    Pid /= undefined.

get_latest_epochid_args(#state{name=Name}) ->
    ConfigType = gen_priv_pub(),
    [Name, ConfigType].

get_latest_epochid(Name, ConfigType) ->
    io:format(user, "g", []),
    ?MUT:get_latest_epochid(Name, ConfigType).

get_latest_epochid_post(S, [Name, ConfigType], {ok, -1}) ->
    [] == get_written_epochs(S, ConfigType);
get_latest_epochid_post(S, [Name, ConfigType], {ok, Epoch}) ->
    lists:max(get_written_epochs(S, ConfigType)) == Epoch.

get_latest_epochid_next(S = #state{}, _Val, [Name, ConfigType]) ->
    S.

%%%%%%%%%%%%%%%%%%%%%%%%%

is_written(#state{dict=D}, Name, ConfigType, Epoch) ->
    dict:is_key({Name, ConfigType, Epoch}, D).

get_value(#state{dict=D}, Name, ConfigType, Epoch) ->
    dict:fetch({Name, ConfigType, Epoch}, D).

get_written_epochs(#state{dict=D}, ConfigType) ->
    lists:sort([Epoch ||
                   {{_Name, CT, Epoch}, _Val} <- dict:to_list(D),
                   CT == ConfigType]).

dict_add(S = #state{dict=D}, Name, ConfigType, Epoch, Val) ->
    S#state{dict=dict:store({Name, ConfigType, Epoch}, Val, D)}.

cleanup() ->
    [file:delete(F) || F <- filelib:wildcard(?TESTDIR ++ "/*")],
    ok.

cleanup_stop(Name) ->
    stop(Name).

-endif. % EQC
-endif. % TEST