%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Scott Lystig Fritchie.  All Rights Reserved.
%% Copyright (c) 2007-2015 Basho Technologies, Inc.  All Rights Reserved.
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

-module(hcl_projection_store).

-export([start_link/2, stop/1,
         write/4, write/5,
         read/3, read/4,
         get_latest_epochid/2, get_latest_epochid/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-behavior(gen_server).

-record(state, {
          regname         :: atom(),
          dets_path = ""  :: string(),
          dets            :: file:name(),
          max_public_epochid :: -1 | non_neg_integer(),
          max_private_epochid :: -1 | non_neg_integer()
         }).

start_link(RegName, DataDir) ->
    gen_server:start_link({local, RegName}, ?MODULE, [RegName, DataDir], []).

stop(RegName) ->
    g_call(RegName, {stop}, infinity).

%% @doc Fetch the latest epoch number + checksum for type `COnfigType'.
%% projection.

get_latest_epochid(PidSpec, ConfigType) ->
    get_latest_epochid(PidSpec, ConfigType, infinity).

%% @doc Fetch the latest epoch number + checksum for type `COnfigType'.
%% projection.

get_latest_epochid(PidSpec, ConfigType, Timeout)
  when ConfigType == 'public' orelse ConfigType == 'private' ->
    g_call(PidSpec, {get_latest_epochid, ConfigType}, Timeout).

%% @doc Write the projection record type `ConfigType' for epoch number `Epoch' .

write(PidSpec, ConfigType, Epoch, Config) ->
    write(PidSpec, ConfigType, Epoch, Config, infinity).

%% @doc Write the projection record type `ConfigType' for epoch number `Epoch' .

write(PidSpec, ConfigType, Epoch, Config, Timeout)
  when ConfigType == 'public' orelse ConfigType == 'private',
       is_integer(Epoch), Epoch > 0 ->
    g_call(PidSpec, {write, ConfigType, Epoch, Config}, Timeout).

%% @doc Write the projection record type `ConfigType' for epoch number `Epoch' .

read(PidSpec, ConfigType, Epoch) ->
    read(PidSpec, ConfigType, Epoch, infinity).

%% @doc Write the projection record type `ConfigType' for epoch number `Epoch' .

read(PidSpec, ConfigType, Epoch, Timeout)
  when ConfigType == 'public' orelse ConfigType == 'private',
       is_integer(Epoch), Epoch > 0 ->
    g_call(PidSpec, {read, ConfigType, Epoch}, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%

init([RegName, DataDir]) ->
    lclock_init(),
    Dets = flat_str("pstore_~s", [RegName]),
    DetsPath = flat_str("~s/~s", [DataDir, Dets]),
    {ok, Dets} = dets:open_file(Dets, [{file, DetsPath},
                                       {access, read_write},
                                       {keypos, 1},
                                       {repair, true},
                                       {type, set}]),
    {MaxPublic, MaxPrivate} =
        dets:foldl(fun({{public,E}, _V}, {MaxPub, MaxPriv}) when E > MaxPub ->
                           {E, MaxPriv};
                      ({{private,E}, _V}, {MaxPub, MaxPriv}) when E > MaxPriv ->
                           {MaxPub, E};
                      (_, Acc) ->
                           Acc
                   end, {-1, -1}, Dets),
    {ok, #state{regname=RegName,
                dets_path=DetsPath,
                dets=Dets,
                max_public_epochid=MaxPublic,
                max_private_epochid=MaxPrivate}}.

flat_str(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

handle_call({{get_latest_epochid, ConfigType}, LC1}, _From, S) ->
    LC2 = lclock_update(LC1),
    EpochId = if ConfigType == public  -> S#state.max_public_epochid;
                 ConfigType == private -> S#state.max_private_epochid
             end,
    {reply, {{ok, EpochId}, LC2}, S};
handle_call({{write, ConfigType, Epoch, Config}, LC1}, _From, S) ->
    LC2 = lclock_update(LC1),
    {Reply, S2} = do_write(ConfigType, Epoch, Config, S),
    {reply, {Reply, LC2}, S2};
handle_call({{read, ConfigType, Epoch}, LC1}, _From, S) ->
    LC2 = lclock_update(LC1),
    {Reply, S2} = do_read(ConfigType, Epoch, S),
    {reply, {Reply, LC2}, S2};
handle_call({{stop}, LC1}, _From, State) ->
    LC2 = lclock_update(LC1),
    {stop, normal, {ok, LC2}, State}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%%%%%%%%%%%%%%%%%%%%%%%

do_write(ConfigType, Epoch, Config,
         S = #state{dets=Dets,
                    max_public_epochid=MaxPublic,
                    max_private_epochid=MaxPrivate}) ->
    %% Sanity check, for debugging use only.
    %%
    %% Key = if Epoch == 8 -> zoo; true -> {ConfigType, Epoch} end,
    Key = {ConfigType, Epoch},
    case dets:lookup(Dets, Key) of
        {error, _} ->
            {error, S};
        [] ->
            ok = dets:insert(Dets, {Key, Config}),
            if ConfigType == public, Epoch > MaxPublic ->
                    {ok, S#state{max_public_epochid=Epoch}};
               ConfigType == private, Epoch > MaxPrivate ->
                    {ok, S#state{max_private_epochid=Epoch}};
               true ->
                    {ok, S}
            end;
        [_|_] ->
            {written, S}
    end.

do_read(ConfigType, Epoch, S = #state{dets=Dets}) ->
    %% Sanity check, for debugging use only.
    %%
    %% Key = if Epoch == 7 -> zoo; true -> {ConfigType, Epoch} end,
    Key = {ConfigType, Epoch},
    case dets:lookup(Dets, Key) of
        {error, _} ->
            {error, S};
        [] ->
            {not_written, S};
        [{_, Val}] ->
            {{ok, Val}, S}
    end.

g_call(PidSpec, Arg, Timeout) ->
    LC1 = lclock_get(),
    {Res, LC2} = gen_server:call(PidSpec, {Arg, LC1}, Timeout),
    lclock_update(LC2),
    Res.

%%-ifdef(TEST).
-ifndef(TEST).

lclock_init() ->
    lamport_clock:init().

lclock_get() ->
    lamport_clock:get().

lclock_update(LC) ->
    lamport_clock:update(LC).

%% testing_sleep_perhaps() ->
%%     try
%%         [{_,Max}] = ets:lookup(?TEST_ETS_TABLE, projection_store_sleep_time),
%%         MSec = random:uniform(Max),
%%         io:format(user, "{", []),
%%         timer:sleep(MSec),
%%         io:format(user, "}", []),
%%         ok
%%     catch _X:_Y ->
%%             ok
%%     end.

-else.  % TEST

lclock_init() ->
    ok.

lclock_get() ->
    ok.

lclock_update(_LC) ->
    ok.

%% testing_sleep_perhaps() ->
%%     ok.

-endif. % TEST

