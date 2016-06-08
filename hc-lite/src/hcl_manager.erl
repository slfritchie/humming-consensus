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

-module(hcl_manager).

%% @doc Humming Consensus manager library
%%
%% Or, rather, I think that it can be "merely" a library.  We'll see
%% if it can work out that way.
%%
%% In the "lite" version of Humming Consensus, we assume that we use
%% registered names for all HC actors/processes/agents/thingies.  (Unlike the
%% original Machi implementation which uses registered names strictly for
%% internal messaging only.)

-compile(export_all).

-ifdef(PULSE).
-compile({parse_transform, pulse_instrument}).
-include("/Users/fritchie/lib/eqc/include/pulse_otp.hrl").
-compile({pulse_replace_module, [{gen_server, pulse_gen_server}]}).
-compile({pulse_side_effect, [
                              {lamport_clock,'_','_'},
                              {gen_server,call,'_'}
                             ]}).
-endif. % PULSE

%% A config wrapper record.  The PaPOC HC paper claims that the
%% configuration can be an arbitrary thing.  However, if HC is going to be
%% able to manage the group membership/list of participants in HC and
%% perhaps change it, then the manager needs to be able to parse enough of
%% an otherwise-opaque configuration in order to find the group/participant
%% list definition.
%%
%% We will make a design decision here & now.  Many possible uses of HC
%% would like to manage group membership.  So, we're going to make that part
%% of the config a first class thing.  We put inside of an opaque term the
%% app-specific configuration data, etc.
%%
%% (TODO: I also predict that the simple list won't be sufficient for
%% dealing with group membership changes ... a version number or perhaps
%% CRDT to manage edits to the group will probably be necessary in a general
%% implementation?)
%%
%% For future work ... In practice, we need more than participant names,
%% which is what this list is.  We would also need DNS hostnames/IP address,
%% TCP ports, protocol(s) to use, encoding hints, authentication
%% information, yadda yadda.
%%
%% Today, we're going to use Erlang's built-in message passing.  Our
%% list of participants will be simple list of the participants' names.
%%
%% Today's assumptions about names:
%%
%% 1. The participant's name.  In this implementation, it's just a
%%    human-friendly atom, i.e., for non-essential stuff like event logging
%%    & debugging.  Because we're attempting to be merely a library, we will
%%    not assume that this name corresponds to any Erlang process or
%%    external OS process.
%%
%% 2. The participant's projection store's Erlang registered name.  We
%%    assume that the projection store is a separate actor (i.e., a separate
%%    Erlang process).  The projection store will be implemented as a
%%    gen_server (Erlang/OTP generic server) implemented by the
%%    `hcl_projection_store' module.
%%
%% 3. We assume that the projection store for any participant `x' will have
%%    the suffix `_pstore' appended to the participant's name.  E.g. Given a
%%    participant name `foo', we will assume that foo's projection store
%%    name is `foo_pstore'.

-include("hcl_manager.hrl").

new_flap_history() ->
    [].                                         % TODO

new_config(InitialParticipants) ->
    #config_w{epoch=0,
              participants=InitialParticipants}.

iterate_once(MyName, #config_w{} = C_cur, _Flap_Hist) ->
    #config_w{epoch=_E_cur, participants=Ps_cur} = C_cur,
    {_Unanimous_p, _C_latest,
     _Ps_need_repair, _Ps_down} = get_latest_config(MyName, Ps_cur).
    %% TODO Left off here.

get_latest_config(MyName, Ps) ->
    TODO_partitions = [],
    TODO_timeout = 5*1000,
    Rs = [{P, perhaps_call(MyName, P, TODO_partitions,
                           fun(Tgt) -> hcl_projection_store:read_latest_config(
                                         proj_store_name(Tgt), public,
                                         TODO_timeout)
                           end)} || P <- Ps],
    classify_latest_projections(Rs, Ps).

classify_latest_projections(Rs, Ps) ->
    Es = lists:usort([C#config_w.epoch || {_P, {ok, C}} <- Rs]),
    Ps_down = [P || {P, {error, _}} <- Rs],
    if Es == [] ->
            {false, undefined, Ps -- Ps_down, Ps_down};
       true ->
            MaxE = lists:max(Es),
            %% It doesn't matter which config @ MaxE that we choose;
            %% unanimity or not is more important.
            C_max = hd([C || {_P, {ok, C}} <- Rs, C#config_w.epoch == MaxE]),
            Ps_need_repair = [P || {P, not_written} <- Rs],
            Unanimous_p = case [C || {_P, {ok, C}} <- Rs,
                                     C#config_w.epoch == MaxE,
                                     C /= C_max] of
                              [] ->
                                  Ps_need_repair == [];
                              _ ->
                                  false
                          end,
            Ps_need_repair = [P || {P, not_written} <- Rs],
            {Unanimous_p, C_max, Ps_need_repair, Ps_down}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

perhaps_call(MyName, TheirName, Partitions, DoItFun) ->
    try
        perhaps_call2(MyName, TheirName, Partitions, DoItFun)
    catch
        exit:timeout ->
            update_remember_down_list(TheirName),
            {error, partition};
        exit:{timeout,_} ->
            update_remember_down_list(TheirName),
            {error, partition}
    end.

perhaps_call2(MyName, Partitions, TheirName, DoItFun) ->
    RemoteTheirName_p = TheirName /= MyName,
    erase(bad_sock),
    case RemoteTheirName_p andalso lists:member({MyName, TheirName}, Partitions) of
        false ->
            Their_PStore_Name = proj_store_name(TheirName),
            Res = DoItFun(Their_PStore_Name),
            if Res == {error, partition} ->
                    update_remember_down_list(TheirName);
               true ->
                    ok
            end,
            case RemoteTheirName_p andalso lists:member({TheirName, MyName}, Partitions) of
                false ->
                    Res;
                _ ->
                    (catch put(react, [{timeout2,me,MyName,to,TheirName,RemoteTheirName_p,Partitions}|get(react)])),
                    exit(timeout)
            end;
        true ->
            (catch put(react, [{timeout1,me,MyName,to,TheirName,RemoteTheirName_p,Partitions}|get(react)])),
            exit(timeout)
    end.

%% Why are we using the process dictionary for this?  In part because
%% we're lazy and in part because we don't want to clutter up the
%% return value of perhaps_call_t() in order to make perhaps_call_t()
%% a 100% pure function.

init_remember_down_list() ->
    put(remember_down_list, []).

update_remember_down_list(TheirName) ->
    catch put(remember_down_list,
              lists:usort([TheirName|get_remember_down_list()])).

get_remember_down_list() ->
    get(remember_down_list).

proj_store_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_pstore").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
