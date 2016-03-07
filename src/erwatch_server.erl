% Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
% All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:

% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.

% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.

% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.

% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(erwatch_server).
-behaviour(gen_server).

-export([start_link/3,
         start_link/4,
         pause/1,
         resume/1,
         set_interval/2,
         get_changes/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(DEFAULT_INTERVAL, 0).

-record(state, {parent,
                ref,
                wildcards = [],
                path_diffs = [],
                interval = 0,
                opt_interval = ?DEFAULT_INTERVAL,
                registered = false}).

%% == API

start_link(Parent, Ref, Wildcards) ->
    start_link(Parent, Ref, Wildcards, []).

start_link(Parent, Ref, Wildcards, Opts) ->
    gen_server:start_link(?MODULE, [Parent, Ref, Wildcards, Opts], []).

pause(Pid) ->
    gen_server:cast(Pid, pause).

resume(Pid) ->
    gen_server:cast(Pid, resume).

set_interval(Pid, Interval) ->
    gen_server:cast(Pid, {set_interval, Interval}).

get_changes(Pid) ->
    gen_server:call(Pid, get_changes).

%% == Callbacks

init([Parent, Ref, Wildcards, Opts]) ->
    Interval = get_interval(Opts),
    State = #state{parent = Parent,
                   ref = Ref,
                   wildcards = Wildcards,
                   interval = Interval,
                   opt_interval = Interval},
    {ok, State, 0}.

handle_call(get_changes, _From, #state{interval = Interval} = State) ->
    {ChangeSets, NewState} = scan_path_diffs(State),
    case Interval of
        0 ->
            {reply, ChangeSets, NewState};
        _ ->
            {reply, ChangeSets, NewState, Interval}
    end.

handle_cast(pause, State) ->
    NewState = State#state{interval = 0},
    {noreply, NewState};

handle_cast(resume, #state{opt_interval = Interval} = State) ->
    NewState = State#state{interval = Interval},
    noreply_timeout(NewState);

handle_cast({set_interval, Interval}, State) ->
    NewState = State#state{interval = Interval,
                           opt_interval = Interval},
    noreply_timeout(NewState).

handle_info(timeout, #state{registered = false,
                            ref = Ref,
                            wildcards = Wildcards} = State) ->
    erwatch_registry:add(Ref, self()),
    NewState = add_wildcard_entries(Wildcards, State),
    noreply_timeout(NewState#state{registered = true});

handle_info(timeout, State) ->
    {WildcardChangeSets, NewState} = scan_path_diffs(State),
    notify_parent(WildcardChangeSets, NewState),
    noreply_timeout(NewState).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{ref = Ref}) ->
    erwatch_registry:remove(Ref).

%% == Internal

noreply_timeout(#state{interval = Interval} = State) ->
    case Interval of
        0 -> {noreply, State};
        _ -> {noreply, State, Interval}
    end.

get_interval(Opts) ->
    proplists:get_value(interval, Opts, ?DEFAULT_INTERVAL).

add_wildcard_entries(Wildcards, State) ->
    F = fun(W) ->
        PD1 = path_diff:new(W),
        {_, PD2} = path_diff:run(PD1),
        PD2
    end,
    PathDiffs = lists:map(F, Wildcards) ,
    State#state{path_diffs = PathDiffs}.

scan_path_diffs(#state{path_diffs = PathDiffs} = State) ->
    F = fun(PathDiff, {ChangeSets, PDs}) ->
        case path_diff:run(PathDiff) of
            {[], _} ->
                {ChangeSets, [PathDiff | PDs]};
            {ChangeSet, NewPathDiff} ->
                {[ChangeSet | ChangeSets], [NewPathDiff | PDs]}
        end
    end,
    {ChangeSets, NewPathDiffs} = lists:foldl(F, {[], []}, PathDiffs),
    NewState = State#state{path_diffs = NewPathDiffs},
    {lists:flatten(ChangeSets), NewState}.

notify_parent(ChangeSets, #state{parent = Parent,
                                 ref = Ref}) ->
    case ChangeSets of
        [] -> ok;
        _ ->
            Parent ! {erwatch@changes, {erwatch@ref, Ref}, ChangeSets}
    end.
