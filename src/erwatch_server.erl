-module(erwatch_server).
-behaviour(gen_server).

-export([start_link/2,
         start_link/3,
         add_wildcard/2,
         pause/1,
         resume/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(DEFAULT_INTERVAL, 5000).

-record(state, {parent,
                ref,
                path_diffs = [],
                interval = ?DEFAULT_INTERVAL,
                opt_interval = ?DEFAULT_INTERVAL,
                registered = false}).

%% == API

start_link(Parent, Ref) ->
    start_link(Parent, Ref, []).

start_link(Parent, Ref, Opts) ->
    gen_server:start_link(?MODULE, [Parent, Ref, Opts], []).

add_wildcard(Pid, Wildcard) ->
    gen_server:cast(Pid, {add_wildcard, Wildcard}).

pause(Pid) ->
    gen_server:cast(Pid, pause).

resume(Pid) ->
    gen_server:cast(Pid, resume).

%% == Callbacks

init([Parent, Ref, Opts]) ->
    Interval = get_interval(Opts),
    State = #state{parent = Parent,
                   ref = Ref,
                   interval = Interval,
                   opt_interval = Interval},
    {ok, State, 0}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({add_wildcard, Wildcard}, State) ->
    {ChangeSet, NewState} = add_wildcard_entry(Wildcard, State),
    notify_parent_initial(ChangeSet, State),
    noreply_timeout(NewState);

handle_cast(pause, State) ->
    NewState = State#state{interval = 0},
    {noreply, NewState};

handle_cast(resume, #state{opt_interval = Interval} = State) ->
    NewState = State#state{interval = Interval},
    noreply_timeout(NewState).

handle_info(timeout, #state{registered = false,
                            ref = Ref} = State) ->
    erwatch_registry:add(Ref, self()),
    noreply_timeout(State#state{registered = true});

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

add_wildcard_entry(Wildcard, #state{path_diffs = PathDiffs} = State) ->
    PathDiff = path_diff:new(Wildcard),
    {InitialChangeSet, NewPathDiff} = path_diff:run(PathDiff),
    NewPathDiffs = [NewPathDiff | PathDiffs],
    {InitialChangeSet, State#state{path_diffs = NewPathDiffs}}.

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
            Parent ! {erwatch@changes, {watch, Ref}, ChangeSets}
    end.

notify_parent_initial(ChangeSet, #state{parent = Parent,
                                        ref = Ref}) ->
    Parent ! {erwatch@initial, {watch, Ref}, ChangeSet}.