-module(erwatch).

-export([new/0,
         new/1,
         add_wildcard/2,
         pause/1,
         resume/1,
         remove/1,
         set_interval/2,
         get_changes/1]).

-type watch() :: {watch, reference()}.

-export_type([watch/0]).

new() ->
    new([]).

-spec new(Opts :: list()) -> {ok, watch()}.
new(Opts) ->
    Ref = make_ref(),
    {ok, _Pid} = erwatch_server_sup:start_child(self(), Ref, Opts),
    {ok, {watch, Ref}}.

-spec add_wildcard(Wildcard :: string(), Watch :: watch()) ->
    ok | {error, not_found}.
add_wildcard(Wildcard, {watch, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:add_wildcard(Pid, Wildcard) end, Ref).

-spec pause(Watch :: watch()) -> ok.
pause({watch, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:pause(Pid) end, Ref).

-spec resume(Watch :: watch()) -> ok.
resume({watch, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:resume(Pid) end, Ref).

-spec remove(Watch :: watch()) ->
    ok | {error, not_found}.
remove({watch, Ref}) ->
    erwatch_server_sup:stop_child(Ref).

-spec set_interval(Interval :: non_neg_integer(), Watch :: watch()) -> ok.
set_interval(Interval, {watch, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:set_interval(Pid, Interval) end, Ref).

-spec get_changes(Watch :: watch()) ->
    [path_diff:changeset()].
get_changes({watch, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:get_changes(Pid) end, Ref).

run_ref(Fun, Ref) ->
    case erwatch_registry:get_pid(Ref) of
        not_found ->
            {error, not_found};
        {ok, Pid} ->
            Fun(Pid)
    end.
