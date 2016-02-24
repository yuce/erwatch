-module(erwatch).

-export([new/0,
         new/1,
         add_wildcard/2,
         remove/1]).

-type watch() :: {watch, reference()}.

-export_type([watch/0]).

new() ->
    new([]).

-spec new(Opts :: list()) -> {ok, watch()}.
new(Opts) ->
    Ref = make_ref(),
    {ok, _Pid} = erwatch_server_sup:start_child(self(), Ref, Opts),
    {ok, {watch, Ref}}.

-spec add_wildcard(Watch :: watch(), Wildcard :: string()) ->
    ok | {error, not_found}.
add_wildcard({watch, Ref}, Wildcard) ->
    run_ref(fun(Pid) -> erwatch_server:add_wildcard(Pid, Wildcard) end, Ref).

-spec remove(Watch :: watch()) ->
    ok | {error, not_found}.
remove({watch, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server_sup:stop_child(Pid) end, Ref).

run_ref(Fun, Ref) ->
    case erwatch_registry:get_pid(Ref) of
        not_found ->
            {error, not_found};
        {ok, Pid} ->
            Fun(Pid)
    end.
