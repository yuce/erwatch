-module(erwatch_registry).
-behaviour(gen_server).

-export([start_link/0,
         add/2,
         remove/1,
         get_pid/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%% == API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Ref, Pid) ->
    gen_server:cast(?MODULE, {add, Ref, Pid}).

remove(Ref) ->
    gen_server:cast(?MODULE, {remove, Ref}).

get_pid(Ref) ->
    gen_server:call(?MODULE, {get_pid, Ref}).

%% == Callbacks

init([]) ->
    Ets = ets:new(?MODULE, []),
    {ok, Ets}.

handle_call({get_pid, Ref}, _From, Ets) ->
    Reply = case ets:lookup(Ets, Ref) of
        [] -> not_found;
        [{Ref, Pid}] -> {ok, Pid}
    end,
    {reply, Reply, Ets}.

handle_cast({add, Ref, Pid}, Ets) ->
    true = ets:insert(Ets, {Ref, Pid}),
    {noreply, Ets};

handle_cast({remove, Ref}, Ets) ->
    true = ets:delete(Ets, Ref),
    {noreply, Ets}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.