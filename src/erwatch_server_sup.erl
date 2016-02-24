-module(erwatch_server_sup).
-behaviour(supervisor).

-export([start_link/0,
         start_child/3,
         stop_child/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Parent, Ref, Opts) ->
    ChildSpec = #{id => Ref,
                  start => {erwatch_server, start_link, [Parent, Ref, Opts]},
                  restart => transient,
                  shutdown => 1000,
                  modules => [erwatch_server]},
    supervisor:start_child(?SERVER, ChildSpec).

stop_child(Pid) ->
    supervisor:terminate_child(?SERVER, Pid),
    supervisor:delete_child(?SERVER, Pid),
    ok.


init([]) ->
    SupSpec = {{one_for_one, 10, 60}, []},
    {ok, SupSpec}.
