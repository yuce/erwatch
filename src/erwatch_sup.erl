-module(erwatch_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% == API

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% == Callbacks

init([]) ->
    ErwatchRegistrySpec = #{
        id => erwatch_registry,
        start => {erwatch_registry, start_link, []},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [erwatch_registry]},
    ErwatchServerSupSpec = #{
        id => erwatch_server_sup,
        start => {erwatch_server_sup, start_link, []},
        restart => permanent,
        shutdown => 1000,
        type => supervisor,
        modules => [erwatch_server_sup]},
    SupSpec = {{rest_for_one, 10, 60}, [ErwatchRegistrySpec,
                                        ErwatchServerSupSpec]},
    {ok, SupSpec}.
