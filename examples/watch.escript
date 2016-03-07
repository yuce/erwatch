#! /usr/bin/env escript
%%! -pa _build/default/lib/erwatch/ebin -pa _build/default/lib/simpre/ebin

main([]) ->
    io:format("Usage: watch.escript path1 [path2, path3, ...]~n");

main(Paths) ->
    case ensure_exists(Paths) of
        true ->
            lists:foreach(fun(P) -> io:format("Watching ~s~n", [P]) end, Paths),
            application:ensure_all_started(erwatch),
            Watch = create_watch(Paths),
            loop(Watch);
        _ ->
            io:format("Check your paths.")
    end.

ensure_exists(Paths) ->
    F = fun(P, AllExists) ->
        case filelib:is_file(P) of
            true -> AllExists and true;
            _ ->
                io:format("~s does not exist or is not a regular file.~n", [P]),
                false
        end
    end,
    lists:foldl(F, true, Paths).

create_watch(Paths) ->
    NewPaths = lists:map(fun(P) -> wildcard(P) end, Paths),
    {ok, Watch} = erwatch:new(NewPaths, [{interval, 1000}]),
    Watch.

wildcard(Path) ->
    case filelib:is_dir(Path) of
        true -> Path ++ "/**";
        _ -> Path
    end.

loop(Watch) ->
    receive
        {erwatch@changes, Watch, Changes} ->
            print_changes(Changes),
            loop(Watch);
        Other ->
            io:format("Received unknown message: ~p~n", [Other]),
            loop(Watch)
    end.

print_changes(Changes) ->
    lists:foreach(fun(C) -> print_change(C) end, Changes).

print_change({added, Path}) ->
    io:format("+ ~s~n", [Path]);

print_change({updated, Path}) ->
    io:format("* ~s~n", [Path]);

print_change({deleted, Path}) ->
    io:format("- ~s~n", [Path]).
