#! /usr/bin/env escript
%%! -pa _build/default/lib/erwatch/ebin

main([]) ->
    io:format("Usage: watch.escript path1 [path2, path3, ...]~n");

main(Paths) ->
    case ensure_exists(Paths) of
        true ->
            lists:foreach(fun(P) -> io:format("Watching ~s~n", [P]) end, Paths),
            application:ensure_all_started(erwatch),
            _Watch = create_watch(Paths),
            loop();
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
    {ok, Watch} = erwatch:new([{interval, 1000}]),
    lists:foreach(fun(P) -> add_wildcard(P, Watch) end, Paths),
    Watch.

add_wildcard(Path, Watch) ->
    erwatch:add_wildcard(Path, Watch),
    case filelib:is_dir(Path) of
        true -> erwatch:add_wildcard(Path ++ "/**", Watch);
        _ -> ok
    end.

loop() ->
    receive
        {erwatch@changes, _Watch, Changes} ->
            print_changes(Changes),
            loop();
        Other ->
            io:format("Received unknown message: ~p~n", [Other]),
            loop()
    end.

print_changes(Changes) ->
    lists:foreach(fun(C) -> print_change(C) end, Changes).

print_change({added, Path}) ->
    io:format("+ ~s~n", [Path]);

print_change({updated, Path}) ->
    io:format("* ~s~n", [Path]);

print_change({deleted, Path}) ->
    io:format("- ~s~n", [Path]).
