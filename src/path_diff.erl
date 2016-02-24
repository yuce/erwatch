-module(path_diff).

-export([new/1,
         run/1]).

-type wildcard() :: string().
-type path_infos() :: #{string() => calendar:datetime()}.
-type path_diff() :: {path_diff, wildcard(), path_infos() | [path_infos()]}.
-type changeset() :: {added | updated | deleted, string()}.

-export_type([wildcard/0,
              path_infos/0,
              path_diff/0,
              changeset/0]).

-spec new(Wildcard :: wildcard()) -> path_diff().

new(Wildcard) ->
    {path_diff, Wildcard, #{}}.

-spec run(PathDiff :: path_diff()) -> {[changeset()], path_diff()}.

run({path_diff, Wildcard, PathInfos}) ->
    Paths = filelib:wildcard(Wildcard),
    {ChangeSet, NewPathInfos} = diff_paths(Paths, PathInfos),
    {ChangeSet, {path_diff, Wildcard, NewPathInfos}}.

-spec diff_paths(Paths :: [string()], PathInfos :: path_infos()) ->
    {[changeset()], [path_infos()]}.

diff_paths(Paths, PathInfos) ->
    {ChangeSets1, PathInfos1} = deleted_paths(Paths, PathInfos),
    added_updated_paths(Paths, ChangeSets1, PathInfos1).

deleted_paths(Paths, PathInfos) ->
    % find deleted
    KnownPathsSet = sets:from_list(maps:keys(PathInfos)),
    PathsSet = sets:from_list(Paths),
    DeletedPaths = sets:to_list(sets:subtract(KnownPathsSet, PathsSet)),
    F = fun(Path) -> {deleted, Path} end,
    ChangeSets1 = lists:map(F, DeletedPaths),
    PathInfos1 = maps:without(DeletedPaths, PathInfos),
    {ChangeSets1, PathInfos1}.

added_updated_paths(Paths, ChangeSets, PathInfos) ->
    F = fun(Path, {CS, PIs}) ->
        case maps:get(Path, PathInfos, undefined) of
            undefined ->
                LastModified = filelib:last_modified(Path),
                NewPIs = maps:put(Path, LastModified, PIs),
                {[{added, Path} | CS], NewPIs};
            LastModified ->
                NewLastModified = filelib:last_modified(Path),
                case LastModified =/= NewLastModified of
                    true ->
                        NewPIs = maps:put(Path, NewLastModified, PIs),
                        {[{updated, Path} | CS], NewPIs};
                    _ ->
                        {CS, PIs}
                end
        end
    end,
    lists:foldl(F, {ChangeSets, PathInfos}, Paths).