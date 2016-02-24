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