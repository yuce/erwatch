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

-module(erwatch).

-export([new/0,
         new/1,
         add_wildcard/2,
         pause/1,
         resume/1,
         remove/1,
         set_interval/2,
         get_changes/1]).

-type watch() :: {erwatch@ref, reference()}.

-export_type([watch/0]).

new() ->
    new([]).

-spec new(Opts :: list()) -> {ok, watch()}.
new(Opts) ->
    Ref = make_ref(),
    {ok, _Pid} = erwatch_server_sup:start_child(self(), Ref, Opts),
    {ok, {erwatch@ref, Ref}}.

-spec add_wildcard(Wildcard :: string(), Watch :: watch()) ->
    ok | {error, not_found}.
add_wildcard(Wildcard, {erwatch@ref, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:add_wildcard(Pid, Wildcard) end, Ref).

-spec pause(Watch :: watch()) -> ok.
pause({erwatch@ref, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:pause(Pid) end, Ref).

-spec resume(Watch :: watch()) -> ok.
resume({erwatch@ref, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:resume(Pid) end, Ref).

-spec remove(Watch :: watch()) ->
    ok | {error, not_found}.
remove({erwatch@ref, Ref}) ->
    erwatch_registry:remove(Ref),
    erwatch_server_sup:stop_child(Ref).

-spec set_interval(Interval :: non_neg_integer(), Watch :: watch()) -> ok.
set_interval(Interval, {erwatch@ref, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:set_interval(Pid, Interval) end, Ref).

-spec get_changes(Watch :: watch()) ->
    [path_diff:changeset()].
get_changes({erwatch@ref, Ref}) ->
    run_ref(fun(Pid) -> erwatch_server:get_changes(Pid) end, Ref).

run_ref(Fun, Ref) ->
    case erwatch_registry:get_pid(Ref) of
        not_found ->
            {error, not_found};
        {ok, Pid} ->
            Fun(Pid)
    end.
