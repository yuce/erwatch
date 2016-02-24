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