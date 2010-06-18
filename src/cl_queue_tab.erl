%%% Copyright (C) 2009 Enrique Marcote, Miguel Rodriguez
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%
%%% o Redistributions of source code must retain the above copyright notice,
%%%   this list of conditions and the following disclaimer.
%%%
%%% o Redistributions in binary form must reproduce the above copyright notice,
%%%   this list of conditions and the following disclaimer in the documentation
%%%   and/or other materials provided with the distribution.
%%%
%%% o Neither the name of ERLANG TRAINING AND CONSULTING nor the names of its
%%%   contributors may be used to endorse or promote products derived from this
%%%   software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
-module(cl_queue_tab).

%%% EXTERNAL EXPORTS
-export([delete/0, delete/1, insert/1, insert/2, lookup/0, lookup/1, new/0]).

%%% INTERNAL EXPORTS
-export([monitor_loop/4]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
delete() ->
    delete(self()).

delete(Owner) ->
    case ets:lookup(?MODULE, Owner) of
        [{Owner, _QueueSrv, Monitor}] ->
            exit(Monitor, kill),
            true = ets:delete(?MODULE, Owner);
        [] ->
            true
    end.


insert(QueueSrv) ->
    insert(self(), QueueSrv).

insert(Owner, QueueSrv) ->
    Monitor = spawn(fun() -> monitor(Owner, QueueSrv) end),
    ets:insert(?MODULE, {Owner, QueueSrv, Monitor}).


lookup() ->
    lookup(self()).

lookup(Owner) ->
    case ets:lookup(?MODULE, Owner) of
        [{Owner, QueueSrv, _Monitor}] ->
            QueueSrv;
        [] ->
            undefined
    end.


new() ->
    ets:new(?MODULE, [named_table, public, ordered_set]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
monitor(Owner, QueueSrv) ->
    OwnerRef = erlang:monitor(process, Owner),
    QueueSrvRef = erlang:monitor(process, QueueSrv),
    monitor_loop(Owner, OwnerRef, QueueSrv, QueueSrvRef).


monitor_loop(Owner, OwnerRef, QueueSrv, QueueSrvRef) ->
    receive
        {'DOWN', OwnerRef, process, Owner, _Info} ->
            true = ets:delete(?MODULE, Owner);
        {'DOWN', QueueSrvRef, process, QueueSrv, _Info} ->
            case ets:match(?MODULE, {'$1', QueueSrv, self()}) of
                [[Owner]] ->
                    true = ets:delete(?MODULE, Owner);
                _NotFound ->
                    true
            end;
        _Other ->
            ?MODULE:monitor_loop(Owner, OwnerRef, QueueSrv, QueueSrvRef)
    end.
