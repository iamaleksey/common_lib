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
-module(cl_consumer).
-behaviour(gen_server).

%%% INCLUDE FILES

%%% START/STOP EXPORTS
-export([start_link/3, stop/1]).

%%% RPS EXPORTS
-export([pause/1, resume/1, rps/1, rps/2]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE MESSAGES EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% INTERNAL EXPORTS
-export([consumer_loop/3]).

%%% MACROS
% Setting a higher number of RPS per worker process will lead to less precise
% RPS due to the deviations in the schedulling and timer functions.  At most
% 40 RPS per process could be set, but keeping it low in 25 provides less
% deviations on loaded systems.  Setting to high the priority for the worker
% processes also leads to more accurate RPS, but it is also more dangerous.
-define(MAX_RPS(Rps), if Rps < 25 -> Rps; true -> 25 end).  % Per worker

%%% RECORDS
-record(st, {queue_srv, req_fun, rps, consumers = []}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link(QueueSrv, ReqFun, Rps) ->
    gen_server:start_link(?MODULE, [QueueSrv, ReqFun, Rps], []).


stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

%%%-----------------------------------------------------------------------------
%%% RPS EXPORTS
%%%-----------------------------------------------------------------------------
pause(Pid) ->
    gen_server:call(Pid, pause, infinity).


resume(Pid) ->
    gen_server:cast(Pid, resume).


rps(Pid) ->
    gen_server:call(Pid, rps, infinity).


rps(Pid, Rps) ->
    gen_server:cast(Pid, {rps, Rps}).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([QueueSrv, ReqFun, Rps]) ->
    L = init_consumers(QueueSrv, ReqFun, Rps),
    {ok, #st{queue_srv = QueueSrv, req_fun = ReqFun, rps = Rps, consumers = L}}.


terminate(_Reason, St) ->
    lists:foreach(fun(X) -> consumer_stop(X) end, St#st.consumers).

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call(pause, _From, #st{consumers = []} = St) ->
    {reply, ok, St};
handle_call(pause, _From, St) ->
    lists:foreach(fun(X) -> consumer_stop(X) end, St#st.consumers),
    {reply, ok, St#st{consumers = []}};
handle_call(rps, _From, St) ->
    {reply, St#st.rps, St};
handle_call(stop, _From, St) ->
    {stop, normal, ok, St}.


handle_cast(resume, #st{consumers = []} = St) ->
    L = init_consumers(St#st.queue_srv, St#st.req_fun, St#st.rps),
    {noreply, St#st{consumers = L}};
handle_cast(resume, St) ->
    {noreply, St};
handle_cast({rps, Rps}, #st{consumers = []} = St) ->
    {noreply, St#st{rps = Rps}};
handle_cast({rps, Rps}, St) ->
    lists:foreach(fun(X) -> consumer_stop(X) end, St#st.consumers),
    L = init_consumers(St#st.queue_srv, St#st.req_fun, Rps),
    {noreply, St#st{rps = Rps, consumers = L}}.


handle_info(_Info, St) ->
    {noreply, St}.

%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%-----------------------------------------------------------------------------
%%% CONSUMER FUNCTIONS
%%%-----------------------------------------------------------------------------
consumer(QueueSrv, ReqFun, Time, Delay) ->
    spawn_link(fun() -> consumer_init(QueueSrv, ReqFun, Time, Delay) end).


consumer_init(QueueSrv, ReqFun, Time, Delay) ->
%    process_flag(priority, high),   % Only if very precise RPS is required
    timer:sleep(Delay),
    consumer_loop(QueueSrv, ReqFun, Time).


consumer_loop(QueueSrv, ReqFun, Time) ->
    erlang:send_after(Time, self(), continue),
    ok = consumer_req(QueueSrv, ReqFun),
    ok = consumer_wait(),
    ?MODULE:consumer_loop(QueueSrv, ReqFun, Time).


consumer_req(QueueSrv, ReqFun) ->
    case cl_queue_srv:out(QueueSrv, 1) of
        [] ->
            ok;
        [Item] ->
            ReqFun(Item),
            ok
    end.


consumer_stop(Pid) ->
    Pid ! stop.


consumer_wait() ->
    receive
        continue ->
            ok;
        stop ->
            exit(normal);
        _Other ->
            consumer_wait()
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
init_consumers(_QueueSrv, _ReqFun, 0) ->
    [];
init_consumers(QueueSrv, ReqFun, Rps) ->
    Gap = 1000 div (Rps div ?MAX_RPS(Rps)),
    init_consumers(QueueSrv, ReqFun, Rps, Gap, []).

init_consumers(_QueueSrv, _ReqFun, 0, _Gap, Acc) ->
    Acc;
init_consumers(QueueSrv, ReqFun, Rps, Gap, Acc) ->
    Pid = consumer(QueueSrv, ReqFun, 1000 div ?MAX_RPS(Rps), Gap * length(Acc)),
    init_consumers(QueueSrv, ReqFun, Rps - ?MAX_RPS(Rps), Gap, [Pid | Acc]).

