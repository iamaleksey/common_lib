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
-module(cl_queue_srv).
-behaviour(gen_server).

%%% START/STOP EXPORTS
-export([start_link/0, start_link/1, stop/1]).

%%% QUEUE EXPORTS
-export([in/3, len/1, out/1, out/2, out_r/1, out_r/2]).

%%% COUNTER EXPORTS
-export([count_in/1, count_out/1, count_reset/1]).

%%% RPS EXPORTS
-export([rps/1, rps_avg/1]).

%%% INIT/TERMINATE EXPORTS
-export([init/1, terminate/2]).

%%% HANDLE MESSAGES EXPORTS
-export([handle_call/3, handle_cast/2, handle_info/2]).

%%% CODE UPDATE EXPORTS
-export([code_change/3]).

%%% RECORDS
-record(st, {parent, queue, in = 0, out = 0, start = now()}).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
start_link() ->
    start_link(undefined).

start_link(File) ->
    gen_server:start(?MODULE, [self(), File], []).  % See init


stop(Pid) ->
    gen_server:call(Pid, stop).

%%%-----------------------------------------------------------------------------
%%% QUEUE EXPORTS
%%%-----------------------------------------------------------------------------
in(Pid, Item, Priority) ->
    gen_server:call(Pid, {in, Item, Priority}, infinity).


len(Pid) ->
    gen_server:call(Pid, len, infinity).


out(Pid) ->
    out(Pid, 1).

out(Pid, Num) when Num >= 0 ->
    gen_server:call(Pid, {out, Num}, infinity).


out_r(Pid) ->
    out_r(Pid, 1).

out_r(Pid, Num) when Num >= 0 ->
    gen_server:call(Pid, {out_r, Num}, infinity).

%%%-----------------------------------------------------------------------------
%%% COUNTER EXPORTS
%%%-----------------------------------------------------------------------------
count_in(Pid) ->
    gen_server:call(Pid, count_in, infinity).


count_out(Pid) ->
    gen_server:call(Pid, count_out, infinity).


count_reset(Pid) ->
    gen_server:cast(Pid, count_reset).

%%%-----------------------------------------------------------------------------
%%% RPS EXPORTS
%%%-----------------------------------------------------------------------------
rps_avg(Pid) ->
    gen_server:call(Pid, rps_avg, infinity).


rps(Pid) ->
    gen_server:call(Pid, rps, infinity).

%%%-----------------------------------------------------------------------------
%%% INIT/TERMINATE EXPORTS
%%%-----------------------------------------------------------------------------
init([Parent, File]) ->
    % Since the gen_server handles exits from the parent process when
    % started with start_link, we need to do this to ensure that we will
    % receive the exit signal in handle_info.
    link(Parent),
    process_flag(trap_exit, true),
    case queue_open(File) of
        {ok, Queue} ->
            {ok, #st{parent = Parent, queue = Queue}};
        {error, Reason} ->
            {stop, Reason}
    end.


terminate(_Reason, St) ->
    ok = queue_close(St#st.queue).

%%%-----------------------------------------------------------------------------
%%% HANDLE MESSAGES EXPORTS
%%%-----------------------------------------------------------------------------
handle_call({in, Item, Priority}, _From, St) ->
    Queue = queue_in(Item, St#st.queue, Priority),
    {reply, ok, St#st{queue = Queue, in = St#st.in + 1}};
handle_call(len, _From, St) ->
    {reply, queue_len(St#st.queue), St};
handle_call({out, Num}, _From, St) ->
    {Result, Queue} = queue_out_num(St#st.queue, Num),
    Len = length(Result),
    {reply, Result, St#st{queue = Queue, out = St#st.out + Len}};
handle_call({out_r, Num}, _From, St) ->
    {Result, Queue} = queue_out_r_num(St#st.queue, Num),
    Len = length(Result),
    {reply, Result, St#st{queue = Queue, out = St#st.out + Len}};
handle_call(count_in, _From, St) ->
    {reply, St#st.in, St};
handle_call(count_out, _From, St) ->
    {reply, St#st.out, St};
handle_call(rps_avg, _From, St) ->
    Secs = timer:now_diff(now(), St#st.start) / 1000000,
    {reply, round(St#st.out / Secs), St};
handle_call(rps, From, St) ->
    _Ref = erlang:start_timer(1000, self(), {rps, From, St#st.out}),
    {noreply, St};
handle_call(stop, _From, St) ->
    {stop, normal, ok, St}.


handle_cast(count_reset, St) ->
    {noreply, St#st{in = 0, out = 0, start = now()}}.


handle_info({timeout, _Ref, {rps, From, Out}}, St) ->
    gen_server:reply(From, St#st.out - Out),
    {noreply, St};
handle_info({'EXIT', Parent, Reason}, #st{parent = Parent} = St) ->
    % Close the queue and remove it from the state to prevent dumping the
    % entire queue structure to the error logger.
    ok = queue_close(St#st.queue),
    {stop, Reason, St#st{queue = undefined}};
handle_info(_Info, St) ->
    {noreply, St}.

%%%-----------------------------------------------------------------------------
%%% CODE UPDATE EXPORTS
%%%-----------------------------------------------------------------------------
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
queue_close({cl_dqueue, Q}) ->
    ok = cl_dqueue:close(Q);
queue_close(_Other) ->
    ok.


queue_open(undefined) ->
    {ok, {cl_queue, cl_queue:new()}};
queue_open(File) ->
    case cl_dqueue:open(File) of
        {ok, Q} ->
            {ok, {cl_dqueue, Q}};
        Error ->
            Error
    end.


queue_in(Item, {Mod, Q}, Priority) ->
    {Mod, Mod:in(Item, Q, Priority)}.


queue_len({Mod, Q}) ->
    Mod:len(Q).


queue_out_num(Queue, Num) ->
    queue_out_num(Queue, out, Num, []).

queue_out_r_num(Queue, Num) ->
    queue_out_num(Queue, out_r, Num, []).

queue_out_num(Queue, _Out, 0, Acc) ->
    {lists:reverse(Acc), Queue};
queue_out_num({Mod, Q}, Out, Num, Acc) ->
    case Mod:Out(Q) of
        {empty, Q} ->
            {lists:reverse(Acc), {Mod, Q}};
        {{value, Item}, NewQ} ->
            queue_out_num({Mod, NewQ}, Out, Num - 1, [Item | Acc])
    end.
