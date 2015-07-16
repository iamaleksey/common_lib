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
-module(cl_consumer_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(HIGH, 0).
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [out, in_out].


sequences() ->
    [].


suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
    {A1, A2, A3} = erlang:timestamp(),
    random:seed(A1, A2, A3),
    dbg:tracer(),
    dbg:p(all, [c, sos, sol]),
    MaxTime = ct:get_config(max_time, ?MAX_TIME),
    [{max_time, MaxTime} | Conf].

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(_Conf) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    init_stubs(Case),
    init_traces(Case),
    Conf.


init_stubs(Case) ->
    NegCases = ct:get_config(neg_cases, []),
    Stubs = proplists:get_value(Case, NegCases, []),
    lists:foreach(fun(Stub) -> load_stub(Stub, true) end, Stubs).


init_traces(Case) ->
    TpCases = ct:get_config(tp_cases, []),
    Tps = proplists:get_value(Case, TpCases, []),
    lists:foreach(fun(Tp) -> add_trace(tp, Tp) end, Tps),
    TplCases = ct:get_config(tpl_cases, []),
    Tpls = proplists:get_value(Case, TplCases, []),
    lists:foreach(fun(Tpl) -> add_trace(tp, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    end_traces(Case),
    end_stubs(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.


end_stubs(Case) ->
    NegCases = ct:get_config(neg_cases, []),
    Stubs = proplists:get_value(Case, NegCases, []),
    lists:foreach(fun purge_stub/1, Stubs).


end_traces(Case) ->
    TpCases = ct:get_config(tp_cases, []),
    Tps = proplists:get_value(Case, TpCases, []),
    lists:foreach(fun(Tp) -> del_trace(ctp, Tp) end, Tps),
    TplCases = ct:get_config(tpl_cases, []),
    Tpls = proplists:get_value(Case, TplCases, []),
    lists:foreach(fun(Tpl) -> del_trace(ctpl, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% TEST CASE
%%%-----------------------------------------------------------------------------
out() ->
    [{userdata, [{doc, "Tests the consumer."}]}].

out(_Conf) ->
    {ok, QueueSrv} = cl_queue_srv:start_link(),
    L = lists:duplicate(100000, element),
    ok = lists:foreach(fun(X) -> cl_queue_srv:in(QueueSrv, X, ?HIGH) end, L),
    100000 = cl_queue_srv:len(QueueSrv),
    Consume = fun(_) -> ok end,
    {ok, Consumer} = cl_consumer:start_link(QueueSrv, Consume, 0),
    timer:sleep(1000),
    100000 = cl_queue_srv:len(QueueSrv),
    ok = cl_consumer:rps(Consumer, 1000),
    ok = cl_consumer:resume(Consumer),
    ok = cl_consumer:resume(Consumer),
    timer:sleep(50000),
    send_junk(Consumer),
    ok = cl_consumer:pause(Consumer),
    ok = cl_consumer:pause(Consumer),
    Len = cl_queue_srv:len(QueueSrv),
    % Allow 10% slower but never faster.
    if
        Len < 50000 ->
            exit({error, {too_fast, Len}});
        Len == 50000 ->
            ct:print("Perfect!");
        Len =< 50500 ->
            ct:print("Good, less than 1% deviation: ~p", [Len]);
        Len =< 55000 ->
            Deviation = ((Len - 50000) * 100) / 50000,
            ct:print("Fair, ~.2f% deviation: ~p", [Deviation, Len]);
        true ->
            exit({error, {too_slow, Len}})
    end,
    timer:sleep(1000),
    Len = cl_queue_srv:len(QueueSrv),
    ok = cl_consumer:rps(Consumer, 250),
    timer:sleep(1000),
    Len = cl_queue_srv:len(QueueSrv),
    ok = cl_consumer:resume(Consumer),
    timer:sleep(40000),
    250 = cl_consumer:rps(Consumer),
    ok = cl_consumer:pause(Consumer),
    250 = cl_consumer:rps(Consumer),
    ExpectedLen = Len - 10000,
    RealLen = cl_queue_srv:len(QueueSrv),
    if
        RealLen < ExpectedLen ->
            exit({error, {too_fast, RealLen}});
        RealLen == ExpectedLen ->
            ct:print("Perfect!");
        RealLen =< (ExpectedLen + 100) ->
            ct:print("Good, less than 1% deviation: ~p", [RealLen]);
        RealLen =< (ExpectedLen + 1000) ->
            Deviation2 = 100 - (((Len - RealLen) * 100) / 10000),
            ct:print("Fair, ~.2f% deviation: ~p", [Deviation2, RealLen]);
        true ->
            exit({error, {too_slow, RealLen}})
    end,
    ok = cl_consumer:resume(Consumer),
    timer:sleep(10000),
    ok = cl_consumer:rps(Consumer, 500),
    timer:sleep(20000),
    ok = cl_consumer:stop(Consumer),
    ExpectedLen2 = RealLen - 12500,
    Rest = cl_queue_srv:len(QueueSrv),
    if
        Rest < ExpectedLen2 ->
            exit({error, {too_fast, Rest}});
        Rest == ExpectedLen2 ->
            ct:print("Perfect!");
        Rest =< (ExpectedLen2 + 130) ->
            ct:print("Good, less than 1% deviation: ~p", [Rest]);
        Rest =< (ExpectedLen2 + 1250) ->
            Deviation3 = 100 - (((RealLen - Rest) * 100) / 12500),
            ct:print("Fair, ~.2f% deviation: ~p", [Deviation3, Rest]);
        true ->
            exit({error, {too_slow, Rest}})
    end,
    timer:sleep(1000),
    Rest = cl_queue_srv:len(QueueSrv),
    cl_queue_srv:stop(QueueSrv),
    ok.


in_out() ->
    [{userdata, [{doc, "Tests the consumer with a generator."}]}].

in_out(_Conf) ->
    {ok, QueueSrv} = cl_queue_srv:start_link(),
    L = lists:duplicate(100000, element),
    % Insert 1000 per second
    Insert = fun(X) -> cl_queue_srv:in(QueueSrv, X, ?HIGH), timer:sleep(1)  end,
    Pid = spawn(fun() -> lists:foreach(Insert, L) end),
    Consume = fun(_) -> ok end,
    % Consume 1000 per second
    {ok, _Consumer} = cl_consumer:start_link(QueueSrv, Consume, 1000),
    InitLen = cl_queue_srv:len(QueueSrv),
    timer:sleep(50000),
    Len = cl_queue_srv:len(QueueSrv),
    % Allow 10% slower but never faster
    if
        Len < InitLen ->
            exit({error, {too_fast, Len}});
        Len == InitLen ->
            ct:print("Perfect!");
        Len =< (InitLen + 500) ->
            ct:print("Good, 1% deviation: ~p", [Len]);
        Len =< (InitLen + 5000) ->
            ok;
        true ->
            exit({error, {too_slow, Len}})
    end,
    timer:sleep(50000),
    NewLen = cl_queue_srv:len(QueueSrv),
    exit(Pid, kill),
    if
        NewLen < InitLen ->
            exit({error, {too_fast, NewLen}});
        NewLen == InitLen ->
            ct:print("Perfect!");
        NewLen =< (InitLen + 1000) ->
            ct:print("Good, 1% deviation: ~p", [NewLen]);
        NewLen =< (InitLen + 10000) ->
            ok;
        true ->
            exit({error, {too_slow, NewLen}})
    end,
    cl_queue_srv:stop(QueueSrv),
    ok.

%%%-----------------------------------------------------------------------------
%%% TRACING UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
add_trace(TpFun, {Mod, Fun, Spec}) ->
    dbg:TpFun(Mod, Fun, Spec);
add_trace(TpFun, {Mod, Fun}) ->
    dbg:TpFun(Mod, Fun, ?MATCH_SPEC);
add_trace(TpFun, Mod) ->
    dbg:TpFun(Mod, ?MATCH_SPEC).


del_trace(CtpFun, {Mod, Fun, _Spec}) ->
    dbg:CtpFun(Mod, Fun);
del_trace(CtpFun, {Mod, Fun}) ->
    dbg:CtpFun(Mod, Fun);
del_trace(CtpFun, Mod) ->
    dbg:CtpFun(Mod).

%%%-----------------------------------------------------------------------------
%%% STUB UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
load_stub(Stub, NegTest) ->
    Opts = if NegTest -> [binary, {d, neg_case}]; true ->  [binary] end,
    Erl = atom_to_list(Stub) ++ ".erl",
    ct:print("Compiling ~s with options ~p", [Erl, Opts]),
    {ok, Mod, Bin} = compile:file(filename:join(?STUBS_DIR, Erl), Opts),
    ct:print("Purge default ~p stub", [Mod]),
    code:purge(Mod),
    code:delete(Mod),
    ct:print("Loading new ~p stub", [Mod]),
    Beam = atom_to_list(Mod) ++ code:objfile_extension(),
    {module, Mod} = code:load_binary(Mod, Beam, Bin).


purge_stub(Stub) ->
    ct:print("Purge ~p stub", [Stub]),
    code:purge(Stub),
    code:delete(Stub),
    ct:print("Reloading default ~p stub", [Stub]),
    {module, Stub} = code:load_file(Stub).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
send_junk(Pid) ->
    {links, L} = process_info(Pid, links),
    lists:foreach(fun(X) -> X ! junk end, [Pid | L]).
