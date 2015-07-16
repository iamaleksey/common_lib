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
-module(cl_queue_srv_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [exports, performance_disk, performance_mem].


sequences() ->
    [].


suite() ->
    [{timetrap, {minutes, 60}},
     {require, paths},
     {require, file},
     {require, min_operations},
     {require, max_operations},
     {require, max_size_diff}].


%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
    {A1, A2, A3} = erlang:timestamp(),
    random:seed(A1, A2, A3),
    dbg:tracer(),
    dbg:p(all, [c, sos, sol]),
    File = ct:get_config(file),
    os:cmd("rm -f " ++ File),
    MinOperations = ct:get_config(min_operations),
    MaxOperations = ct:get_config(max_operations),
    MaxSizeDiff = ct:get_config(max_size_diff),
    MaxTime = ct:get_config(max_time, ?MAX_TIME),
    [{max_time, MaxTime},
     {file, File},
     {min_operations, MinOperations},
     {max_operations, MaxOperations},
     {max_size_diff, MaxSizeDiff} | Conf].

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    os:cmd("rm -f " ++ ?config(file, Conf)).

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
%%% TEST CASES
%%%-----------------------------------------------------------------------------
exports() ->
    [{userdata, [{doc, "Tests for the exported functions in queue_srv."}]}].

exports(Conf) ->
    {ok, Pid1} = cl_queue_srv:start_link(?config(file, Conf)),
    [] = cl_queue_srv:out(Pid1),
    ok = cl_queue_srv:in(Pid1, 1, 1001),
    ok = cl_queue_srv:in(Pid1, 2, 3),
    [2] = cl_queue_srv:out(Pid1),
    [1] = cl_queue_srv:out(Pid1),
    [] = cl_queue_srv:out(Pid1),
    ok = cl_queue_srv:stop(Pid1),
    {ok, Pid2} = cl_queue_srv:start_link(),
    [] = cl_queue_srv:out(Pid2),
    ok = cl_queue_srv:in(Pid2, 1, 1001),
    ok = cl_queue_srv:in(Pid2, 2, 3),
    [2, 1] = cl_queue_srv:out(Pid2, 2),
    [] = cl_queue_srv:out(Pid2),
    ok = cl_queue_srv:stop(Pid2),
    {ok, Pid3} = cl_queue_srv:start_link(),
    [] = cl_queue_srv:out(Pid3),
    ok = cl_queue_srv:in(Pid3, 1, 1001),
    ok = cl_queue_srv:in(Pid3, 2, 3),
    [1, 2] = cl_queue_srv:out_r(Pid3, 2),
    [] = cl_queue_srv:out(Pid3),
    ok = cl_queue_srv:in(Pid3, 1, 100),
    ok = cl_queue_srv:in(Pid3, 2, 3),
    [1] = cl_queue_srv:out_r(Pid3),
    ok = cl_queue_srv:in(Pid3, 1, 100),
    [1, 2] = cl_queue_srv:out_r(Pid3, 3),
    ok = cl_queue_srv:stop(Pid3),
    ok.


performance_disk() ->
    [{userdata,
      [{doc, "Tests that cl_queue_srv conforms with performace restrictions."}]}].

performance_disk(Conf) ->
    {ok, Pid} = cl_queue_srv:start_link(?config(file, Conf)),
    MinOperations = ?config(min_operations, Conf),
    {T, _} = timer:tc(?MODULE, in_out, [Pid, MinOperations]),
    Time = trunc(T/1000),
    ct:print("~p operations in ~p milliseconds", [MinOperations, Time]),
    if Time > 1000 -> exit(too_slow); true -> ok end,
    ok = cl_queue_srv:stop(Pid).


performance_mem() ->
    [{userdata,
      [{doc, "Tests that cl_queue_srv conforms with performace restrictions."}]}].

performance_mem(Conf) ->
    {ok, Pid} = cl_queue_srv:start_link(),
    MinOperations = ?config(min_operations, Conf),
    {T, _} = timer:tc(?MODULE, in_out, [Pid, MinOperations]),
    Time = trunc(T/1000),
    ct:print("~p operations in ~p milliseconds", [MinOperations, Time]),
    if Time > 1000 -> exit(too_slow); true -> ok end,
    ok = cl_queue_srv:stop(Pid).

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
in_out(Pid, 0) ->
    Pid;
in_out(Pid, Times) ->
    case random:uniform(2) of
        1 ->
            ok = cl_queue_srv:in(Pid, erlang:timestamp(), random:uniform(10) - 1),
            in_out(Pid, Times - 1);
        2 ->
            _ = dqueue:out(Pid),
            in_out(Pid, Times - 1)
    end.
