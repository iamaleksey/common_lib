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
-module(cl_queue_SUITE).

%%% INCLUDE FILES
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
suite() ->
    [{timetrap, {minutes, 300}},
     {require, paths},
     {require, file},
     {require, min_operations},
     {require, max_operations},
     {require, max_size_diff}].

init_per_suite(Config) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths)),
    {A1, A2, A3} = erlang:timestamp(),
    random:seed(A1, A2, A3),
    start_dbg(),
    File = ct:get_config(file),
    os:cmd("rm -f " ++ File),
    MinOperations = ct:get_config(min_operations),
    MaxOperations = ct:get_config(max_operations),
    MaxSizeDiff = ct:get_config(max_size_diff),
    [{file, File},
     {min_operations, MinOperations},
     {max_operations, MaxOperations},
     {max_size_diff, MaxSizeDiff}|Config].

start_dbg() ->
    dbg:tracer(),
    dbg:p(all, [c, sos, sol]),
    case ct:get_config(tpl) of
        TplList when is_list(TplList) ->
            F = fun({Mod, Fun}) ->
                        dbg:tpl(Mod, Fun, ?MATCH_SPEC);
                   (Mod) ->
                        dbg:tpl(Mod, ?MATCH_SPEC)
                end,
            lists:foreach(F, TplList);
        _TplUndefined ->
            ok
    end,
    case ct:get_config(tp) of
        TpList when is_list(TpList) ->
            G = fun({Mod, Fun}) ->
                        dbg:tp(Mod, Fun, ?MATCH_SPEC);
                   (Mod) ->
                        dbg:tp(Mod, ?MATCH_SPEC)
                end,
            lists:foreach(G, TpList);
        _TpUndefined ->
            ok
    end.


end_per_suite(Config) ->
    os:cmd("rm -f " ++ ?config(file, Config)).


init_per_testcase(Case, Config) ->
    ct:print("Starting test case ~p", [Case]),
    Config.


sequences() ->
    [].


all() ->
    [exports, join, performance].

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
exports() ->
    [{userdata, [{doc, "Tests for the exported functions in cl_queue."}]}].

exports(_Config) ->
    Q1 = cl_queue:new(),
    0 = cl_queue:len(Q1),
    true = cl_queue:is_empty(Q1),
    true = cl_queue:is_queue(Q1),
    Q2 = cl_queue:in(1, Q1, 1001),
    false = cl_queue:is_empty(Q2),
    Q3 = cl_queue:in(2, Q2, 3),
    Q4 = cl_queue:in(4, Q3),
    3 = cl_queue:len(Q4),
    Q5 = cl_queue:in(3, Q4, 0),
    false = cl_queue:is_queue({malformed, queue}),
    false = cl_queue:is_queue({[not_queue], 0, queue}),
    4 = cl_queue:len(Q5),
    false = cl_queue:is_empty(Q5),
    4 = cl_queue:get(Q5),
    1 = cl_queue:get_r(Q5),
    true = cl_queue:is_queue(Q5),
    {Queues, Len} = Q5,
    false = cl_queue:is_queue({Queues ++ [not_queue], Len}),
    {{value, 4}, Q6} = cl_queue:out(Q5),
    {{value, 3}, Q7} = cl_queue:out(Q6),
    {{value, 2}, Q8} = cl_queue:out(Q7),
    {{value, 1}, Q9} = cl_queue:out(Q8),
    {empty, Q9} = cl_queue:out(Q9),
    empty = try cl_queue:get(Q9) catch _:empty -> empty end,
    empty = try cl_queue:get_r(Q9) catch _:empty -> empty end,
    true = cl_queue:is_empty(Q9),
    true = cl_queue:is_queue(Q9),
    Q10 = cl_queue:in(1, Q9, 1001),
    Q11 = cl_queue:in(2, Q10, 3),
    {{value, 2}, Q12} = cl_queue:out(Q11),
    Q13 = cl_queue:in(4, Q12),
    Q14 = cl_queue:in(3, Q13, 0),
    {{value, 4}, Q15} = cl_queue:out(Q14),
    {{value, 3}, Q16} = cl_queue:out(Q15),
    {{value, 1}, Q17} = cl_queue:out(Q16),
    {empty, Q17} = cl_queue:out(Q17),
    true = cl_queue:is_empty(Q17),
    Q18 = cl_queue:in(2, Q17, 3),
    false = cl_queue:is_empty(Q18),
    Q19 = cl_queue:in(3, Q18, 0),
    Q20 = cl_queue:in(5, Q19, 10),
    Q21 = cl_queue:in(4, Q20, 0),
    {{value, 5}, Q22} = cl_queue:out_r(Q21),
    {{value, 2}, Q23} = cl_queue:out_r(Q22),
    {{value, 4}, Q24} = cl_queue:out_r(Q23),
    {{value, 3}, Q25} = cl_queue:out_r(Q24),
    {empty, Q25} = cl_queue:out_r(Q25),
    ok.

join() ->
    [{userdata, [{doc, "Tests queue join."}]}].

join(_Config) ->
    Q1 = cl_queue:new(),
    Q2 = cl_queue:in({1, 1}, Q1, 1001),
    Q3 = cl_queue:in({1, 2}, Q2, 3),
    Q4 = cl_queue:in({1, 4}, Q3, 10),
    Q5 = cl_queue:in({1, 3}, Q4, 0),
    Q6 = cl_queue:in({1, 5}, Q5),
    5 = cl_queue:len(Q6),
    Q7 = cl_queue:new(),
    Q8 = cl_queue:in({2, 1}, Q7, 1002),
    Q9 = cl_queue:in({2, 2}, Q8, 3),
    Q10 = cl_queue:in({2, 4}, Q9, 9),
    Q11 = cl_queue:in({2, 3}, Q10, 0),
    Q12 = cl_queue:in({2, 5}, Q11, 0),
    Q13 = cl_queue:in({2, 6}, Q12, 1),
    6 = cl_queue:len(Q13),
    Q14 = cl_queue:join(Q6, Q13),
    11 = cl_queue:len(Q14),
    Q15 = cl_queue:join(Q14, cl_queue:new()),
    Q16 = cl_queue:new(),
    Q17 = cl_queue:in({3, 1}, Q16),
    {{value, {3, 1}}, Q18} = cl_queue:out(Q17),
    true = cl_queue:is_empty(Q18),
    Q19 = cl_queue:join(Q18, Q15),
    11 = cl_queue:len(Q19),
    {{value, {1, 3}}, Q20} = cl_queue:out(Q19),
    {{value, {1, 5}}, Q21} = cl_queue:out(Q20),
    {{value, {2, 3}}, Q22} = cl_queue:out(Q21),
    {{value, {2, 5}}, Q23} = cl_queue:out(Q22),
    {{value, {2, 6}}, Q24} = cl_queue:out(Q23),
    {{value, {1, 2}}, Q25} = cl_queue:out(Q24),
    {{value, {2, 2}}, Q26} = cl_queue:out(Q25),
    {{value, {2, 4}}, Q27} = cl_queue:out(Q26),
    {{value, {1, 4}}, Q28} = cl_queue:out(Q27),
    {{value, {1, 1}}, Q29} = cl_queue:out(Q28),
    {{value, {2, 1}}, Q30} = cl_queue:out(Q29),
    {empty, Q30} = cl_queue:out(Q30),
    true = cl_queue:is_empty(Q30),
    ok.


performance() ->
    [{userdata, [{doc, "Tests that queue conforms with performace restrictions."}]}].

performance(Config) ->
    Q1 = cl_queue:new(),
    {T, _Q2} = timer:tc(?MODULE, in_out, [Q1, ?config(min_operations, Config)]),
    if
        trunc(T/1000) > 1000 ->
            exit(too_slow);
        true ->
            ok
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
in(Q, 0) ->
    Q;
in(Q, N) ->
    in(cl_queue:in(erlang:timestamp(), Q, random:uniform(10) - 1), N - 1).


in_out(Q, 0) ->
    Q;
in_out(Q1, Times) ->
    case random:uniform(2) of
        1 ->
            Q2 = cl_queue:in(erlang:timestamp(), Q1, random:uniform(10) - 1),
            in_out(Q2, Times - 1);
        2 ->
            {_, Q2} = cl_queue:out(Q1),
            in_out(Q2, Times - 1)
    end.


out(Q, 0) ->
    Q;
out(Q, N) ->
    out(element(2, cl_queue:out(Q)), N - 1).
