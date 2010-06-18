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
-module(cl_dqueue_SUITE).

%%% INCLUDE FILES
-include("ct.hrl").
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
    {A1, A2, A3} = now(),
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
    [exports, integrity, performance, disk_usage].

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
exports() ->
    [{userdata, [{doc, "Tests for the exported functions in dqueue."}]}].

exports(Config) ->
    {ok, Q1} = cl_dqueue:open(?config(file, Config)),
    0 = cl_dqueue:len(Q1),
    true = cl_dqueue:is_empty(Q1),
    true = cl_dqueue:is_dqueue(Q1),
    Q2 = cl_dqueue:in(1, Q1, 1001),
    false = cl_dqueue:is_empty(Q2),
    Q3 = cl_dqueue:in(2, Q2, 3),
    Q4 = cl_dqueue:in(4, Q3),
    3 = cl_dqueue:len(Q4),
    Q5 = cl_dqueue:in(3, Q4, 0),
    ok = cl_dqueue:close(Q5),

    % Recover dqueue from file
    {ok, Q6} = cl_dqueue:open(?config(file, Config)),
    true = cl_dqueue:is_dqueue(Q6),
    false = cl_dqueue:is_dqueue({malformed, dqueue}),
    false = cl_dqueue:is_dqueue({[not_queue], 0, dqueue}),
    4 = cl_dqueue:len(Q6),
    false = cl_dqueue:is_empty(Q6),
    {Queues, Len, Dets} = Q6,
    false = cl_dqueue:is_dqueue({Queues ++ [not_queue], Len, Dets}),
    4 = cl_dqueue:get(Q6),
    1 = cl_dqueue:get_r(Q6),
    {{value, 4}, Q7} = cl_dqueue:out(Q6),
    {{value, 3}, Q8} = cl_dqueue:out(Q7),
    {{value, 2}, Q9} = cl_dqueue:out(Q8),
    {{value, 1}, Q10} = cl_dqueue:out(Q9),
    {empty, Q10} = cl_dqueue:out(Q10),
    empty = try cl_dqueue:get(Q10) catch _:empty -> empty end,
    empty = try cl_dqueue:get_r(Q10) catch _:empty -> empty end,
    true = cl_dqueue:is_empty(Q10),
    true = cl_dqueue:is_dqueue(Q10),
    Q11 = cl_dqueue:in(1, Q10, 1001),
    Q12 = cl_dqueue:in(2, Q11, 3),
    {{value, 2}, Q13} = cl_dqueue:out(Q12),
    Q14 = cl_dqueue:in(4, Q13),
    Q15 = cl_dqueue:in(3, Q14, 0),
    {{value, 4}, Q16} = cl_dqueue:out(Q15),
    {{value, 3}, Q17} = cl_dqueue:out(Q16),
    {{value, 1}, Q18} = cl_dqueue:out(Q17),
    {empty, Q18} = cl_dqueue:out(Q18),
    true = cl_dqueue:is_empty(Q18),
    Q19 = cl_dqueue:in(2, Q18, 3),
    false = cl_dqueue:is_empty(Q19),
    ok = cl_dqueue:close(Q19),

    % Corrupt the file
    ok = file:write_file(?config(file, Config), <<"corrupted data">>),

    % Reopen.  File must be erased and dqueue empty
    {ok, Q20} = cl_dqueue:open(?config(file, Config)),
    true = cl_dqueue:is_dqueue(Q20),
    true = cl_dqueue:is_empty(Q20),
    ok = cl_dqueue:close(Q20),

    % Change mode of file
    os:cmd("chmod u-rw " ++ ?config(file, Config)),
    {error, _Reason} = cl_dqueue:open(?config(file, Config)),
    os:cmd("chmod u+rw " ++ ?config(file, Config)),

    % Open the dets file and add trash to it
    {ok, bar} = dets:open_file(bar, [{file, ?config(file, Config)}]),
    dets:insert(bar, {1, foo}),
    dets:close(bar),

    % Reopen.  File must be erased and dqueue empty
    {ok, Q21} = cl_dqueue:open(?config(file, Config)),
    true = cl_dqueue:is_dqueue(Q21),
    true = cl_dqueue:is_empty(Q21),
    Q22 = cl_dqueue:in(2, Q21, 3),
    false = cl_dqueue:is_empty(Q22),
    Q23 = cl_dqueue:in(3, Q22, 0),
    Q24 = cl_dqueue:in(5, Q23, 10),
    Q25 = cl_dqueue:in(4, Q24, 0),
    {{value, 5}, Q26} = cl_dqueue:out_r(Q25),
    {{value, 2}, Q27} = cl_dqueue:out_r(Q26),
    {{value, 4}, Q28} = cl_dqueue:out_r(Q27),
    {{value, 3}, Q29} = cl_dqueue:out_r(Q28),
    {empty, Q29} = cl_dqueue:out_r(Q29),
    ok = cl_dqueue:close(Q29).


integrity() ->
    [{userdata, [{doc, "Tests that dqueue open works ok."}]}].

integrity(Config) ->
    {ok, Q1} = cl_dqueue:open(?config(file, Config)),
    Max = ?config(max_operations, Config),
    Q2 = in(Q1, Max),
    ok = cl_dqueue:close(Q2),
    {ok, Q2} = cl_dqueue:open(?config(file, Config)),
    ok = cl_dqueue:close(Q2).


performance() ->
    [{userdata, [{doc, "Tests that dqueue conforms with performace restrictions."}]}].

performance(Config) ->
    {ok, Q1} = cl_dqueue:open(?config(file, Config)),
    {T, Q2} = timer:tc(?MODULE, in_out, [Q1, ?config(min_operations, Config)]),
    ok = cl_dqueue:close(Q2),
    if
        trunc(T/1000) > 1000 ->
            exit(too_slow);
        true ->
            ok
    end.

disk_usage() ->
    [{userdata, [{doc, "Tests that dqueue conforms with disk usage restrictions."}]}].

disk_usage(Config) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Max = ?config(max_operations, Config),
    {ok, Q1} = cl_dqueue:open(?config(file, Config)),
    {Time2, Q2} = timer:tc(?MODULE, in, [Q1, Max]),
    {ok, I2} = file:read_file_info(?config(file, Config)),
    ct:print("~p items added in ~.2f seconds (disk file of ~p bytes)",
             [Max, Time2 / 1000000, I2#file_info.size]),
    {Time3, Q3} = timer:tc(?MODULE, out, [Q2, Max]),
    {ok, I3} = file:read_file_info(?config(file, Config)),
    ct:print("~p items removed in ~.2f seconds (disk file of ~p bytes)",
             [Max, Time3 / 1000000, I3#file_info.size]),
    {Time4, Q4} = timer:tc(?MODULE, in, [Q3, Max]),
    {ok, I4} = file:read_file_info(?config(file, Config)),
    ct:print("~p items added in ~.2f seconds (disk file of ~p bytes)",
             [Max, Time4 / 1000000, I4#file_info.size]),
    {Time5, Q5} = timer:tc(?MODULE, out, [Q4, Max]),
    {ok, I5} = file:read_file_info(?config(file, Config)),
    ct:print("~p items removed in ~.2f seconds (disk file of ~p bytes)",
             [Max, Time5 / 1000000, I5#file_info.size]),
    {Time6, Q6} = timer:tc(?MODULE, in_out, [Q5, Max]),
    {ok, I6} = file:read_file_info(?config(file, Config)),
    ct:print("~p in/out operations in ~.2f seconds (disk file of ~p bytes)",
             [Max, Time6 / 1000000, I6#file_info.size]),
    ok = cl_dqueue:close(Q6),
    MaxSizeDiff = ?config(max_size_diff, Config),
    if
        I6#file_info.size < trunc(I2#file_info.size * MaxSizeDiff) ->
            ok;
        true ->
            exit(too_big)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
in(Q, 0) ->
    Q;
in(Q, N) ->
    in(cl_dqueue:in(now(), Q, random:uniform(10) - 1), N - 1).


in_out(Q, 0) ->
    Q;
in_out(Q1, Times) ->
    case random:uniform(2) of
        1 ->
            Q2 = cl_dqueue:in(now(), Q1, random:uniform(10) - 1),
            in_out(Q2, Times - 1);
        2 ->
            {_, Q2} = cl_dqueue:out(Q1),
            in_out(Q2, Times - 1)
    end.


out(Q, 0) ->
    Q;
out(Q, N) ->
    out(element(2, cl_dqueue:out(Q)), N - 1).
