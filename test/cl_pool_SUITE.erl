%%% Copyright (C) 2009 Nomasystems, S.L.
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%%% AB. All Rights Reserved.
-module('cl_pool_SUITE').

%%% INCLUDE FILES
-include("ct.hrl").

%%% EXTERNAL EXPORTS
-compile(export_all).

%%% MACROS
-define(MATCH_SPEC, [{'_', [], [{message, {return_trace}}]}]).
-define(MAX_TIME, 10000).
-define(MAX_TIMETRAP, 429496729).   % Don't ask me why.
-define(STUBS_DIR, "../../stubs").  % Tests run in log/ct_run.*

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [api].


sequences() ->
    [].


suite() ->
    [{timetrap, {minutes, 60}}].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    lists:foreach(fun(X) -> code:add_path(X) end, ct:get_config(paths, [])),
    {A1, A2, A3} = now(),
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

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    end_traces(Case),
    end_stubs(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
api() ->
    [{userdata, [{doc, "Tests the public API."}]}].

api(_Conf) ->
    ok = cl_pool:new(test),
    ok = cl_pool:add(test, 1),
    1 = cl_pool:next(test),
    1 = cl_pool:next(test),
    [1] = cl_pool:items(test),
    ok = cl_pool:add(test, 2),
    2 = cl_pool:next(test),
    1 = cl_pool:next(test),
    ok = cl_pool:add(test, 3),
    [3, 2, 1] = cl_pool:items(test),
    2 = cl_pool:next(test),
    1 = cl_pool:next(test),
    3 = cl_pool:next(test),
    2 = cl_pool:next(test),
    ok = cl_pool:del(test, 2),
    3 = cl_pool:next(test),
    1 = cl_pool:next(test),
    ok = cl_pool:del(test, 1),
    3 = cl_pool:next(test),
    3 = cl_pool:next(test),
    ok = cl_pool:del(test, 1),
    ok = cl_pool:del(test, 3),
    [] = cl_pool:items(test),
    try
        _Error = cl_pool:next(test)
    catch
        not_found ->
            ok
    end,
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


end_traces(Case) ->
    TpCases = ct:get_config(tp_cases, []),
    Tps = proplists:get_value(Case, TpCases, []),
    lists:foreach(fun(Tp) -> del_trace(ctp, Tp) end, Tps),
    TplCases = ct:get_config(tpl_cases, []),
    Tpls = proplists:get_value(Case, TplCases, []),
    lists:foreach(fun(Tpl) -> del_trace(ctpl, Tpl) end, Tpls).


init_traces(Case) ->
    TpCases = ct:get_config(tp_cases, []),
    Tps = proplists:get_value(Case, TpCases, []),
    lists:foreach(fun(Tp) -> add_trace(tp, Tp) end, Tps),
    TplCases = ct:get_config(tpl_cases, []),
    Tpls = proplists:get_value(Case, TplCases, []),
    lists:foreach(fun(Tpl) -> add_trace(tp, Tpl) end, Tpls).

%%%-----------------------------------------------------------------------------
%%% STUB UTIL FUNCTIONS
%%%-----------------------------------------------------------------------------
end_stubs(Case) ->
    NegCases = ct:get_config(neg_cases, []),
    Stubs = proplists:get_value(Case, NegCases, []),
    lists:foreach(fun purge_stub/1, Stubs).


init_stubs(Case) ->
    NegCases = ct:get_config(neg_cases, []),
    Stubs = proplists:get_value(Case, NegCases, []),
    lists:foreach(fun(Stub) -> load_stub(Stub, true) end, Stubs).


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
