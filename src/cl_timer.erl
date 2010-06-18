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
-module(cl_timer).

%%% UTILITY EXPORTS
-export([tc/1, tc_avg/4, then/1, tstamp/0]).

%%% TIMER EXPORTS
-export([cancel/1, exit_after/2, exit_after/3]).

%%%-----------------------------------------------------------------------------
%%% UTILITY EXPORTS
%%%-----------------------------------------------------------------------------
tc(Fun) ->
    Before = erlang:now(),
    Val = (catch Fun()),
    After = erlang:now(),
    {timer:now_diff(After, Before), Val}.


tc_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Len = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Len / 2)), lists:sort(L)),
    Avg = lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) div Len,
    {Min, Max, Med, Avg}.


then(TimeLapse) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    TotalMicroSecs = MicroSecs + TimeLapse,
    SecsIncr = TotalMicroSecs div 1000000,
    MicroSecsRest = TotalMicroSecs - (SecsIncr * 1000000),
    TotalSecs = Secs + SecsIncr,
    MegaSecsIncr = TotalSecs div 1000000,
    SecsRest = TotalSecs - (MegaSecsIncr * 1000000),
    {MegaSecs + MegaSecsIncr, SecsRest, MicroSecsRest}.


tstamp() ->
    {A, B, C} = now(),
    (((A * 1000000) * 1000) + (B * 1000) + (C div 1000)).

%%%-----------------------------------------------------------------------------
%%% TIMER EXPORTS
%%%-----------------------------------------------------------------------------
cancel(Ref) ->
    cl_timer_srv:cancel(Ref).


exit_after(Time, Reason) ->
    exit_after(Time, self(), Reason).

exit_after(Time, Pid, Reason) ->
    cl_timer_srv:exit_after(Time, Pid, Reason).


%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).
