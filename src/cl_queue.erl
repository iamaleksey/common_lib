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
-module(cl_queue).

%%% ORIGINAL EXPORTS
-export([in/2,
         in/3,
         is_queue/1,
         is_empty/1,
         join/2,
         len/1,
         new/0,
         out/1,
         out_r/1]).

%%% EXTENDED EXPORTS
-export([get/1, get_r/1]).

%%% MACROS
-define(HIGH_PRIORITY, 0).

%%%-----------------------------------------------------------------------------
%%% ORIGINAL EXPORTS
%%%-----------------------------------------------------------------------------
in(Item, Q) ->
    in(Item, Q, ?HIGH_PRIORITY).


in(Item, {Queues, Len}, Priority) ->
    {in_queues(Item, Priority, Queues), Len + 1}.


is_queue({Queues, Len}) when is_integer(Len) ->
    F = fun({_Priority, Queue}) ->
                queue:is_queue(Queue);
           (_Other) ->
                false
        end,
    lists:all(F, Queues);
is_queue(_Other) ->
    false.


is_empty({_Queues, 0}) ->
    true;
is_empty(_Other) ->
    false.


join({Queues1, Len1}, {Queues2, Len2}) ->
    {join_queues(Queues1, Queues2), Len1 + Len2}.


len({_Queues, Len}) ->
    Len.


new() ->
    {[], 0}.


out({_Queues, 0} = Q) ->
    {empty, Q};
out({Queues1, Len}) ->
    {{value, Item}, Queues2} = out_queues(Queues1),
    {{value, Item}, {Queues2, Len - 1}}.

out_r({_Queues, 0} = Q) ->
    {empty, Q};
out_r({Queues1, Len}) ->
    {{value, Item}, Queues2} = out_r_queues(Queues1),
    {{value, Item}, {Queues2, Len - 1}}.

%%%-----------------------------------------------------------------------------
%%% EXTENDED EXPORTS
%%%-----------------------------------------------------------------------------
get({_Queues, 0}) ->
    erlang:error(empty);
get({Queues, _Len}) ->
    get_queues(Queues).

get_r({_Queues, 0}) ->
    erlang:error(empty);
get_r({Queues, _Len}) ->
    get_r_queues(Queues).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
get_queues(Queues) ->
    get_queues(Queues, get).

get_r_queues(Queues) ->
    get_queues(lists:reverse(Queues), get_r).

get_queues([{_Priority, Q} | _], Get) ->
    queue:Get(Q).


in_queues(Item, Priority, Queues) ->
    in_queues(Item, Priority, [], Queues).

in_queues(Item, Priority, Acc, [{Priority, Q1} | T]) ->
    Q2 = queue:in(Item, Q1),
    lists:reverse(Acc) ++ [{Priority, Q2} | T];
in_queues(Item, Priority, Acc, [{Higher, _} = H | T]) when Priority > Higher ->
    in_queues(Item, Priority, [H | Acc], T);
in_queues(Item, Priority, Acc, L) ->
    Q1 = queue:new(),
    Q2 = queue:in(Item, Q1),
    lists:reverse(Acc) ++ [{Priority, Q2} | L].


join_queues(Queues1, Queues2) ->
    join_queues(Queues1, Queues2, []).

join_queues([], Queues2, Acc) ->
    lists:reverse(Acc) ++ Queues2;
join_queues(Queues1, [], Acc) ->
    lists:reverse(Acc) ++ Queues1;
join_queues([{Priority, Q1} | T1], [{Priority, Q2} | T2], Acc) ->
    join_queues(T1, T2, [{Priority, queue:join(Q1, Q2)} | Acc]);
join_queues([{Priority1, _} = H | T1], [{Priority2, _} | _] = Queues2, Acc)
  when Priority1 < Priority2 ->
    join_queues(T1, Queues2, [H | Acc]);
join_queues(Queues1, [H | T], Acc) ->
    join_queues(Queues1, T, [H | Acc]).


out_queues(Queues) ->
    out_queues(Queues, out).

out_r_queues(Queues1) ->
    {Value, Queues2} = out_queues(lists:reverse(Queues1), out_r),
    {Value, lists:reverse(Queues2)}.

out_queues([{Priority, Q1} | T], Out) ->
    {{value, Item}, Q2} = queue:Out(Q1),
    case queue:is_empty(Q2) of
        true ->
            {{value, Item}, T};
        false ->
            {{value, Item}, [{Priority, Q2} | T]}
    end.
