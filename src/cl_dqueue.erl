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
-module(cl_dqueue).

%%% OPEN/CLOSE EXPORTS
-export([close/1, open/1]).

%%% ORIGINAL EXPORTS
-export([in/2, in/3, is_dqueue/1, is_empty/1, len/1, out/1, out_r/1]).

%%% EXTENDED EXPORTS
-export([get/1, get_r/1]).

%%% MACROS
-define(HIGH_PRIORITY, 0).

%%%-----------------------------------------------------------------------------
%%% OPEN/CLOSE EXPORTS
%%%-----------------------------------------------------------------------------
close({_Queues, _Len, Dets}) ->
    dets:close(Dets).


open(File) ->
    Name = list_to_atom(filename:basename(filename:rootname(File))),
    case dets:open_file(Name, [{file, File}, {repair, force}]) of
        {ok, Name} ->
            Tmp = ets:new(Name, [ordered_set]),
            F = fun({Key, Priority, _Item}) ->
                        ets:insert(Tmp, {Key, Priority}),
                        continue;
                   (_Other) ->
                        not_dqueue
                end,
            case dets:traverse(Name, F) of
                not_dqueue ->
                    true = ets:delete(Tmp),
                    ok = dets:delete_all_objects(Name),
                    ok = dets:sync(Name),
                    {ok, {init_queues([]), 0, Name}};
                [] ->
                    Items = ets:tab2list(Tmp),
                    true = ets:delete(Tmp),
                    {ok, {init_queues(Items), length(Items), Name}}
            end;
        Error ->
            Error
    end.

%%%-----------------------------------------------------------------------------
%%% ORIGINAL EXPORTS
%%%-----------------------------------------------------------------------------
in(Item, Q) ->
    in(Item, Q, ?HIGH_PRIORITY).

in(Item, {Queues, Len, Dets}, Priority) ->
    Key = item_insert(Dets, Item, Priority),
    {in_queues(Key, Priority, Queues), Len + 1, Dets}.


is_dqueue({Queues, Len, Dets}) when is_integer(Len), is_atom(Dets) ->
    F = fun({_Priority, Queue}) ->
                queue:is_queue(Queue);
           (_Other) ->
                false
        end,
    lists:all(F, Queues);
is_dqueue(_Other) ->
    false.


is_empty({_Queues, 0, _Dets}) ->
    true;
is_empty(_Other) ->
    false.


len({_Queues, Len, _Dets}) ->
    Len.


out({_Queues, 0, _Dets} = Q) ->
    {empty, Q};
out({Queues1, Len, Dets}) ->
    {{value, Key}, Queues2} = out_queues(Queues1),
    Item = item_extract(Dets, Key),
    {{value, Item}, {Queues2, Len - 1, Dets}}.


out_r({_Queues, 0, _Dets} = Q) ->
    {empty, Q};
out_r({Queues1, Len, Dets}) ->
    {{value, Key}, Queues2} = out_r_queues(Queues1),
    Item = item_extract(Dets, Key),
    {{value, Item}, {Queues2, Len - 1, Dets}}.

%%%-----------------------------------------------------------------------------
%%% EXTENDED EXPORTS
%%%-----------------------------------------------------------------------------
get({_Queues, 0, _Dets}) ->
    erlang:error(empty);
get({Queues, _Len, Dets}) ->
    Key = get_queues(Queues),
    item_lookup(Dets, Key).


get_r({_Queues, 0, _Dets}) ->
    erlang:error(empty);
get_r({Queues, _Len, Dets}) ->
    Key = get_r_queues(Queues),
    item_lookup(Dets, Key).

%%%-----------------------------------------------------------------------------
%%% QUEUES FUNCTIONS
%%%-----------------------------------------------------------------------------
get_queues(Queues) ->
    get_queues(Queues, get).

get_r_queues(Queues) ->
    get_queues(lists:reverse(Queues), get_r).

get_queues([{_Priority, Q} | _], Get) ->
    queue:Get(Q).


in_queues(Key, Priority, Queues) ->
    in_queues(Key, Priority, [], Queues).

in_queues(Key, Priority, Acc, [{Priority, Q1}|T]) ->
    Q2 = queue:in(Key, Q1),
    lists:reverse(Acc) ++ [{Priority, Q2}|T];
in_queues(Key, Priority, Acc, [{Higher, _} = H|T]) when Priority > Higher ->
    in_queues(Key, Priority, [H|Acc], T);
in_queues(Key, Priority, Acc, L) ->
    Q1 = queue:new(),
    Q2 = queue:in(Key, Q1),
    lists:reverse(Acc) ++ [{Priority, Q2}|L].


init_queues(Items) ->
    init_queues(Items, []).

init_queues([], Queues) ->
    Queues;
init_queues([{Key, Priority} | T], Queues) ->
    init_queues(T, in_queues(Key, Priority, Queues)).

out_queues(Queues) ->
    out_queues(Queues, out).

out_r_queues(Queues1) ->
    {Value, Queues2} = out_queues(lists:reverse(Queues1), out_r),
    {Value, lists:reverse(Queues2)}.

out_queues([{Priority, Q1}|T], Out) ->
    {{value, Key}, Q2} = queue:Out(Q1),
    case queue:is_empty(Q2) of
        true ->
            {{value, Key}, T};
        false ->
            {{value, Key}, [{Priority, Q2}|T]}
    end.

%%%-----------------------------------------------------------------------------
%%% ITEM FUNCTIONS
%%%-----------------------------------------------------------------------------
item_extract(Dets, Key) ->
    [{Key, _Priority, Item}] = dets:lookup(Dets, Key),
    ok = dets:delete(Dets, Key),
    Item.


item_insert(Dets, Item, Priority) ->
    Key = erlang:unique_integer([positive, monotonic]),
    ok = dets:insert(Dets, {Key, Priority, Item}),
    Key.


item_lookup(Dets, Key) ->
    [{Key, _Priority, Item}] = dets:lookup(Dets, Key),
    Item.
