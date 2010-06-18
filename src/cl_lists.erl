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
-module(cl_lists).

%%% EXTERNAL EXPORTS
-export([asearch/2,
         count/2,
         cut/2,
         delete/3,
         first/2,
         from_number/1,
         has_duplicates/1,
         hexlist_to_intlist/1,
         index/2,
         intersec/2,
         intlist_to_hexlist/1,
         is_deep/1,
         keyindex/3,
         keyextract/3,
         random/2,
         random_seq/2,
         search/2,
         split/2,
         split2/1,
         splitwith/2,
         to_integer/1,
         to_number/1,
         ukeymerge/3]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
asearch(APred, List) ->
    ASearch = fun([], _, ambiguous) ->
                      false;
                 ([], _, Value) ->
                      Value;
                 ([H|T], F, Value) ->
                      case APred(H) of
                          true ->
                              {value, H};
                          approximate when Value == false ->
                              % First approximate match...go on searching
                              F(T, F, {value, H});
                          approximate ->
                              % Keep on searching for an exact match
                              F(T, F, ambiguous);
                          false ->
                              F(T, F, Value)
                      end
              end,
    ASearch(List, ASearch, false).


count(Elem, List) ->
    count(Elem, List, 0).

count(_, [], Count) ->
    Count;
count(H, [H|T], Count) ->
    count(H, T, Count+1);
count(H, [_|T], Count) ->
    count(H, T, Count).


cut(List, Index) when length(List) >= Index, Index >= 0 ->
    cut(List, [], Index).

cut(Suffix, RevPrefix, 0) ->
    {lists:reverse(RevPrefix), Suffix};
cut([H|T], RevPrefix, N) ->
    cut(T, [H|RevPrefix], N - 1).


delete(Element, N, TupleList) ->
    delete(Element, N, TupleList, []).

delete(_Element, _N, [], Acc) ->
    lists:reverse(Acc);
delete(Element, N, [H|T], Acc) when element(N, H) == Element ->
    delete(Element, N, T, Acc);
delete(Element, N, [H|T], Acc) ->
    delete(Element, N, T, [H|Acc]).


first(Pred, [H|T]) ->
    case Pred(H) of
        true  -> H;
        false -> first(Pred, T)
    end.


from_number(Number) ->
    case catch integer_to_list(Number) of
        {'EXIT', _Reason} ->
            float_to_list(Number);
        L ->
            L
    end.


has_duplicates([]) ->
    false;
has_duplicates([H|T]) ->
    case lists:member(H, T) of
        true ->
            true;
        _False ->
            has_duplicates(T)
    end.


hexlist_to_intlist(List) ->
    [erlang:list_to_integer(X, 16) || X <- split2(List)].


index(Elem, List) ->
    index(Elem, List, 1).

index(H, [H|_], Index) ->
    Index;
index(Elem, [_|T], Index) ->
    index(Elem, T, Index + 1).


intersec(List1, List2) ->
    intersec(List1, List2, []).

intersec([], _List2, Acc) ->
    lists:reverse(Acc);
intersec([H|T], List2, Acc) ->
    case lists:member(H, List2) of
        true ->
            intersec(T, List2, [H|Acc]);
        false ->
            intersec(T, List2, Acc)
    end.


intlist_to_hexlist(List) ->
    intlist_to_hexlist(List, []).

intlist_to_hexlist([], Acc) ->
    lists:reverse(Acc);
intlist_to_hexlist([0|T], Acc) ->
    intlist_to_hexlist(T, [$0,$0|Acc]);
intlist_to_hexlist([H|T], Acc) when H < 16 ->
    [A] = erlang:integer_to_list(H, 16),
    intlist_to_hexlist(T, [A,$0|Acc]);
intlist_to_hexlist([H|T], Acc) ->
    [A,B] = erlang:integer_to_list(H, 16),
    intlist_to_hexlist(T, [B,A|Acc]).


is_deep([H|_]) when is_list(H) ->
    true;
is_deep([_|T]) ->
    is_deep(T);
is_deep(_) ->
    false.


keyextract(Key, N, TupleList) ->
    keyextract(Key, N, TupleList, []).

keyextract(Key, N, [H|T], Acc) when element(N, H) == Key ->
    {H, lists:reverse(Acc) ++ T};
keyextract(Key, N, [H|T], Acc) ->
    keyextract(Key, N, T, [H|Acc]).


keyindex(Key, N, TupleList) ->
    keyindex(Key, N, TupleList, 1).

keyindex(Key, N, [H|_], Index) when element(N, H) == Key ->
    Index;
keyindex(Key, N, [_|T], Index) ->
    keyindex(Key, N, T, Index + 1).


random(Max, Length) ->
    random(Max, Length, []).

random(_Max, 0, List) ->
    List;
random(Max, Length, List) ->
    random(Max, Length - 1, [random:uniform(Max)|List]).


random_seq(Max, Length) when (Length * 2) > Max ->
    random_del(Max, Max - Length, lists:seq(1, Max));
random_seq(Max, Length) ->
    lists:sort(random_add(Max, Length, [])).


search(_Pred, []) ->
    false;
search(Pred, [H|T]) ->
    case Pred(H) of
        true ->
            {value, H};
        _False ->
            search(Pred, T)
    end.


split(List, N) when N >= 0, length(List) >= N ->
    split(List, N, []).

split(List, 0, Acc) ->
    {lists:reverse(Acc), List};
split([H|T], N, Acc) ->
    split(T, N-1, [H|Acc]).


split2(List) ->
    split2(List, []).

split2([], Acc) ->
    lists:reverse(Acc);
split2([H1,H2|T], Acc) ->
    split2(T, [[H1,H2]|Acc]);
split2([H|T], Acc) ->
    split2(T, [[H]|Acc]).


splitwith(Pred, L) ->
    splitwith(Pred, L, [], []).

splitwith(_Pred, [], L1, L2) ->
    {lists:reverse(L1), lists:reverse(L2)};
splitwith(Pred, [H|T], L1, L2) ->
    case Pred(H) of
        true  ->
            splitwith(Pred, T, [H|L1], L2);
        false ->
            splitwith(Pred, T, L1, [H|L2])
    end.


to_integer(OctetList) ->
    Size = length(OctetList) * 8,
    <<Value:Size/integer>> = list_to_binary(OctetList),
    Value.


to_number(List) ->
    case catch list_to_integer(List) of
        {'EXIT', _Reason} ->
            list_to_float(List);
        I ->
            I
    end.


ukeymerge(N, List1, List2) ->
    ukeymerge(N, List1, List2, []).

ukeymerge(_N, [], List2, MergedList) ->
    lists:reverse(MergedList) ++ List2;
ukeymerge(_N, List1, [], MergedList) ->
    lists:reverse(MergedList) ++ List1;
ukeymerge(N, [H1|T1], [H2|T2], MergedList)
  when element(N, H1) == element(N, H2) ->
    ukeymerge(N, T1, T2, [H1|MergedList]);
ukeymerge(N, [H1|T1], [H2|T2], MergedList)
  when element(N, H1) < element(N, H2) ->
    ukeymerge(N, T1, [H2|T2], [H1|MergedList]);
ukeymerge(N, [H1|T1], [H2|T2], MergedList) ->
    ukeymerge(N, [H1|T1], T2, [H2|MergedList]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
random_del(_Max, 0, List) ->
    List;
random_del(Max, Count, List1) ->
    List2 = lists:delete(random:uniform(Max), List1),
    random_del(Max, Count - (length(List1) - length(List2)), List2).


random_add(_Max, 0, List) ->
    List;
random_add(Max, Count, List) ->
    Element = random:uniform(Max),
    case lists:member(Element, List) of
        true ->
            random_add(Max, Count, List);
        _    ->
            random_add(Max, Count - 1, [Element|List])
    end.
