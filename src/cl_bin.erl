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
-module(cl_bin).

%%% EXTERNAL EXPORTS
-export([bin_or/1,
         bits_on/1,
         bits_st/1,
         from_bool/1,
         from_hexlist/1,
         incr_nth/2,
         incr_nth/3,
         invert/1,
         negate/1,
         sum/1,
         take_until/3,
         to_bool/1,
         to_hexlist/1,
         to_list/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
bin_or(Bin) ->
    List = binary_to_list(Bin),
    bin_or(List, 0).

bin_or([], I) ->
    I;
bin_or([A | B], I)->
    bin_or(B, I bxor A).



bits_on(Data) ->
    bits_on(Data, (size(Data) * 8) - 1, 0, []).

bits_on(_Data, -1, _Suffix, Acc) ->
    Acc;
bits_on(Data, Prefix, Suffix, Acc) ->
    case Data of
        <<_:Prefix, 1:1, _:Suffix>> ->
            bits_on(Data, Prefix-1, Suffix+1, [Prefix+1|Acc]);
        <<_:Prefix, 0:1, _:Suffix>> ->
            bits_on(Data, Prefix-1, Suffix+1, Acc)
    end.


bits_st(Data) ->
    bits_st(Data, (size(Data) * 8) - 1, 0, []).

bits_st(_Data, -1, _Suffix, Acc) ->
    Acc;
bits_st(Data, Prefix, Suffix, Acc) ->
    case Data of
        <<_:Prefix, 1:1, _:Suffix>> ->
            bits_st(Data, Prefix-1, Suffix+1, [true|Acc]);
        <<_:Prefix, 0:1, _:Suffix>> ->
            bits_st(Data, Prefix-1, Suffix+1, [false|Acc])
    end.


from_bool(false) ->
    0;
from_bool(true) ->
    1.


from_hexlist(HexList) ->
    list_to_binary(cl_lists:hexlist_to_intlist(HexList)).


incr_nth(Bin, Pos) ->
    incr_nth(Bin, Pos, 8).

incr_nth(Bin, Pos, Unit) ->
    Len = (Pos - 1) * Unit,
    <<Prefix:Len/binary-unit:1, V:Unit, Rest/binary>> = Bin,
    Val = V + 1,
    <<Prefix:Len/binary-unit:1, Val:Unit, Rest/binary>>.


invert(Bin) ->
    invert(Bin, <<>>).

invert(<<>>, Acc) ->
    Acc;
invert(<<A, B/binary>>, Acc) ->
   invert(B, <<A, Acc/binary>>).


negate(Binary) ->
    negate(Binary, <<>>).

negate(<<Byte, Binary/binary>>, Acc) ->
    negate(Binary, <<Acc/binary, (255-Byte)>>);
negate(<<>>, Acc) ->
    Acc.


sum(Bin) ->
    sum(Bin,0).


take_until(Binary, Pattern, Size) ->
    case take_until(Binary, Pattern, Size, []) of
        not_found ->
            MinSize = lists:min([Size, size(Binary)]),
            <<UntilSize:MinSize/binary-unit:8, _Rest/binary>> = Binary,
            {error, {not_found, Pattern, UntilSize}};
        {ok, UntilPattern, Rest} ->
            {ok, UntilPattern, Rest}
    end.

take_until(<<>>, _Pattern, _Size, _Acc) ->
    not_found;
take_until(_Binary, Pattern, Size, _Acc) when size(Pattern) > Size ->
    not_found;
take_until(Binary, Pattern, Size, Acc) ->
    Len = size(Pattern),
    case Binary of
        <<Pattern:Len/binary-unit:8, _Rest/binary>> ->
            {ok, list_to_binary(lists:reverse(Acc)), Binary};
        <<Octet:8, Rest/binary>> ->
            take_until(Rest, Pattern, Size - 1, [Octet|Acc])
    end.


to_bool(0) ->
    false;
to_bool(1) ->
    true.


to_hexlist(Bin) ->
    cl_lists:intlist_to_hexlist(binary_to_list(Bin)).


to_list(Bin, Unit) ->
    <<H:Unit, T/binary>> = Bin,
    to_list(T, Unit, [H]).

to_list(<<>>, _Unit, Acc) ->
    lists:reverse(Acc);
to_list(Bin, Unit, Acc) ->
    <<H:Unit, T/binary>> = Bin,
    to_list(T, Unit, [H|Acc]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
sum(<<>>, Acc) ->
    Acc;
sum(<<A:8/integer, B/binary>>, Acc)->
    sum(<<B/binary>>, Acc + A).

