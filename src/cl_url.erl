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
-module(cl_url).

%%% EXTERNAL EXPORTS
-export([decode/1, encode/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
decode([$%, X, Y | T]) ->
    [erlang:list_to_integer([X, Y], 16) | decode(T)];
decode([$+ | T]) ->
    [32 | decode(T)];
decode([H | T]) ->
    [H | decode(T)];
decode([]) ->
    [].


encode([H | T]) when H >= $a, H =< $z ->
    [H | encode(T)];
encode([H | T]) when H >= $A, H =< $Z ->
    [H | encode(T)];
encode([H | T]) when H >= $0, H =< $9 ->
    [H | encode(T)];
encode([H | T]) when H == $_; H == $.; H == $- ->
    [H | encode(T)];
encode([32 | T]) ->
    [$+ | encode(T)];
encode([H | T]) ->
    case erlang:integer_to_list(H, 16) of
        [X, Y] ->
            [$%, X, Y | encode(T)];
        [X] ->
            [$%, $0, X | encode(T)]
    end;
encode([]) ->
    [].
