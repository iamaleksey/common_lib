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
-module(cl_string).

%%% EXTERNAL EXPORTS
-export([aequal/2,
         aequal/6,
         aiequal/2,
         aiequal/6,
         chop_token/2,
         chop_tokens/3,
         insert_every/3,
         is_dec/1,
         is_hex/1,
         is_atime/1,
         is_rtime/1,
         normalize/1,
         pop_token/2,
         pop_tokens/3,
         replace_chars/3,
         split_by_word/2,
         strip/2,
         strip/3]).

%%% MACROS
-define(SPACE, 32).
-define(CR, 13).
-define(LF, 10).
-define(TAB, 9).

%% whitespace consists of 'space', 'carriage return', 'line feed' or 'tab'
-define(WHITESPACE(H), H == ?SPACE; H == ?CR; H == ?LF; H == ?TAB).
-define(WHITESPACES, [?SPACE, ?CR, ?LF, ?TAB]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
aequal(String, Pattern) ->
    case aequal(String, Pattern, 1, 0, 0, 0) of % Substitution
        false ->
            case aequal(String, Pattern, 0, 0, 1, 0) of % Deletion
                false ->
                    case aequal(String, Pattern, 0, 0, 0, 1) of %Transposition
                        false ->
                            aequal(String, Pattern, 0, 1, 0, 0); % Insertion
                        WithTransposition ->
                            WithTransposition
                    end;
                WithDeletion ->
                    WithDeletion
            end;
        WithSubstitution ->
            WithSubstitution
    end.

aequal(_String, _Pattern, S, I, D, T) when S < 0; I < 0; D < 0; T < 0 ->
    false;
aequal(String, Pattern, S, I, D, T) ->
    case common_prefix(String, Pattern) of
        {[], [], _Prefix} ->
            true;
        {[], Suffix2, _Prefix} when length(Suffix2) =< D ->
            approximate;
        {[], _Suffix2, _Prefix} ->
            false;
        {Suffix1, [], _Prefix} when length(Suffix1) =< I ->
            approximate;
        {_Suffix, [], _Prefix} ->
            false;
        {[X,Y|Suffix1], [Y,X|Suffix2], _Prefix} ->
            case aequal(Suffix1, Suffix2, S, I, D, T-1) of
                false ->
                    % Instead of a transposition it could be a deletion...
                    case aequal([X,Y|Suffix1], [X|Suffix2], S, I, D-1, T) of
                        false ->
                            % ...an insertion...
                            case aequal([Y|Suffix1],[Y,X|Suffix2],S,I-1,D,T) of
                                false ->
                                    % ...or even a couple of substitutions
                                    case aequal(Suffix1,Suffix2, S-2,I,D,T) of
                                        true ->
                                            approximate;
                                        Other ->
                                            Other
                                    end;
                                _WithInsertion ->
                                    approximate
                            end;
                        _WithDeletion ->
                            approximate
                    end;
                _WithTransposition ->
                    approximate
            end;
        {[H1|Suffix1], [H2|Suffix2], _Prefix} ->
            case aequal(Suffix1, Suffix2, S-1, I, D, T) of
                false ->
                    case aequal([H1|Suffix1], Suffix2, S, I, D-1, T) of
                        false ->
                            case aequal(Suffix1, [H2|Suffix2], S, I-1, D, T) of
                                true ->
                                    approximate;
                                Other ->
                                    Other
                            end;
                        _WithDeletion ->
                            approximate
                    end;
                _WithSubstitution ->
                    approximate
            end
    end.


aiequal(String, Pattern) ->
    aequal(string:to_lower(String), string:to_lower(Pattern)).

aiequal(String, Pattern, S, I, D, T) ->
    aequal(string:to_lower(String), string:to_lower(Pattern), S, I, D, T).


chop_token(String, SeparatorList) ->
    F = fun(X) -> not lists:member(X, SeparatorList) end,
    {Token, Rest} = lists:splitwith(F, strip(String, left, SeparatorList)),
    {Token, strip(Rest, left, SeparatorList)}.


chop_tokens(String, N, SeparatorList) when N > 0 ->
    chop_tokens(String, N, SeparatorList, []).

chop_tokens(String, N, _SeparatorList, Tokens) when String == ""; N == 0 ->
    {lists:reverse(Tokens), String};
chop_tokens(String, N, SeparatorList, Tokens) ->
    {Token, RestOfString} = chop_token(String, SeparatorList),
    chop_tokens(RestOfString, N-1, SeparatorList, [Token|Tokens]).


insert_every(String, Pos, Text) ->
    insert_every(String, Pos, lists:reverse(Text), 0, []).

insert_every([], _Pos, _Text, _Processed, Acc) ->
    lists:reverse(Acc);
insert_every(L, Pos, Text, Pos, Acc) ->
    insert_every(L, Pos, Text, 0, Text++Acc);
insert_every([Char|Rest], Pos, Text, Processed, Acc) ->
    insert_every(Rest, Pos, Text, Processed+1, [Char |Acc]).


is_dec([]) ->
    true;
is_dec([Digit|Rest]) when (Digit >= 48) and (Digit =< 57) ->
    is_dec(Rest);
is_dec(_String) ->
    false.


is_hex([]) ->
    true;
is_hex([Digit|Rest]) when (Digit >= 47) and (Digit =< 57);
                          (Digit >= 65) and (Digit =< 70);
                          (Digit >= 97) and (Digit =< 102) ->
    is_hex(Rest);
is_hex(_String) ->
    false.


is_atime([]) ->
    true;
is_atime([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,T,N1,N2,P]) when P == $+;
                                                                   P == $- ->
    case is_dec([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,T,N1,N2]) of
        true ->
            Date   = {list_to_integer([Y1,Y2]),
                      list_to_integer([M1,M2]),
                      list_to_integer([D1,D2])},
            Hour   = list_to_integer([H1,H2]),
            Minute = list_to_integer([Min1,Min2]),
            Second = list_to_integer([S1,S2]),
            To_UTC = list_to_integer([N1,N2]),
            case calendar:valid_date(Date) of
                true when Hour < 24, Minute < 60, Second < 60, To_UTC < 49 ->
                    true;
                _Otherwise ->
                    false
            end;
        false ->
            false
    end;
is_atime(_String) ->
    false.


is_rtime([]) ->
    true;
is_rtime([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2,$0,$0,$0,$R]) ->
    case is_dec([Y1,Y2,M1,M2,D1,D2,H1,H2,Min1,Min2,S1,S2]) of
        true ->
            Hour   = list_to_integer([H1,H2]),
            Minute = list_to_integer([Min1,Min2]),
            Second = list_to_integer([S1,S2]),
            if
                (Hour < 24) and (Minute < 60) and (Second < 60) ->
                    true;
                true ->
                    false
            end;
        false ->
            false
    end;
is_rtime(_String) ->
    false.


normalize(String) ->
    normalize(strip(String, ?WHITESPACES), []).

normalize([], Acc) ->
    lists:reverse(Acc);
normalize([H|T], Acc) when ?WHITESPACE(H) ->
    normalize(strip(T, left, ?WHITESPACES), [?SPACE|Acc]);
normalize([H|T], Acc) ->
    normalize(T, [H|Acc]).


pop_token(String, SeparatorList) ->
    {Token, Rest} = chop_token(lists:reverse(String), SeparatorList),
    {lists:reverse(Token), lists:reverse(Rest)}.


pop_tokens(String, N, SeparatorList) when N > 0 ->
    {Tokens, Rest} = chop_tokens(lists:reverse(String), N, SeparatorList),
    {[lists:reverse(T) || T <- Tokens], lists:reverse(Rest)}.


replace_chars(String, Characters, Char) ->
    Replace = fun(X) ->
                      case lists:member(X, Characters) of
                          true ->
                              Char;
                          _False ->
                              X
                        end
              end,
    [Replace(C) || C <- String].


split_by_word(String, N) ->
    split_by_word(String, N, []).

split_by_word(String, N, Acc) when String == ""; N =< 0 ->
    {lists:reverse(Acc), String};
split_by_word([H1,H2|T], N, Acc) when H1 /= $ , H2 == $ ->
    split_by_word([H2|T], N-1, [H1|Acc]);
split_by_word([H|T], N, Acc) ->
    split_by_word(T, N, [H|Acc]).


strip(String, SeparatorList) ->
    strip(String, both, SeparatorList).


strip(String, left, SeparatorList) ->
    lists:dropwhile(fun(X) -> lists:member(X, SeparatorList) end, String);
strip(String, right, SeparatorList) ->
    lists:reverse(strip(lists:reverse(String), left, SeparatorList));
strip(String, both, SeparatorList) ->
    strip(strip(String, left, SeparatorList), right, SeparatorList).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
common_prefix(String1, String2) ->
    common_prefix(String1, String2, []).

common_prefix([H|Suffix1], [H|Suffix2], Prefix) ->
    common_prefix(Suffix1, Suffix2, [H|Prefix]);
common_prefix(Suffix1, Suffix2, Prefix) ->
    {Suffix1, Suffix2, lists:reverse(Prefix)}.
