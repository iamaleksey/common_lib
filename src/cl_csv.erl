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
-module(cl_csv).

%%% EXTERNAL EXPORTS
-export([process_file/2, read/1]).

%%% MACROS
-define(SEPARATOR, $;).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
process_file(Filename, Fun) ->
    true = filelib:is_regular(Filename),
    for_each_line_in_file(Filename, Fun, [read]).


read(String) -> read(String, []).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
read([], Acc) ->
 	lists:reverse(Acc);
read(String, []) ->
	{Line, Rest} = read_line(String),
    read(Rest, [Line]);
read([10|String], Acc) ->
	{Line, Rest} = read_line(String),
    case Line of
        [] ->
            read(Rest, Acc);
        _ ->
            read(Rest, [Line|Acc])
    end;
read([13,10|String], Acc) ->
 	{Line, Rest} = read_line(String),
    case Line of
        [] ->
            read(Rest, Acc);
        _ ->
            read(Rest, [Line|Acc])
    end.


add_spaces(0, String) ->
    String;
add_spaces(Count, String) ->
    add_spaces(Count-1, [$ |String]).


read_item([34|T]) ->
    read_item_quoted(T, []);
read_item(Other) ->
    read_item(Other, 0, []).


read_item([32|T], 0, []) ->
    read_item(T, 0, []);
read_item([9|T], 0, []) ->
    read_item(T, 0, []);
read_item([10|T], _SpaceCount, Acc) ->
    {lists:reverse(Acc), [10|T]};
read_item([13,10|T], _SpaceCount, Acc) ->
    {lists:reverse(Acc), [13,10|T]};
read_item([?SEPARATOR|T], _SpaceCount, Acc) ->
    {lists:reverse(Acc), [?SEPARATOR|T]};
read_item([], _SpaceCount, Acc) ->
    {lists:reverse(Acc), []};
read_item([9|T], SpaceCount, Acc) ->
    read_item(T, SpaceCount+1, Acc);
read_item([32|T], SpaceCount, Acc) ->
    read_item(T, SpaceCount+1, Acc);
read_item([C|T], SpaceCount, Acc) ->
    read_item(T, 0, [C|add_spaces(SpaceCount, Acc)]).


read_item_quoted([34,34|T], Acc) ->
    read_item_quoted(T, [34|Acc]);
read_item_quoted([34|T], Acc) ->
    {lists:reverse(Acc), T};
read_item_quoted([C|T], Acc) ->
    read_item_quoted(T, [C|Acc]).


read_line(String) ->
    read_line(String,[]).


read_line([10|T], Acc) ->
    {lists:reverse(Acc), [10|T]};
read_line([13,10|T], Acc) ->
    {lists:reverse(Acc), [13|T]};
read_line([], Acc) ->
    {lists:reverse(Acc), []};
read_line(String, []) ->
    {Item, Rest} = read_item(String),
    read_line(Rest, [Item]);
read_line([?SEPARATOR|String], Acc) ->
    {Item, Rest} = read_item(String),
    read_line(Rest, [Item|Acc]).


for_each_line_in_file(Name, Fun, Mode) ->
    {ok, Device} = file:open(Name, Mode),
    for_each_line(Device, Fun).

for_each_line(Device, Fun) ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device), ok;
        Line ->
            [Args] = read(Line),
            case Fun(Args) of
                    ok ->
                        for_each_line(Device, Fun);
                    _ ->
                        {error, {parsing_line, Line}}
                end
    end.
