%%% Copyright (c) 2009 Nomasystems, S.L., All Rights Reserved
%%%
%%% This file contains Original Code and/or Modifications of Original Code as
%%% defined in and that are subject to the Nomasystems Public License version
%%% 1.0 (the 'License'). You may not use this file except in compliance with
%%% the License. BY USING THIS FILE YOU AGREE TO ALL TERMS AND CONDITIONS OF
%%% THE LICENSE. A copy of the License is provided with the Original Code and
%%% Modifications, and is also available at www.nomasystems.com/license.txt.
%%%
%%% The Original Code and all software distributed under the License are
%%% distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
%%% EXPRESS OR IMPLIED, AND NOMASYSTEMS AND ALL CONTRIBUTORS HEREBY DISCLAIM
%%% ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
%%% NON-INFRINGEMENT. Please see the License for the specific language
%%% governing rights and limitations under the License.
-module(cl_pool).

%%% EXTERNAL EXPORTS
-export([add/2, del/2, items/1, new/1, next/1]).

%%% MACROS
-define(CL_POOL_TAB, cl_pool_tab).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
add(Name, Item) ->
    Items = ets:lookup_element(Name, items, 2),
    true = ets:insert(Name, {items, [Item | Items]}),
    ok.


del(Name, Item) ->
    Items = ets:lookup_element(Name, items, 2),
    true = ets:insert(Name, {items, lists:delete(Item, Items)}),
    ok.


items(Name) ->
    ets:lookup_element(Name, items, 2).


new(Name) ->
    Name = ets:new(Name, [public, named_table]),
    true = ets:insert(Name, {next, -1}),
    true = ets:insert(Name, {items, []}),
    ok.


next(Name) ->
    try
        Items = ets:lookup_element(Name, items, 2),
        Next = (ets:update_counter(Name, next, 1) rem length(Items)) + 1,
        lists:nth(Next, Items)
    catch
        _Class:_Reason ->
            erlang:throw(not_found)
    end.

