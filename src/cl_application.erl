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
-module(cl_application).

%%% EXTERNAL EXPORTS
-export([get_env/2, get_env/3, get_env_opt/2, get_env_opt/3]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
get_env(App, Key) ->
    case application:get_env(App, Key) of
        {ok, Val} ->
            Val;
        _Undefined ->
            erlang:throw({undefined, Key})
    end.

get_env(App, Key, Fun) ->
    case application:get_env(App, Key) of
        {ok, Val} ->
            case Fun(Val) of
                true ->
                    Val;
                false ->
                    erlang:throw({invalid, {Key, Val}})
            end;
        _Undefined ->
            erlang:throw({undefined, Key})
    end.

get_env_opt(App, Key) ->
    case application:get_env(App, Key) of
        {ok, Val} ->
            Val;
        undefined ->
            undefined
    end.

get_env_opt(App, Key, Fun) ->
    case application:get_env(App, Key) of
        {ok, Val} ->
            case Fun(Val) of
                true ->
                    Val;
                false ->
                    erlang:throw({invalid, {Key, Val}})
            end;
        undefined ->
            undefined
    end.

