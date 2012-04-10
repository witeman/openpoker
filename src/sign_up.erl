-module(sign_up).


%%%
%%% 用户注册处理
%%%
-compile(export_all).
%% -export([sign_in/3]).

-include("common.hrl").
-include("pp.hrl").
-include("test.hrl").
-include("schema.hrl").

sign_up(Nick, Pass, Socket) 
    when is_binary(Nick),
         is_binary(Pass) ->
    io:format("sign_up:sign_up(~p, ~p, ~p)~n", [Nick, Pass, Socket]),
    case player:create(Nick, Pass, <<"Test">>, 10000) of
        {error, Error} ->
            {error, Error};
        {ok, ID} ->
            {ok, ID}
    end.

