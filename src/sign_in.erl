-module(sign_in).

%%%
%%% 用户注册处理
%%%
-compile(export_all).
%% -export([sign_in/3]).

-include("common.hrl").
-include("pp.hrl").
-include("test.hrl").
-include("schema.hrl").

sign_in(Nick, Pass, Socket) 
    when is_binary(Nick),
         is_binary(Pass) ->
    io:format("sign_in:sign_in(~p, ~p, ~p)~n", [Nick, Pass, Socket]),
    case player:create(Nick, Pass, <<"Test">>, 0) of
        {error, Error} ->
            {error, Error};
        {ok, ID} ->
            {ok, ID}
    end.

