%%%% Copyright (C) 2005-2008 Wager Labs, SA
%%%%
%%%% THE WORK (AS DEFINED BELOW) IS PROVIDED UNDER THE TERMS OF THIS 
%%%% CREATIVE COMMONS PUBLIC LICENSE ("CCPL" OR "LICENSE"). THE WORK IS 
%%%% PROTECTED BY COPYRIGHT AND/OR OTHER APPLICABLE LAW. ANY USE OF 
%%%% THE WORK OTHER THAN AS AUTHORIZED UNDER THIS LICENSE OR COPYRIGHT 
%%%% LAW IS PROHIBITED.
%%%%
%%%% BY EXERCISING ANY RIGHTS TO THE WORK PROVIDED HERE, YOU ACCEPT 
%%%% AND AGREE TO BE BOUND BY THE TERMS OF THIS LICENSE. TO THE EXTENT 
%%%% THIS LICENSE MAY BE CONSIDERED TO BE A CONTRACT, THE LICENSOR GRANTS 
%%%% YOU THE RIGHTS CONTAINED HERE IN CONSIDERATION OF YOUR ACCEPTANCE 
%%%% OF SUCH TERMS AND CONDITIONS.
%%%%
%%%% Please see LICENSE for full legal details and the following URL
%%%% for a human-readable explanation:
%%%%
%%%% http://creativecommons.org/licenses/by-nc-sa/3.0/us/
%%%%

-module(schema).

%%%
%%% Database schema
%%%

-export([install/1, install/0, populate/0]).

-include("schema.hrl").
-include("common.hrl").
-include("pp.hrl").

install() ->
    install([node()]).

install(Nodes) when is_list(Nodes) ->
    mnesia:stop(),
    mnesia:delete_schema(Nodes),
    catch(mnesia:create_schema(Nodes)),
    db:start(), %% 为什么这样命名？上面是mnesia:stop()!

    %% 这个结构不如我的_mnesia.erl里面创建表的方法好
    install_counter(Nodes),
    install_player_info(Nodes),
    install_player(Nodes),
    install_balance(Nodes),
    install_inplay(Nodes),
    install_game_xref(Nodes),
    install_cluster_config(Nodes),
    install_game_config(Nodes),
    install_tourney_config(Nodes),

    %% 创建些房间的信息
    populate(),
    %% 重置Counter的信息
    reset_counters(),
    ok.

install_player_info(Nodes) ->
    %% static player info
    {atomic, ok} =
        mnesia:create_table(tab_player_info, 
                            [
                             {disc_copies, Nodes}, 
                             {index, [nick]}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_player_info)}
                            ]).

install_player(Nodes) ->
    %% player 
    {atomic, ok} =
        mnesia:create_table(tab_player, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_player)}
                            ]).

install_balance(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_balance, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_balance)}
                            ]).
install_inplay(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_inplay, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_inplay)}
                            ]).

install_game_xref(Nodes) ->
    %% online game
    {atomic, ok} =
        mnesia:create_table(tab_game_xref, 
                            [
                             {ram_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_game_xref)}
                            ]).

install_cluster_config(Nodes) ->
    %% cluster configuration
    {atomic, ok} =
        mnesia:create_table(tab_cluster_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_cluster_config)}
                            ]),
    Conf = #tab_cluster_config {
      id = 0,
      mnesia_masters = Nodes,
      test_game_pass = <<"@!%#%2E35D$%#$^">>
     },
    F = fun() -> mnesia:write(Conf) end,
    {atomic, ok} = mnesia:transaction(F).

install_game_config(Nodes) ->
    {atomic, ok} = 
        mnesia:create_table(tab_game_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_game_config)}
                            ]).

install_tourney_config(Nodes) ->
    {atomic, ok} =
        mnesia:create_table(tab_tourney_config, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_tourney_config)}
                            ]).

install_counter(Nodes) ->
    %% counter
    {atomic, ok} = 
        mnesia:create_table(tab_counter, 
                            [
                             {disc_copies, Nodes}, 
                             {type, set}, 
                             {attributes, record_info(fields, tab_counter)}
                            ]).

populate() ->
    %% 创建tab_game_config table 的一条记录
    g:setup(?GT_TEXAS_HOLDEM, 9, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 1, high = 2, min = 20, max = 200}, 
        ?START_DELAY, ?SLOW_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 9, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 1, high = 2, min = 20, max = 200}, 
        ?START_DELAY, ?FAST_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 5, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 1, high = 2, min = 20, max = 200}, 
        ?START_DELAY, ?SLOW_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 5, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 1, high = 2, min = 20, max = 200}, 
        ?START_DELAY, ?FAST_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 9, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 2, high = 4, min = 40, max = 400}, 
        ?START_DELAY, ?SLOW_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 9, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 2, high = 4, min = 40, max = 400}, 
        ?START_DELAY, ?FAST_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 5, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 2, high = 4, min = 40, max = 400}, 
        ?START_DELAY, ?SLOW_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 5, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 2, high = 4, min = 40, max = 400}, 
        ?START_DELAY, ?FAST_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 9, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 5, high = 10, min = 50, max = 1000}, 
        ?START_DELAY, ?SLOW_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 9, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 5, high = 10, min = 50, max = 1000}, 
        ?START_DELAY, ?FAST_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 5, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 5, high = 10, min = 50, max = 1000}, 
        ?START_DELAY, ?SLOW_PLAYER_TIMEOUT, 50),
    g:setup(?GT_TEXAS_HOLDEM, 5, 
        #limit{ type = ?LT_FIXED_LIMIT, low = 5, high = 10, min = 50, max = 1000}, 
        ?START_DELAY, ?FAST_PLAYER_TIMEOUT, 50).

reset_counters()->
    counter:reset(game),
    counter:reset(player),
    counter:reset(inplay_xref),
    ok.

