-module(mod_admin_extra_custom).
-author('gage.tseng@geniecapital.com').

-behaviour(gen_mod).

-export([start/2, stop/1,
	 %% Roster
	 auto_friend/4,
	 auto_friends/3,
	 delete_rosteritem/4
	]).
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_jid.hrl").
-include("ejabberd.hrl").
-include("ejabberd_commands.hrl").
-include("mod_roster.hrl").
-include("jlib.hrl").


%%%
%%% gen_mod
%%%

start(_Host, _Opts) ->
    ejabberd_commands:register_commands(commands()).

stop(_Host) ->
    ejabberd_commands:unregister_commands(commands()).


%%%
%%% Register commands
%%%

commands() ->

    [
     #ejabberd_commands{name = auto_friend, tags = [roster],
		desc = "Add an item to a user's roster",
		module = ?MODULE, function = auto_friend,
		args = [{localuser, string}, {localserver, string},
			{user, string}, {server, string}],
		result = {res, rescode}},
	
    #ejabberd_commands{name = auto_friends, tags = [roster],
   		desc = "Add items to a user's roster",
   		module = ?MODULE, function = auto_friends,
   		args = [{localuser, string}, {localserver, string},
   			{users, string}],
   		result = {res, rescode}},
	
     #ejabberd_commands{name = delete_rosteritem, tags = [roster],
			desc = "Delete an item from a user's roster",
			module = ?MODULE, function = delete_rosteritem,
			args = [{localuser, string}, {localserver, string},
				{user, string}, {server, string}],
			result = {res, rescode}}
    ].


%%%
%%% Roster
%%%

auto_friend(LocalUser, LocalServer, User, Server) ->
    case subscribe(list_to_binary(LocalUser), list_to_binary(LocalServer), list_to_binary(User), list_to_binary(Server)) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
	
auto_friends(LocalUser, LocalServer, Users) ->
	USERS = re:split(Users, ":", [{return, list}]),
    case subscribe_all(LocalUser, LocalServer, USERS) of
	ok ->
	    ok;
	error ->
	    error
    end.

subscribe_all(LU, LS, Users) ->
	case Users of
		[User | Others] ->
		    case subscribe(list_to_binary(LU), list_to_binary(LS), list_to_binary(User), list_to_binary(LS)) of
			{atomic, ok} ->
				subscribe_all(LU, LS, Others),
			    ok;
			_ ->
			    error
		    end;
		[] ->
			ok
	end.

subscribe(LU, LS, User, Server) ->
    mnesia:transaction(
      fun() ->
	      mnesia:write({rosteritem,
			    {LU,LS,{User,Server,undefined}}, % uj
			    <<"">>,                  % name: "Mom", []
			    both,  % subscription: none, to=you see him, from=he sees you, both
			    none,          % ask: out=send request, in=somebody requests you, none
				<<"">>
			   })
      end),
    mnesia:transaction(
      fun() ->
          mnesia:write({rosteritem,
		      {User,Server,{LU,LS,undefined}}, % uj
		      <<"">>,                  % name: "Mom", []
		      both,  % subscription: none, to=you see him, from=he sees you, both
		      none,          % ask: out=send request, in=somebody requests you, none
			  <<"">>
		     })
      end).
	

delete_rosteritem(LocalUser, LocalServer, User, Server) ->
    case unsubscribe(list_to_binary(LocalUser), list_to_binary(LocalServer), list_to_binary(User), list_to_binary(Server)) of
	{atomic, ok} ->
	    ok;
	_  ->
	    error
    end.

unsubscribe(LU, LS, User, Server) ->
    mnesia:transaction(
      fun() ->
              mnesia:delete({rosteritem, {LU, LS, {User, Server,undefined}}})
      end).


