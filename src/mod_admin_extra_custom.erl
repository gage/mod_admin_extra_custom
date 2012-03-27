%%%-------------------------------------------------------------------
%%% File    : mod_admin_extra.erl
%%% Author  : Badlop <badlop@process-one.net>
%%% Purpose : Contributed administrative functions and commands
%%% Created : 10 Aug 2008 by Badlop <badlop@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%-------------------------------------------------------------------

-module(mod_admin_extra_custom).
-author('badlop@process-one.net').

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


