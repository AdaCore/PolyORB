%%%----------------------------------------------------------------------
%%% File    : package_daemon_sup.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : Top level supervisor for the application package_daemon.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(package_daemon_sup).
-author('quinot@shalmaneser.enst.fr').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, package_daemon_sup}, package_daemon_sup, StartArgs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init(StartArgs) ->
    AChild = {package_daemon_server,{package_daemon_server,start_link,[]},
	      permanent,2000,worker,[package_daemon_server]},
    %% BChild = {package_daemon_pkg_loop,{package_daemon_server,start_link_pkg_loop,[]},
    %%          permanent,2000,worker,[package_daemon_server]},
    io:format ("In package_daemon_sup:init/1.~n"),
    {ok,{{one_for_all,4,3600}, [AChild]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
