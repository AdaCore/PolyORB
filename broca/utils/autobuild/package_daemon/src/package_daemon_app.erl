%%%----------------------------------------------------------------------
%%% File    : package_daemon_app.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : Application callback module for the application package_daemon.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(package_daemon_app).
-author('quinot@shalmaneser.enst.fr').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(Type, StartArgs) ->
    erlang:set_cookie (node(), 'foo'),
    monitor_node ('manager@shalmaneser.enst.fr', true),
    case package_daemon_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    io:format ("package_daemon supervisor ~w started.~n", [Pid]),
	    {ok, Pid};
	Error ->
	    io:format ("package_daemon supervisor not started: ~w~n", [Error]),
	    Error
    end.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
