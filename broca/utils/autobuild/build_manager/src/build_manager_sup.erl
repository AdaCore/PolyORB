%%%----------------------------------------------------------------------
%%% File    : build_manager_sup.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : Top level supervisor for the application build_manager.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(build_manager_sup).
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
    supervisor:start_link({local, build_manager_sup}, build_manager_sup, StartArgs).

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
    AChild = {build_manager_event,{build_manager_event,start_link,[]},
	      permanent,2000,worker,[build_manager_event]},
    {ok,{{one_for_all,4,3600}, [AChild]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
