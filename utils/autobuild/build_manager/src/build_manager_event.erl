%%%----------------------------------------------------------------------
%%% File    : build_manager_event.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : Event handler for the auto-build manager application.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(build_manager_event).
-author('quinot@shalmaneser.enst.fr').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_event).

%% External exports
-export([start_link/0, add_handler/2,delete_handler/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-record(state, { server, events }).
% server : a gen_server reference
% events : a list of events this server is interested in.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_event:start_link({global, build_manager_event}). 

add_handler(Server, Events) ->
    io:format ('build_manager_event: add_handler (~w, ~w).~n',
      [Server, Events]),
    gen_event:add_handler({ global, build_manager_event },
                          { build_manager_event, Server },
			  #state { server = Server, events = Events }).

delete_handler (Server) ->
    gen_event:delete_handler ({ global, build_manager_event },
                              { build_manager_event, Server }),
    ok.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init(State) ->
    io:format ('build_manager_event: init (~w).~n', [State]),
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({ Event, Args },
	     State = #state{ server = Server, events = Events }) ->
    io:format ('build_manager_event: handle_event ~w.~n', [{ Event, Args }]),
    case lists:member (Event, Events) of
	true ->
	    gen_server:cast (Server, { Event, Args });
	_ ->
	    true
    end,
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
