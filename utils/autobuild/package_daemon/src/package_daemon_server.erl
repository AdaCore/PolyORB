%%%----------------------------------------------------------------------
%%% File    : package_daemon_server.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : A packaging worker. This gen_server is called by the build
%%%           manager to act upon change events.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(package_daemon_server).
-author('quinot@shalmaneser.enst.fr').

%%-compile(export_all).
%%-export([Function/Arity, ...]).

-behaviour(gen_server).

%% External exports
-export([start_link/0,start_link_pkg_loop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    io:format ('package_daemon_server: starting.~n', []),
    { ok, Pid } = gen_server:start_link({local, package_daemon_server},
         package_daemon_server, [], []),
    io:format ('package_daemon_server: started, Pid = ~w.~n', [Pid]),
    build_manager:add_packager (Pid),
    io:format ("package_daemon_server:start_link: gen_server ~w.~n",[Pid]),
    { ok, Pid }.

start_link_pkg_loop() ->
    Pid = spawn_link (?MODULE, do_pkg_loop, []),
    register (pkg_loop, Pid),
    io:format ("package_daemon_server:start_link_pkg_loop: loop ~w.~n",[Pid]),
    { ok, Pid }.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% pkg_loop ! Event,
handle_cast(Event, State) ->
    io:format ("package_daemon_server: handle_cast~w.~n", [Event]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    io:format ("gen_server shutdown: ~w~n", [ Reason ]),
    build_manager:delete_handler (whereis (package_daemon_server)),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_pkg_loop() ->
    receive
	{ change, { Branch, Change } } ->
	    { ok, Path } = make_tarball (Branch, Change),
	    build_manager:notify_tarball (Path)
    end,
    do_pkg_loop ().

make_tarball (Branch, Change) ->
    io:format ('Would make a tar ball for changelist ~w on branch ~w.~n', [ Branch, Change ]),
    { ok, '/dev/null' }.
