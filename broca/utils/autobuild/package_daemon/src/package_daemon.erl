%%%----------------------------------------------------------------------
%%% File    : package_daemon.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : API module for the application package_daemon.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(package_daemon).
-author('quinot@shalmaneser.enst.fr').

%%-compile(export_all).
-export([start_application/0]).

start_application() ->
  application:start(package_daemon).
