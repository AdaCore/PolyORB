%%%----------------------------------------------------------------------
%%% File    : build_manager.erl
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : API module for the application build_manager.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

-module(build_manager).
-author('quinot@shalmaneser.enst.fr').

%%-compile(export_all).
-export([add_builder/1, add_packager/1, delete_handler/1,
         notify_change/2, notify_tarball/1]).

add_builder (Server) ->
    build_manager_event:add_handler (Server, [ tarball ]).

add_packager (Server) ->
    build_manager_event:add_handler (Server, [ change ]).

delete_handler (Server) ->
    build_manager_event:delete_handler (Server).

notify_change (Branch, Change) ->
    gen_event:notify ({ global, build_manager_event }, { change, { Branch, Change } }).

notify_tarball (Path) ->
    gen_event:notify ({ global, build_manager_event }, { tarball, Path }).
