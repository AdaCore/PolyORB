%%%----------------------------------------------------------------------
%%% File    : build_daemon.app
%%% Author  : Thomas Quinot <quinot@shalmaneser.enst.fr>
%%% Purpose : Application specification file for build_daemon.
%%% Created : 24 Apr 2000 by Thomas Quinot <quinot@shalmaneser.enst.fr>
%%%----------------------------------------------------------------------

{application, build_daemon,
 [{description, "build_daemon"},
  {vsn, "0.1"},
  {modules, [build_daemon,
	     build_daemon_app,
	     build_daemon_sup,
	     build_daemon_server]},
  {registered, [build_daemon_server,build_daemon_sup]},
  {applications, [kernel,
		  stdlib,
		  sasl,
		  mnesia,
		  build_manager]},
  {env, []},
  {mod, {build_daemon_app, []}}]}.
