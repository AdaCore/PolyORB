.. _Building_an_application_with_PolyORB:

************************************
Building an application with PolyORB
************************************

.. index:: Configuration, PolyORB

.. _Compile-time_configuration:

Compile-time configuration
==========================

The user may configure some elements of a PolyORB application at
compile-time.

.. _Tasking_runtimes:

Tasking runtimes
----------------

PolyORB provides several tasking runtimes. The user may select the
most appropriate one, depending on application requirements. The
tasking runtimes determine the constructs PolyORB may use for its
internal synchronizations.

* `No_Tasking`: There is no dependency on the Ada tasking
  runtime, middleware is mono-task.

* `Full_Tasking`: Middleware uses Ada tasking constructs,
  middleware can be configured for multi-tasking.

* `Ravenscar` : Middleware uses Ada
  tasking constructs, with the limitations of the Ravenscar profile
  :cite:`burns98ravenscar`.  Middleware can be configured for multi-tasking.
  .. index:: Ravenscar


See :ref:`Tasking_model_in_PolyORB` for more information on this point.

.. _Middleware_tasking_policies:

Middleware tasking policies
---------------------------

PolyORB provides several tasking policies. A tasking policy defines
how tasks are used by the middleware to process incoming requests.

* `No_Tasking`: There is only one task in middleware,
  processing all requests.

* `Thread_Per_Session`: One task monitors communication
  entities. One task is spawned for each active connection. This task
  handles all incoming requests on this connection.

* `Thread_Per_Request`: One task monitors communication
  entities. One task is spawned for each incoming request.

* `Thread_Pool`: A set of tasks cooperate to handle all
  incoming requests.


See :ref:`Tasking_model_in_PolyORB` for more information on this point.

.. _Sample_files:

Sample files
------------

PolyORB provides a set of predefined setup packages. You must `with'
one of them in your application node to activate the corresponding
setup.

* `PolyORB.Setup.No_Tasking_Client`: a client node, without any
  tasking support, configured to use all protocol personalities built with
  PolyORB. Note that this configuration should not be used with multiple
  application tasks.

* `PolyORB.Setup.Thread_Pool_Client`: a client node, with tasking
  enabled, configured to use all protocol personalities built with PolyORB.
  This configuration places no restriction on the use of tasking by
  application code. Middleware tasking policy is `Thread_Pool`.

* `PolyORB.Setup.Ravenscar_TP_Server`: a server node, with tasking
  enabled, configured to use all protocol personalities built with
  PolyORB. Middleware tasking runtime follows Ravenscar's profile
  restrictions. Middleware tasking policy is `Thread_Pool`.

* `PolyORB.Setup.Thread_Per_Request_Server`: a server node,
  with tasking enabled, configured to use all protocol personalities
  built with PolyORB. Middleware tasking policy is
  `Thread_Per_Request`.

* `PolyORB.Setup.Thread_Per_Session_Server`: a server node,
  with tasking enabled, configured to use all protocol personalities
  built with PolyORB. Middleware tasking policy is
  `Thread_Per_Session`.

* `PolyORB.Setup.Thread_Pool_Server`: a server node,
  with tasking enabled, configured to use all protocol personalities
  built with PolyORB. Middleware tasking policy is
  `Thread_Pool`.


To use one of these configurations, add a dependency on one of
these packages, for example,
`with PolyORB.Setup.Thread_Pool_Server;`. The elaboration of the
application (based on Ada rules) and the initialization of the partition
(based on the application personalities mechanisms) will properly set up
your application.

.. _Run-time_configuration:

Run-time configuration
======================

The user may configure some elements of a PolyORB application at
run time.

Using the default configurations provided by PolyORB, the parameters
are read in the following order: command line, environment variables,
configuration file. PolyORB will use the first value that matches the
searched parameter.

.. _Using_a_configuration_file:

Using a configuration file
--------------------------

.. index:: :file:`polyorb.conf`

.. index:: `POLYORB_CONF`

A configuration file may be used to configure a PolyORB node. A sample
configuration file may be found in :file:`src/polyorb.conf`.

The syntax of the configuration file is:

* empty lines and lines that have a '#' in column 1 are ignored;

* sections can be started by lines of the form
  `[ SECTION-NAME ]`;

* variable assignments can be performed by lines of the form
  `VARIABLE-NAME = VALUE`.

  Any variable assignment is local to a section.

  Assignments that occur before the first section declaration are
  relative to section [environment].  Section and variable names are
  case sensitive.

  Furthermore, each time a value starts with `"file:"`, the contents
  of the file are used instead.

Default search path for :file:`polyorb.conf` is current
directory. Environment variable `POLYORB_CONF` may be used to
set up information on configuration file.

PolyORB's configuration file allows the user to

* enable/disable the output of debug information
* set up default reference on naming service
* select the default protocol personality
* set up each protocol personality

The configuration file is read once when running a node, during
initialization. Look in the sample configuration file
:file:`src/polyorb.conf` to see the available sections and variables.

.. _Using_environment_variables:

Using environment variables
---------------------------

A variable `Var.Iable` in section `[Sec]` can be overridden
by setting environment variable `"POLYORB_SEC_VAR_IABLE"`.

.. _Using_the_command_line:

Using the command line
----------------------

PolyORB allows to set up configuration variables on the command
line. The syntax is close to the one described in configuration files.
A variable `Var.Iable` in section `[Sec]` can be overridden
with flag `--polyorb-<sec>-<var>-<iable>[=<value>]`.

.. _Using_a_source_file:

Using a source file
-------------------

Many embedded systems do not have a filesystem or a shell, so the
previous run-time configuration methods cannot be used on these targets.
On these platforms, a PolyORB node can also be configured using the API of package
`PolyORB.Parameters.Static`. An example configuration file may be
found in :file:`examples/static/po_static_conf.ads`.

An array of PolyORB parameters of type `Static_Parameters_Array` is
first declared containing a list of pairs of Variable and Value strings.
The syntax is close to the one described in configuration files.
A variable `Var.Iable` in section `[Sec]` is specified
as the pair of strings `"[sec]var.iable", "<value>"`.

There is no need to with this :file:`po_static_conf.ads` in the application
source code, the only requirement is that the array is exported with the
external name "`__polyorbconf_optional`". This allows to modify
PolyORB parameters without recompiling the application, just relinking
it. For example:


::

    $ gnatmake -c po_static_conf.ads `polyorb-config`
    $ gnatmake -b -l server.adb `polyorb-config` -largs po_static_conf.o
  

Note the `-l` flag to gnatmake for linking only, and the need to
specify to the linker the object file with the array using `-largs`
if no package withs it.

It should be noticed that this static array of parameters is read at
elaboration time only, this API cannot be used to modify the PolyORB
configuration at run-time.

.. _Macros:

Macros
------

If PolyORB is compiled with GNATCOLL support, macros can be used
in the configuration file, and will be expanded automatically.

Macros can be defined by setting parameters in the `[macros]` section
of the runtime configuration. The following macros are predefined:



*hostname*
  The local host name

Macro references can appear anywhere in runtime parameter values and
are of the form `$*macro-name*` or `${*macro-name*}`.

For example, in order for a single setting to control all GIOP-based
binding modules, one can specify:


::

  [macros]
  giop_enable=true
  # ... or false

  [modules]
  binding_data.iiop=${giop_enable}
  binding_data.iiop.ssliop=${giop_enable}
  binding_data.diop=${giop_enable}
  binding_data.uipmc=${giop_enable}
  

.. _Setting_up_protocol_personalities:

Setting up protocol personalities
=================================

PolyORB allows the user to activate some of the available protocol
personalities and to set up the preferred protocol. Protocol-specific
parameters are defined in their respective sections.

.. _Activating/Deactivating_protocol_personalities:

Activating/Deactivating protocol personalities
----------------------------------------------

.. index:: Protocol personality, activation

Protocol activation is controlled by PolyORB's configuration file.

The section `[access_points]` controls the initialization of
*access points*. An access point is a node entry point that may
serve incoming requests.


::

  [access_points]
  soap=enable
  iiop=enable
  diop=disable
  uipmc=disable
  

This example activates SOAP and IIOP, but deactivates DIOP and MIOP.

The section `[modules]` controls the activation/deactivation of
some modules within PolyORB. It is used to enable *bindings* to
remote entities.


::

  [modules]
  binding_data.soap=enable
  binding_data.iiop=enable
  binding_data.diop=disable
  binding_data.uipmc=disable
  

This example enables the creation of bindings to remote objects using
SOAP or IIOP. Objects cannot be reached using DIOP or UIPMC.

*Note: by default, all configured personalities are activated.*

.. _Configuring_protocol_personality_preferences:

Configuring protocol personality preferences
--------------------------------------------

The user may affect a *preference* to each protocol
personality. The protocol with the higher preference will be selected
among possible protocols to send a request to a remote node.

See `polyorb.binding_data.<protocol>.preference` in
section `[protocol]` to set up protocol's preference.

Possible protocols are defined as the protocols available on the
remote node, as advertised in its *object reference*.  `IOR`
or `corbaloc` references may support multiple protocols;
`URI` references support only one protocol.

Each protocol supports a variety of configuration parameters, please
refer to the protocols' sections for more details.

.. _Activating_debugging_traces:

Activating debugging traces
===========================

.. index:: Debugging traces

To activate the output of debug information, you must first configure
and compile PolyORB with debugging traces activated (which is the default,
unless your build is configured with *--enable-debug-policy=ignore*).

To output debugging traces on a selected package, create a
configuration file with a `[log]` section and the name of the
packages for which you want debug information:


::

  # Sample configuration file, output debug for PolyORB.A_Package
  [log]
  polyorb.a_package=debug
  

Note that some packages may not provide such information. See the sample
configuration file :file:`src/polyorb.conf` for the complete list of
packages that provide traces.

A default logging level may be specified using a line of the form

::

  default=<level>
  

Time stamps may optionally be prepended to every generated trace.
This is enabled using:

::

  timestamp=true
  

.. _Tracing_exceptions:

Tracing exceptions
==================

.. index:: Exceptions

To trace exception propagation in PolyORB's source code, activate debugging
traces for package `PolyORB.Exceptions`.

.. _*polyorb.gpr*:

*polyorb.gpr*
=============

.. index:: *polyorb.gpr*

This section describes how to build your program using project files.
An alternative method, using *polyorb-config*, is described in the
following section. *polyorb-config* is intended primarily for Unix-like
systems. The project-file method will work on all supported systems.

To build your application, create a project file as usual.
Import the *polyorb.gpr* project by putting `with "polyorb";`
in your project file.

Set the ADA_PROJECT_PATH environment variable to point to the directory
containing *polyorb.gpr*, which is *<prefix>/lib/gnat*.
If SOAP is being used, ADA_PROJECT_PATH must also be set so we can find
*xmlada.gpr*.

If your project file is *my_proj.gpr*, you can build it by saying:


::

    $ gnatmake -P my_proj
  

See the GNAT User's Guide and the GNAT Reference Manual for more information
on project files.

.. _*polyorb-config*:

*polyorb-config*
================

.. index:: *polyorb-config*

*polyorb-config* returns path and library information on
PolyORB's installation.  It can be used on the *gnatmake*
command line, like this:


::

    $ gnatmake my_program.adb `polyorb-config`
  


::

  NAME
         polyorb-config  - script to get information about the installed version
         of PolyORB.

  SYNOPSIS
         polyorb-config [--prefix[=DIR]] [--exec-prefix[=DIR]] [--version|-v]
         [--config] [--libs] [--cflags] [--idls] [--help]

  DESCRIPTION
         polyorb-config  is  a  tool  that is used to determine the compiler and
         linker flags that should be used to compile and link programs that  use
         PolyORB.

  OPTIONS
         polyorb-config accepts the following options:

         --prefix[=DIR]
                  Output the directory in which PolyORB architecture-independent
                 files are installed, or set this directory to DIR.

         --exec-prefix[=DIR]
                  Output the directory in which  PolyORB  architecture-dependent
                 files are installed, or set this directory to DIR.

         --version
                 Print  the  currently installed version of PolyORB on the stan-
                 dard output.

         --config
                 Print the configuration of the currently installed  version  of
                 PolyORB on the standard output.

         --libs  Print  the  linker  flags  that are necessary to link a PolyORB
                 program.

         --cflags
                 Print the compiler flags that are necessary to compile a  Poly-
                 ORB program.

         --idls
                 Output flags to set up path to CORBA's IDL for idlac.

         --with-appli-perso=P,P,P
                 Restrict output to only those flags relevant to the listed
                 applicative personalities.

         --with-proto-perso=P,P,P
                 Restrict output to only those flags relevant to the listed
                 protocol personalities.

         --with-corba-services=S,S,S
                Restrict output to only those flags relevant to the listed
                services.

         --help  Print help message.

  

