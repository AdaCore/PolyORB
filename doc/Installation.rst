.. _Installation:

************
Installation
************


.. _Supported_Platforms:

Supported Platforms
===================

PolyORB has been compiled and successfully tested on the following
platforms:

* AIX
* FreeBSD
* HP-UX
* Linux
* MacOS X
* Solaris
* Tru64
* VxWorks
* Windows

*Note: PolyORB should compile and run on every target for which
GNAT and the `GNAT.Sockets` package are available.*

.. _Build_requirements:

Build requirements
==================

GNU tar is required to unpack PolyORB source packages.

Ada compiler:

* GNAT Pro 6.2.* or later
* GNAT GPL 2009 or later
* FSF GCC 4.4 or later

For builds for cross targets, both a native and a cross compiler are
required, as some tools (like an IDL-to-Ada compiler) are meant for
use on the build host.

A Python interpreter is required for installation.

Optional:

* (Only for older versions of GNAT, and only if you want to build
  the CORBA application personality): A C++ compiler. The OMG IDL
  specification mandates that IDL source files be preprocessed according
  to standard C++ preprocessing rules. Newer versions of GNAT provide an
  integrated IDL preprocessor. This feature is detected and used
  automatically. However, for older versions of GNAT, PolyORB relies on
  an external preprocessor provided by a suitable C++ compiler. Please
  refer to the documentation of your particular version of GNAT to know
  if it supports this feature.

* XML/Ada (`http://libre.adacore.com/libre/tools/xmlada/ <http://libre.adacore.com/libre/tools/xmlada/>`_) if you
  want to build the SOAP protocol personality.

Note: per construction, the macro *configure* used to find
your GNAT compiler looks first for the executable *gnatgcc*,
then *adagcc* and finally *gcc* to find out which
Ada compiler to use. You should be very careful with your path and
executables if you have multiple GNAT versions installed. See the
explanation below on the ADA environment variable if you need to
override the default guess.

.. _Build_instructions:

Build instructions
==================

To compile and install PolyORB, execute:


::

    $ ./configure [some options]
    $ make
    $ make install
  

This will install files in standard locations.  If you want to choose
a prefix other than :file:`/usr/local`, give configure a
*--prefix=whereveryouwant* argument.

NOTE: you must use GNU make (version 3.80 or later) to build PolyORB.

.. _Additional_instructions_for_cross_platforms:

Additional instructions for cross platforms
===========================================

The :file:`RANLIB` environment variable must be set to the path of the
cross :file:`ranlib` prior to running :file:`configure` with the
appropriate `--target` option.

For example, for VxWorks 5 execute:

::

    $ export RANLIB=ranlibppc
    $ ./configure --target=powerpc-wrs-vxworks [some options]
    $ make
    $ make install
  

Only one PolyORB installation (native or cross) is currently possible
with a given `--prefix`. If both a native and a cross
installation are needed on the same machine, distinct prefixes must be
used.

Use `./configure --help` for a full list of available configuration
switches.

.. _Building_the_documentation_and_PolyORB's_examples:

Building the documentation and PolyORB's examples
=================================================

PolyORB's documentation and examples are built separately.

To build the examples, run *make examples* in the root
directory.  The build process will only build examples that correspond
to the personalities you configured. Note that some examples require the
CORBA COS Naming and IR services to be enabled (using
*--enable-corba-services="naming ir"* on the *configure*
command line).

Similarly, to build the documentation, run *make docs*.

You may install PolyORB's documentation in a standard location using
*make install*.

.. _Build_Options:

Build Options
=============

Available options for the 'configure' script include:


* *--with-appli-perso="..."*: application personalities to build

  Available personalities: CORBA, DSA, MOMA

  e.g.   *--with-appli-perso="corba moma"* to build both the CORBA
  and MOMA personalities

* *--with-proto-perso="..."*: protocol personalities to build

  Available personalities: GIOP, SOAP

  e.g.   *--with-proto-perso="giop soap"* to build both the GIOP
  and SOAP personalities

* *--with-idl-compiler="..."*: select IDL compiler

  Available IDL compilers: iac (default), idlac

  e.g.  *--with-idl-compiler=''iac''* to build iac

* *--with-corba-services="..."*: CORBA COS services to build

  Available services: event, ir, naming, notification, time

  e.g.  *--with-corba-services="event naming"* to build only
  COS Event and COS Naming.


By default, only the CORBA and GIOP personalities are built, and no
CORBA Services are built.


* *--with-openssl*: build SSL support and SSL dependent
  features, including the IIOP/SSLIOP personality

* *--help*: list all options available

* *--enable-shared*: build shared libraries.

* *--enable-debug*:  enable debugging information generation
  and supplementary runtime checks. Note that this option has a significant
  space and time cost, and is not recommended for production use.


.. _Compiler,_Tools_and_Run-Time_libraries_Options:

Compiler, Tools and Run-Time libraries Options
==============================================

The following environment variables can be used to override configure's
guess at what compilers to use:


* `CC`:  the C compiler
* `ADA`: the Ada compiler (e.g. gcc, gnatgcc or adagcc)
* `CXXCPP`, `CXXCPPFLAGS`: the preprocessor used by
  the IDL-to-Ada compiler (only when setting up the CORBA application
  personality). CORBA specifications require this preprocessor to be
  compatible with the preprocessing rules defined in the C++
  programming language specifications.


For example, if you have two versions of GNAT installed and available
in your `PATH`, and configure picks the wrong one, you can
indicate what compiler should be used with the following (assuming
Bourne shell syntax):


::

    $ ADA=/path/to/good/compiler/gcc ./configure [options]
  

PolyORB will be compiled with GNAT build host's configuration,
including run-time library. You may override this setting using
`ADA_INCLUDE_PATH` and `ADA_OBJECTS_PATH` environment
variables. See GNAT User's Guide for more details.

You can add specific build options to GNAT using the
`EXTRA_GNATMAKE_FLAGS` variable:


::

    $ EXTRA_GNATMAKE_FLAGS=--RTS=rts-sjlj ./configure [options]
  

You can also pass compiler-only flags using the `ADAFLAGS` variable.

NOTE: Developers building PolyORB from the version control repository
will need to rebuild the configure script and other files. To do so, from
the top-level source directory, run the support/reconfig script after each
update from the repository. In addition to the requirements above, developers
will need autoconf 2.57 or newer, automake 1.6.3 or newer, and libtool 1.5.8
or newer.

.. _Platform_notes:

Platform notes
==============

Solaris (all versions):

/usr/ucb/tr and /usr/bin/tr are not suitable to build PolyORB. Your PATH must
be set to that tr(1) is /use/xpg4/bin/tr or GNU tr. (However note that
if you have GNU make in /usr/local/bin, then /usr/xpg4/bin must occur
*after* /usr/local/bin in your PATH, since /usr/xpg4/bin/make is
not suitable to build PolyORB.

So, assuming GNU make is installed in /usr/local/bin, a suitable PATH
setting would be: PATH=/usr/local/bin:/usr/xpg4/bin:/usr/ccs/bin:/usr/bin.

Tru64 5.1A:

The default maximal data segment size may not be sufficient to compile
PolyORB. If a GNAT heap exhausted error message occurs during build,
try raising this limit using:

::

  ulimit -d unlimited
  

AIX 5.2:

PolyORB must be compiled with the -mminimal-toc compiler switch. This is
taken care of automatically by the PolyORB configure script.

The 'ulimit' command may be needed as for Tru64 (see above).

HP-UX 11.00:

The version of install(1) from /opt/imake/bin on HP-UX is not suitable
for installing PolyORB. Make sure that /opt/imake/bin is not on the PATH
when building and installing PolyORB.

