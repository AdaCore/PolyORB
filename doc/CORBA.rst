.. _CORBA:

*****
CORBA
*****

.. index:: CORBA

.. _What_you_should_know_before_Reading_this_section:

What you should know before Reading this section
================================================

This section assumes that the reader is familiar with the CORBA
specifications described in :cite:`corba` and the *IDL-to-Ada*
mapping defined in :cite:`corba-ada-mapping1.2:2001`.

.. _Installing_CORBA_application_personality:

Installing CORBA application personality
========================================

Ensure PolyORB has been configured and then compiled with the CORBA
application personality. See :ref:`Building_an_application_with_PolyORB`
for more details on how to check installed personalities.

To build the CORBA application personality, :ref:`Installation`.

.. _IDL-to-Ada_compiler:

IDL-to-Ada compiler
===================

PolyORB provides two IDL-to-Ada compilers:

* *iac* is the new, optimized PolyORB IDL-to-Ada compiler.
* *idlac* is the legacy PolyORB IDL-to-Ada compiler,

.. _Usage_of_*iac*:

Usage of *iac*
--------------

.. index:: *iac*

*iac* is PolyORB's new IDL-to-Ada compiler. It supports
many command line parameters to control code generation optimizations
such as use of static hashing for deterministic request dispatching,
and optimized GIOP marshalling for CORBA applications.


::

  NAME
         iac - PolyORB's IDL-to-Ada compiler

  SYNOPSIS
         iac [options] file [-cppargs args...]

  DESCRIPTION
         iac is an IDL-to-Ada compiler, compliant with version 1.2 of the
         'Ada Language Mapping Specification' produced by the OMG.

  OPTIONS
         iac accepts the following options:

    -h       Print this help message, and do nothing else

    file is the name of the .idl file (.idl suffix optional)

    -E       Preprocess only
    -k       Keep temporary files
    -o DIR   Output directory (DIR must exist)
    -p       Produce source on standard output
    -q       Quiet mode

    -dm      Generate debug messages when analyzing scopes

    -df      Dump the frontend tree (the IDL tree)

    -cppargs Pass arguments to the C++ preprocessor
    -I <dir> Shortcut -cppargs -I directory. Use this flag
             for the imported entities
    -nocpp   Do not preprocess input

    -gnatW8  Use UTF-8 character encoding in Ada output.
             (Default is Latin-1.)

    -<lang>  Generate code for one of the following languages:

     types   Generate a list of all types present in the IDL file
             -p       Print the list generated

     ada     (default) Generate Ada source code
             -i       Generate implementation packages
             -c       Generate code for client side only
             -s       Generate code for server side only
             -d       Generate delegation package (defunct)
             -ir      Generate code for interface repository
             -noir    Do not generate code for interface repository (default)
             -hc      Minimize CPU time in perfect hash tables in skels
             -hm      Minimize memory use in perfect hash tables in skels
                      This is the default.
             -rs      Use the SII/SSI to handle requests
             -rd      Use the DII/DSI to handle requests (default)
             -da      Dump the Ada tree
             -db      Generate only the package bodies
             -ds      Generate only the package specs
             -dw      Output the withed entities
             -dt      Output tree warnings
             -di      Generate code for imported entities

     idl     Dump parsed IDL file
             -b n     Base to output integer literals
                      As a default (zero) use base from input
             -e       Expand IDL Tree
             -df      Dump IDL Tree (may be used in conjunction with -e
                      to dump the expanded IDL tree)
             -di      Output IDL code of imported entities  (may be
                      used in conjunction with -e to output the
                      expanded IDL code)

  EXIT STATUS
         iac returns one of the following values upon exit:

         0      Successful completion

         1      Usage error

         2      Illegal IDL specification

  

*iac* creates several files :


* `myinterface.ads`, `myinterface.adb` : these files contain
  the mapping for user defined types (client and server side).

* `myinterface-impl.ads`, `myinterface-impl.adb` :
  these files are to be filled in by the user. They contain the
  implementation of the server. They are generated only if the -i flag
  is specified.

* `myinterface.ads`, `myinterface.adb` : these files
  contain the client stubs for the interface.

* `myinterface-skel.ads`, `myinterface-skel.adb` : these files
  contain the server-side skeletons for the interface.

* `myinterface-helper.ads`, `myinterface-helper.adb` : these
  files contain subprograms to marshal data into CORBA Any containers.

* `myinterface-ir_info.ads`, `myinterface-ir_info.adb` : these
  files contain code for registering IDL definitions in the CORBA
  Interface Repository. They are generated only if the `'-ir'` flag
  is specified.

* `myinterface-cdr.ads`, `myinterface-cdr.adb` : these
  files contain code for optimized CDR marshalling of GIOP
  messages. They are generated only if the `'-rs'` flag is
  specified.


.. _Usage_of_*idlac*:

Usage of *idlac*
----------------

.. index:: *idlac*

*idlac* is PolyORB's IDL-to-Ada compiler.


::

  NAME
         idlac - PolyORB's IDL-to-Ada compiler

  SYNOPSIS
         idlac [-Edikpqv] [-[no]ir] [-gnatW8] [-o DIR] idl_file [-cppargs ...]

  DESCRIPTION
         idlac is an IDL-to-Ada compiler, compliant with version 1.2 of the
         'Ada Language Mapping Specification' produced by the OMG.

  OPTIONS
         idlac accepts the following options:

         -E      Preprocess only.

         -d      Generate delegation package.

         -i      Generate implementation template.

         -s      Generate server side code.

         -c      Generate client side code.

         -k      Keep temporary files.

         -p      Produce source on standard output.

         -q      Be quiet (default).

         -v      Be verbose.

         -ir     Generate code for interface repository.

         -noir   Don't generate code for interface repository (default).

         -gnatW8
                 Use UTF8 character encoding

         -o DIR Specify output directory

         -cppargs ARGS
                 Pass ARGS to the C++ preprocessor.

         -I dir  Shortcut for -cppargs -I dir.

  EXIT STATUS
         idlac returns one of the following values upon exit:

         0      Successful completion

         1      Usage error

         2      Illegal IDL specification

  

*idlac* creates several files :


* `myinterface.ads`, `myinterface.adb` : these files contain
  the mapping for user defined types (client and server side).

* `myinterface-impl.ads`, `myinterface-impl.adb` :
  these files are to be filled in by the user. They contain the
  implementation of the server. They are generated only if the -i flag
  is specified.

* `myinterface.ads`, `myinterface.adb` : these files
  contain the client stubs for the interface.

* `myinterface-skel.ads`, `myinterface-skel.adb` : these files
  contain the server-side skeletons for the interface.

* `myinterface-helper.ads`, `myinterface-helper.adb` : these
  files contain subprograms to marshal data into CORBA Any containers.

* `myinterface-ir_info.ads`, `myinterface-ir_info.adb` : these
  files contain code for registering IDL definitions in the CORBA Interface
  Repository. They are generated only if the `'-ir'` flag is specified.


.. _Difference_between_idlac_and_iac:

Difference between idlac and iac
--------------------------------

This section lists the main differences between *idlac* and
*iac*

* *iac* is backward compatible with *idlac*, but
  lacks the following feature:

  * generation of delegation files.

*iac* implements additional name clash resolution rules.
When the name of an IDL operation clashes with a primitive operation
of Ada.Finalization.Controlled (of which CORBA.Object.Ref is a derived
type), it is prefixed with "IDL_" in generated sources.

.. _Resolving_names_in_a_CORBA_application:

Resolving names in a CORBA application
======================================

PolyORB implements the CORBA COS Naming service.

.. _*po_cos_naming*:

*po_cos_naming*
---------------

.. index:: *po_cos_naming*

.. index:: *CORBA COS Naming*

*po_cos_naming* is a standalone server that supports the CORBA
COS Naming specification. When launched, it returns its `IOR` and
`corbaloc`, which can then be used by other CORBA applications.

If you want *po_cos_naming* to return the same `IOR` or
`corbaloc` at each startup, you must set a default listen port
for the protocol personalities you use. See :ref:`Configuring_protocol_personality_preferences` for more details.

*po_cos_naming* can output its `IOR` directly to a file
using the *-file <filename>* flag.  This, in conjonction with
the *'file://'* naming scheme provided by *CORBA*,
provides a convenient way to store initial references to the Naming
Service.


::

  Usage: po_cos_naming
   -file <filename> : output COS Naming IOR to 'filename'
   -help : print this help
   [PolyORB command line configuration variables]
  

.. _Registering_the_reference_to_the_COS_Naming_server:

Registering the reference to the COS Naming server
--------------------------------------------------

You have two ways to register the reference to the root context of the
COS Naming server the application will use:

* Setting up the `name_service` entry in the `[corba]`
  section in your configuration file, `name_service` is the
  `IOR` or `corbaloc` of the COS Naming server to use. See
  :ref:`Using_a_configuration_file` for more details.

* Registering an initial reference using the `-ORBInitRef NamingService=<IOR>` or `-ORBInitRef NamingService=<corbaloc>`
  command-line argument. See the CORBA specifications for more details.

* Registering an initial reference for
  `NamingService` using the
  `CORBA.ORB.Register_Initial_Reference` function. See the CORBA
  specifications for more details.


.. _Using_the_COS_Naming:

Using the COS Naming
--------------------

PolyORB provides a helper package to manipulate the COS Naming in your
applications. See :ref:`PolyORB_specific_APIs` for more details.

.. _The_CORBA_Interface_Repository:

The CORBA Interface Repository
==============================

PolyORB implements the CORBA Interface Repository.

.. _*po_ir*:

*po_ir*
-------

.. index:: *po_ir*

*po_ir* is a standalone server that supports the CORBA
Interface Repository. When launched, it returns its `IOR` and
`corbaloc`, which can then be used by other CORBA applications.

If you want *po_ir* to return the same `IOR` or
`corbaloc` at each startup, you must set a default listen port
for the protocol personalities you use. See :ref:`Configuring_protocol_personality_preferences` for more details.

.. _Using_the_Interface_Repository:

Using the Interface Repository
------------------------------

The IDL-to-Ada compiler generates a helper package that allows you to
register all entities defined in your IDL specification in the
Interface Repository.

.. _Building_a_CORBA_application_with_PolyORB:

Building a CORBA application with PolyORB
=========================================


.. _`echo`_example:

`echo` example
--------------

We consider building a simple 'Echo' CORBA server and client. This
application echoes a string. The source code for this example is
located in the :file:`examples/corba/echo` directory in the PolyORB
distribution. This applications uses only basic elements of CORBA.

To build this application, you need the following pieces of code:

* IDL definition of an `echo` object
* Implementation code for the `echo` object
* Code for client and server nodes

IDL definition of an `echo` object
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This interface defines an `echo` object with a unique method
`echoString`. Per construction, this method returns its argument.

.. literalinclude:: echo.idl
   :language: idl

Implementation code for the `echo` object
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Package `Echo.Impl` is an implementation of this interface. This
implementation follows the *IDL-to-Ada* mapping.

.. literalinclude:: echo-impl.ads
   :language: ada
.. literalinclude:: echo-impl.adb
   :language: ada

Note: the body of `Echo.Impl` must have a dependency on
`Echo.Skel` to ensure the elaboration of skeleton code and the
correct setup of PolyORB's internals.

Test code for client and server nodes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Client and server code demonstrate how to make a remote invocation on
a CORBA object, and how to set up an object on a server node.

Note: the dependency on `PolyORB.Setup.Client` or
`PolyORB.Setup.No_Tasking_Server` enforces compile-time
configuration, see :ref:`Sample_files`.

* Client code tests a simple remote invocation on
  an object. It is a no-tasking client. A reference to the object is built
  from a stringified reference (or `IOR`), which is passed on command
  line.

.. literalinclude:: client.adb
   :language: ada

* The server code sets up a no-tasking node. The object is registered to
  the `RootPOA`. Then an `IOR` reference is built to enable
  interaction with other nodes.

.. literalinclude:: server.adb
   :language: ada


Compilation and execution
^^^^^^^^^^^^^^^^^^^^^^^^^

To compile this demo,

* Process the IDL file with *idlac* (or *iac*)

  ::

     $ idlac echo.idl
    

* Compile the client node

  ::

     $ gnatmake client.adb `polyorb-config`
    

* Compile the server node

  ::

     $ gnatmake server.adb `polyorb-config`
    


Note the use of backticks (`). This means that
*polyorb-config* is first executed, and then the command line
is replaced with the output of the script, setting up library and
include paths and library names.

To run this demo:

* run :file:`server`, the server outputs its IOR, a hexadecimal
  string with the IOR: prefix:

  ::

    $ ./server
    Loading configuration from polyorb.conf
    No polyorb.conf configuration file.
    'IOR:01534f410d00000049444c3[..]'
    

* In another shell, run :file:`client`, passing cut-and-pasting the
  complete IOR on the command line:

  ::

    $ ./client 'IOR:01534f410d00000049444c3[..]'
    Echoing string: " Hello Ada ! "
    I said : Hello Ada !
    The object answered : Hello Ada !
    

.. _Other_examples:

Other examples
--------------

PolyORB provides other examples to test other CORBA features. These
examples are located in the :file:`example/corba` directory in the PolyORB
distribution.

* :file:`all_functions` tests CORBA parameter passing modes
  (`in`, `out`, ..);
* :file:`all_types` tests CORBA types;
* :file:`echo` is a simple CORBA demo;
* :file:`random` is a random number generator;
* :file:`send` tests MIOP specific API.

.. _Configuring_a_CORBA_application:

Configuring a CORBA application
===============================

.. index:: Configuration, CORBA

To configure a CORBA application, you need to separately configure
PolyORB and the GIOP protocol (or any other protocol personality you
wish to use).

.. _Configuring_PolyORB:

Configuring PolyORB
-------------------

Please refer to :ref:`Building_an_application_with_PolyORB` for more
information on PolyORB's configuration.

.. _Configuring_GIOP_protocol_stack_for_PolyORB:

Configuring GIOP protocol stack for PolyORB
-------------------------------------------

The GIOP protocol is separated from the CORBA application
personality. See :ref:`Configuring_the_GIOP_personality` for more
information on GIOP's configuration.

.. _Configuring_Security_services_for_PolyORB:

Configuring Security services for PolyORB
-----------------------------------------

PolyORB provides support for some elements of the CORBA Security
mechanisms. This sections lists the corresponding configuration
parameters.

Supported mechasnisms
^^^^^^^^^^^^^^^^^^^^^

PolyORB provides support for the following security mechanisms:

* SSL/TLS protected transport;

* GSSUP (user/password) authentication mechanism;

* identity assertion and backward trust evaluation.

Compile-time configuration
^^^^^^^^^^^^^^^^^^^^^^^^^^

To enable security support, applications must `with' one of the
predefined setup packages:

* `PolyORB.Setup.Secure_Client` - for client side support only;

* `PolyORB.Setup.Secure_Server` - for both client and server
  side support.

Run-time configuration
^^^^^^^^^^^^^^^^^^^^^^


* Capsule configuration

  This section details the configuration parameters for capsule
  configuration.


  ::

    [security_manager]
    # List of sections for configure client's credentials
    #own_credentials=my_credentials
    #
    # Client requires integrity proteced messages
    #integrity_required=true
    #
    # Client requires confiodentiality protected messages
    #confidentiality_required=true
    #
    # Client requires security association to detect replay (not supported
    for now)
    #detect_replay_required=true
    #
    # Client requires security association to detect message sequence
    errors (not
    # supported for now)
    #detect_misordering_required=true
    #
    # Client requires target authentication
    #establish_trust_in_target_required=true
    #
    # Client requires client authentication (usually not applicable at
    all)
    #establish_trust_in_client_required=true
    #
    # (rare useful)
    #identity_assertion_required=true
    #
    # (rare useful)
    #delegation_by_client_required=true
    

* Credentials configuration

  This section details configuration parameters for defining a program's
  credentials. Depending on the mechanisms used for the transport and
  authentication layers, the credentials configuration section may define
  configuration only for one transport mechanism and/or one
  authentication mechanism.


  ::

    #[my_credentials]
    #
    # TLS protected transport mechanism used as transport mechanism
    #transport_credentials_type=tls
    #
    # Connection method. Available methods: tls1, ssl3, ssl2
    #tls.method=tls1
    #
    # Certificate file name
    #tls.certificate_file=my.crt
    #
    # Certificate chain file name
    #tls.certificate_chain_file=
    #
    # Private key file name
    #tls.private_key_file=my.key
    #
    # Name of file, at which CA certificates for verification purposes are
    #located
    #tls.certificate_authority_file=root.crt
    #
    # Name of directory, at which CA certificates for verification
    #purposes are
    # located
    #tls.certificate_authority_path=
    #
    # List of available ciphers
    #tls.ciphers=ALL
    #
    # Verify peer certificate
    #tls.verify_peer=true
    #
    # Fail if client don't provide ceritificate (server only)
    #tls.verify_fail_if_no_peer_certificate=true
    #
    # GSSUP (user/password) mechanism as authentication mechanism
    #authentication_credentials_type=gssup
    #
    # User name
    #gssup.username=username@domain
    #
    # User password
    #gssup.password=password
    #
    # Target name for which user/password pair is applicable
    #gssup.target_name=@domain
    

* POA configuration

  This section details configuration parameters for defining security
  characteristics of objects managed by POA. The POA's name is used as
  the section name.


  ::

    #[MySecurePOA]
    #
    # Unprotected invocations is allowed
    #unprotected_invocation_allowed=true
    #
    # Section name for configuration of used protected transport mechanism
    #(if any)
    #transport_mechanism=tlsiop
    #
    # Section name for configuration of used authentication mechanism (if
    #any)
    #authentication_mechanism=my_gssup
    #
    # Target require client authentication at authentication layer (in
    #addition
    # to authentication at transport layer)
    #authentication_required=true
    #
    # Name of file for backward trust evalutation rules
    #backward_trust_rules_file=file.btr
    #
    # Section name for configuration of authorization tokens authority
    #privilege_authorities=
    

* TLS protected transport mechanism configuration

  This section details configuration parameters for the TLS protected
  transport mechanism. The section name for mechanism configuration is
  defined in the POA configuration.


  ::

    [tlsiop]
    # List of access points
    #addresses=127.0.0.1:3456
    

* GSSUP authentication mechanism

  This section details configuration parameters for the GSSUP
  authentication mechanism. The section name for mechanism configuration
  is defined in the POA configuration.


  ::

    #[my_gssup]
    #
    # Authentication mechanism
    #mechanism=gssup
    #
    # Target name
    #gssup.target_name=@domain
    #
    # User name/password mapping file
    #gssup.passwd_file=passwd.pwd
    

.. _Command_line_arguments:

Command line arguments
----------------------

The CORBA specifications define a mechanism to pass command line
arguments to your application, using the `CORBA::ORB:Init`
method.

For now, PolyORB supports the following list of arguments:

* `InitRef` to pass initial reference.


.. _Implementation_Notes:

Implementation Notes
====================

PolyORB strives to support CORBA specifications as closely as
possible. However, on rare occasions, the implementation adapts the
specifications to actually enable its completion. This section
provides information on the various modifications we made.

.. _Tasking:

Tasking
-------

PolyORB provides support for tasking and no-tasking, using
configuration parameters. Please refer to :ref:`Building_an_application_with_PolyORB` for more information on PolyORB's
configuration.

When selecting a tasking-capable runtime, ORB-related functions are
thread safe, following the IDL-to-Ada mapping recommendations.

.. _Implementation_of_CORBA_specifications:

Implementation of CORBA specifications
--------------------------------------

In some cases, the CORBA specifications do not describe the
semantics of the interface in sufficient detail. We add an
`Implementation Notes` tag to the package specification to
indicate the modifications or enhancements we made to the standard.

In some cases, the IDL-to-Ada mapping specifications and the CORBA
specifications conflict. We add an `Implementation Notes` tag to
the package specification to indicate this issue. Whenever possible,
PolyORB follows the CORBA specifications.

.. _Additions_to_the_CORBA_specifications:

Additions to the CORBA specifications
-------------------------------------

In some cases, the specifications lack features that may be
useful. We add an `Implementation Notes` tag to the package
specification to detail the additions we made to the standard.

In addition to the above, PolyORB follows some of the recommendations
derived from the OMG Issues for Ada 2003 Revision Task Force mailing
list (see `http://www.omg.org/issues/ada-rtf.html <http://www.omg.org/issues/ada-rtf.html>`_ for more
information).

.. _Interface_repository:

Interface repository
--------------------

*The documentation of the PolyORB's CORBA Interface Repository will appear in a future revision of PolyORB.*

.. _Policy_Domain_Managers:

Policy Domain Managers
----------------------

You have two ways to register the reference to the CORBA Policy Domain
Manager the application will use:

* Setting up the `policy_domain_manager` entry in the
  `[corba]` section in your configuration file,
  `policy_domain_manager` is the `IOR` or `corbaloc` of
  the COS Naming server to use. See :ref:`Using_a_configuration_file` for
  more details.

* Registering an initial reference using the `-ORB InitRef PolyORBPolicyDomainManager=<IOR>` or `-ORB InitRef PolyORBPolicyDomainManager=<corbaloc>` command-line argument. See the
  CORBA specifications for more details.

* Registering an initial reference for
  `PolyORBPolicyDomainManager` using the
  `CORBA.ORB.Register_Initial_Reference` function. See the CORBA
  specifications for more details.


.. _Mapping_of_exceptions:

Mapping of exceptions
---------------------

For each exception defined in the CORBA specifications, PolyORB
provides the `Raise_<excp_name>` function, a utility function
that raises the exception `<excp_name>`, along with its exception
member. PolyORB also defines the `Get_Members` function (as
defined in the IDL-to-Ada mapping) to provide accessors to retrieve
information on the exception.

In addition, for each exception defined in a user-defined IDL
specification, the IDL-to-Ada compiler will generate a
`Raise_<excp_name>` function in the Helper package. It is a
utility function that raises the exception `<excp_name>`, along
with its exception member.

.. _Additional_information_to_`CORBA::Unknown`:

Additional information to `CORBA::Unknown`
------------------------------------------

.. index:: `CORBA::Unknown`

.. index:: CORBA, Server-side exception

When a CORBA application raises an Ada exception that is not part of
the IDL specifications, nor defined by the CORBA specifications, then
this exception is translated into a `CORBA::UNKNOWN` exception.

To help debugging CORBA applications, PolyORB supports a specific
service context to the GIOP protocol personality that conveys
exception information. When displaying exception information,
server-side specific exception information is delimited by
*'<Invocation Exception Info: ...>'*

Here is an example from the `all_types` example provided by
PolyORB.


::

  Exception name: CORBA.UNKNOWN
  Message: 4F4D0001M
  <Invocation Exception Info: Exception name: CONSTRAINT_ERROR
  Message: all_types-impl.adb:315 explicit raise
  Call stack traceback locations:
  0x84d279c 0x84c1e78 0x84b92c6 0x84b8e9>
  Call stack traceback locations:
  0x81d0425 0x81d0554 0x81d6d8c 0x81fd02b 0x81fc091 0x82eea12 0x83e4c22 0x807b69a 0xb7a15e3e
  

Note that call stack tracebacks can be translated into symbolic form
using the `addr2line` utility that comes with GNAT.

.. _Internals_packages:

Internals packages
------------------

PolyORB sometimes declares internal types and routines inside CORBA
packages. These entities are gathered into an
`Internals` child package. You should not use these functions:
they are not portable, and may be changed in future releases.

.. _PolyORB's_specific_APIs:

PolyORB's specific APIs
=======================

PolyORB defines packages to help in the development of CORBA programs.


* :ref:`PolyORB.CORBA_P.CORBALOC`:

  This package defines a helper function to build a `corbaloc`
  stringified reference from a CORBA object reference.

* :ref:`PolyORB.CORBA_P.Naming_Tools`:

  This package defines helper functions to ease interaction with CORBA
  COS Naming.

* :ref:`PolyORB.CORBA_P.Server_Tools`:

  This package defines helper functions to ease set up of a simple CORBA
  Server.


.. _`PolyORB.CORBA_P.CORBALOC`:

`PolyORB.CORBA_P.CORBALOC`
--------------------------

.. index:: `PolyORB.CORBA_P.CORBALOC`

.. literalinclude:: ../src/corba/polyorb-corba_p-corbaloc.ads
   :language: ada

.. _`PolyORB.CORBA_P.Naming_Tools`:

`PolyORB.CORBA_P.Naming_Tools`
------------------------------

.. index:: `PolyORB.CORBA_P.Naming_Tools`

.. literalinclude:: ../src/corba/polyorb-corba_p-naming_tools.ads
   :language: ada

.. _`PolyORB.CORBA_P.Server_Tools`:

`PolyORB.CORBA_P.Server_Tools`
------------------------------

.. index:: `PolyORB.CORBA_P.Server_Tools`

.. literalinclude:: ../src/corba/polyorb-corba_p-server_tools.ads
   :language: ada

