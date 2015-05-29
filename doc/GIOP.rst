.. _GIOP:

****
GIOP
****

.. index:: GIOP

.. _Installing_GIOP_protocol_personality:

Installing GIOP protocol personality
====================================

Ensure PolyORB has been configured and then compiled with the GIOP
protocol personality. See :ref:`Building_an_application_with_PolyORB`
for more details on how to check installed personalities.

To enable configuration of the GIOP protocol personality,
:ref:`Installation`.

.. _GIOP_Instances:

GIOP Instances
==============

GIOP is a generic protocol that can be instantiated for multiple
transport stacks. PolyORB provides three different instances.

.. _IIOP:

IIOP
----

.. index:: IIOP

Internet Inter-ORB Protocol (IIOP) is the default protocol defined by
the CORBA specifications. It is a TCP/IP, IPv4, based protocol that
supports the full semantics of CORBA requests.

.. _SSLIOP:

SSLIOP
------

.. index:: SSLIOP

The SSLIOP protocol provides transport layer security for transmitted
requests.  Its provides encryption of GIOP requests.

To build the SSLIOP, it is required to activate SSL-related features
when building PolyORB. See *--with-openssl* in :ref:`Installation` for more details.

Enabling security is completely transparent to a preexisting
application, it is also possible to phase in secure communications by
allowing incoming requests which are unsecured.

.. _DIOP:

DIOP
----

.. index:: DIOP

Datagram Inter-ORB Protocol (DIOP) is a specialization of GIOP for the
UDP/IP protocol stack. It supports only asynchronous (`oneway`)
requests.

This protocol is specific to PolyORB. DIOP 1.0 is a mapping of GIOP on
top of UDP/IP. DIOP 1.0 uses GIOP 1.2 message format.

.. _MIOP:

MIOP
----

.. index:: MIOP

Unreliable Multicast Inter-ORB Protocol (MIOP) :cite:`miop` is a
specialization of GIOP for IP/multicast protocol stack. It supports
only asynchronous (`oneway`) requests.

.. _Configuring_the_GIOP_personality:

Configuring the GIOP personality
================================

.. index:: Configuration, GIOP

The GIOP personality is configured using a configuration
file. See :ref:`Using_a_configuration_file` for more details.

Here is a summary of available parameters for each instance of GIOP.

.. _Common_configuration_parameters:

Common configuration parameters
-------------------------------

.. index:: GIOP

This section details configuration parameters common to all GIOP
instances.


::

  ###############################################################################
  # GIOP parameters
  #

  [giop]

  ###############################################################
  # Native code sets
  #
  # Available char data code sets:
  #   16#00010001#   ISO 8859-1:1987; Latin Alphabet No. 1
  #   16#05010001#   X/Open UTF-8; UCS Transformation Format 8 (UTF-8)
  #
  # Available wchar data code sets:
  #   16#00010100#   ISO/IEC 10646-1:1993; UCS-2, Level 1
  #   16#00010109#   ISO/IEC 10646-1:1993;
  #                                UTF-16, UCS Transformation Format 16-bit form
  #
  #giop.native_char_code_set=16#00010001#
  #giop.native_wchar_code_set=16#00010100#
  #
  # The following parameters force the inclusion of fallback code sets
  # as supported conversion code sets. This is required to enable
  # interoperability with ORBs whose code sets negotiation support is
  # broken. See PolyORB's Users Guide for additional information.
  #
  #giop.add_char_fallback_code_set=false
  #giop.add_wchar_fallback_code_set=false
  

.. _IIOP_Configuration_Parameters:

IIOP Configuration Parameters
-----------------------------


::


  ###############################################################################
  # IIOP parameters
  #

  [iiop]

  ###############################################################
  # IIOP Global Settings

  # Preference level for IIOP
  #polyorb.binding_data.iiop.preference=0

  # IIOP's default address
  #polyorb.protocols.iiop.default_addr=127.0.0.1

  # IIOP's default port
  #polyorb.protocols.iiop.default_port=2809

  # IIOP's alternate addresses
  #polyorb.protocols.iiop.alternate_listen_addresses=127.0.0.1:2810 127.0.0.1:2820

  # Default GIOP/IIOP Version
  #polyorb.protocols.iiop.giop.default_version.major=1
  #polyorb.protocols.iiop.giop.default_version.minor=2

  ###############################################################
  # IIOP 1.2 specific parameters

  # Set to True to enable IIOP 1.2
  #polyorb.protocols.iiop.giop.1.2.enable=true

  # Set to True to send a locate message prior to the request
  #polyorb.protocols.iiop.giop.1.2.locate_then_request=true

  # Maximum message size before fragmenting request
  #polyorb.protocols.iiop.giop.1.2.max_message_size=1000

  ###############################################################
  # IIOP 1.1 specific parameters

  # Set to True to enable IIOP 1.1
  #polyorb.protocols.iiop.giop.1.1.enable=true

  # Set to True to send a locate message prior to the request
  #polyorb.protocols.iiop.giop.1.1.locate_then_request=true

  # Maximum message size before fragmenting request
  #polyorb.protocols.iiop.giop.1.1.max_message_size=1000

  ###############################################################
  # IIOP 1.0 specific parameters

  # Set to True to enable IIOP 1.0
  #polyorb.protocols.iiop.giop.1.0.enable=true

  # Set to True to send a locate message prior to the request
  #polyorb.protocols.iiop.giop.1.0.locate_then_request=true
  

`default_addr` specifies a listening endpoint address, and
`alternate_listen_addresses` specifies a whitespace-separated list
of additional listening endpoint addresses. The value of `default_addr`,
and each element of `alternate_listen_addresses`, have a similar
format: `*<bind-addr>*[*<pub-addr>*]:*<port-hint>*`

`@it bind-addr` is the address on which to listen to, as passed
to the bind(2) system call. The default value is 0.0.0.0 (i.e., listen
for incoming connections on all addresses of the local host). If an
IP address is specified, it will be used instead. If a host name is specified,
it will be resolved, and connections will be listened for on each
returned IP address.

`pub-addr` is the address to be published in constructed object
references. In particular, this is what appears in IORs produced by
the `Object_To_String` CORBA function. If not specified, this
defaults to the same as `bind-addr`, except if `bind-addr` 
is 0.0.0.0 (the default value), in which case the default `pub-addr`
is the first non-loopback IP address found to be associated with the local
host name.

`<port-hint>` may be a specific port number, or a range of ports
separated by an hyphen. If specified, the listening port will be assigned in
the indicated range. If not, a random port will be selected by the operating
system.

Any of the three components can be omitted. The following are examples
of valid listening address specifications:


::

  0.0.0.0
  # Bind on 0.0.0.0, publish first IP address of local host

  1.2.3.4
  # Bind on 1.2.3.4, publish "1.2.3.4"

  1.2.3.4[server.example.com]
  # Bind on 1.2.3.4, publish as "server.example.com", no specified port

  server.example.com
  # Bind on all IP addresses associated with "server.example.com", publish
  # "server.example.com"

  [server.example.com]
  # Bind on 0.0.0.0, publish "server.example.com"
  

If PolyORB is compiled with GNATCOLL support, macro substitution may
be used in listening address specifications. For example, the following
setting directs PolyORB to listen on port 1234 on all local addresses,
and publish the local host name:


::

  [iiop]
  polyorb.protocols.iiop.default_addr=[${hostname}]:1234
  # <bind-addr> is unspecified, so defaults to 0.0.0.0
  # <pub-addr> is the local hostname, from built-in macro ${hostname}
  # <port-hint> is specified explicitly as 1234
  

.. _SSLIOP_Configuration_Parameters:

SSLIOP Configuration Parameters
-------------------------------


.. _Ciphers_name:

Ciphers name
^^^^^^^^^^^^

PolyORB's SSLIOP uses the OpenSSL library to support all ciphers
recommended by CORBA 3.0.3. The OpenSSL library uses specific names for
ciphers. The table below contains CORBA-recommended cipher names and
their OpenSSL equivalents:

@multitable @columnfractions .6 .4
* CORBA recommended ciphers               @tab OpenSSL equivalent
* TLS_RSA_WITH_RC4_128_MD5                @tab RC4-MD5
* SSL_RSA_WITH_RC4_128_MD5                @tab RC4-MD5
* TLS_DHE_DSS_WITH_DES_CBC_SHA            @tab EDH-DSS-CBC-SHA
* SSL_DHE_DSS_WITH_DES_CBC_SHA            @tab EDH-DSS-CBC-SHA
* TLS_RSA_EXPORT_WITH_RC4_40_MD5          @tab EXP-RC4-MD5
* SSL_RSA_EXPORT_WITH_RC4_40_MD5          @tab EXP-RC4-MD5
* TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA   @tab EXP-EDH-DSS-DES-CBC-SHA
* SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA   @tab EXP-EDH-DSS-DES-CBC-SHA
@end multitable

.. _SSLIOP_Parameters:

SSLIOP Parameters
^^^^^^^^^^^^^^^^^


::

  ###############################################################################
  # SSLIOP parameters
  #

  [ssliop]

  ###############################################################
  # SSLIOP Global Settings

  # SSLIOP's default port
  #polyorb.protocols.ssliop.default_port=2810
  # If no SSLIOP default address is provide, PolyORB reuses IIOP's
  # address

  # Private Key file name
  #polyorb.protocols.ssliop.privatekeyfile=privkey.pem

  # Certificate file name
  #polyorb.protocols.ssliop.certificatefile=cert.pem

  # Trusted CA certificates file
  #polyorb.protocols.ssliop.cafile=cacert.pem

  # Trusted CA certificates path
  #polyorb.protocols.ssliop.capath=demoCA/certs

  # Disable unprotected invocations
  #polyorb.protocols.ssliop.disable_unprotected_invocations=true

  ###############################################################
  # Peer certificate verification mode

  # Verify peer certificate
  #polyorb.protocols.ssliop.verify=false

  # Fail if client did not return certificate. (server side option)
  #polyorb.protocols.ssliop.verify_fail_if_no_peer_cert=false

  # Request client certificate only once. (server side option)
  #polyorb.protocols.ssliop.verify_client_once=false
  

.. _DIOP_Configuration_Parameters:

DIOP Configuration Parameters
-----------------------------


::

  ###############################################################
  # DIOP Global Settings

  # Preference level for DIOP
  #polyorb.binding_data.diop.preference=0

  # DIOP's default address
  #polyorb.protocols.diop.default_addr=127.0.0.1

  # DIOP's default port
  #polyorb.protocols.diop.default_port=12345

  # Default GIOP/DIOP Version
  #polyorb.protocols.diop.giop.default_version.major=1
  #polyorb.protocols.diop.giop.default_version.minor=2

  ###############################################################
  # DIOP 1.2 specific parameters

  # Set to True to enable DIOP 1.2
  #polyorb.protocols.diop.giop.1.2.enable=true

  # Maximum message size
  #polyorb.protocols.diop.giop.1.2.max_message_size=1000

  ###############################################################
  # DIOP 1.1 specific parameters

  # Set to True to enable DIOP 1.1
  #polyorb.protocols.diop.giop.1.1.enable=true

  # Maximum message size
  #polyorb.protocols.diop.giop.1.1.max_message_size=1000

  ###############################################################
  # DIOP 1.0 specific parameters

  # Set to True to enable DIOP 1.0
  #polyorb.protocols.diop.giop.1.0.enable=true
  

.. _MIOP_Configuration_Parameters:

MIOP Configuration Parameters
-----------------------------


::

  ###############################################################################
  # MIOP parameters
  #

  [miop]

  ###############################################################
  # MIOP Global Settings

  # Preference level for MIOP
  #polyorb.binding_data.uipmc.preference=0

  # Maximum message size
  #polyorb.miop.max_message_size=6000

  # Time To Leave parameter
  #polyorb.miop.ttl=15

  # Multicast address to use
  # These two parameters must be set explicitly, no default value is provided.
  # If either parameter is unset, the MIOP access point is disabled.
  #polyorb.miop.multicast_addr=<group-ip-address>
  #polyorb.miop.multicast_port=<port-number>

  # Set to True to enable MIOP
  #polyorb.protocols.miop.giop.1.2.enable=false

  # Maximum message size
  #polyorb.protocols.miop.giop.1.2.max_message_size=1000
  

.. _Code_sets:

Code sets
=========

.. index:: Code sets, GIOP

This sections details the various steps required to add support
for new character code sets to PolyORB's GIOP personality. Please
refer to the CORBA specifications (:cite:`corba`) section 13.10 for more
details on this topic.

.. _Supported_code_sets:

Supported code sets
-------------------

PolyORB supports the following list of code sets:

* Available char data code sets:

  * 16#00010001#   ISO 8859-1:1987; Latin Alphabet No. 1
  * 16#05010001#   X/Open UTF-8; UCS Transformation Format 8 (UTF-8)
* Available wchar data code sets:

  * 16#00010100#   ISO/IEC 10646-1:1993; UCS-2, Level 1
  * 16#00010109#   ISO/IEC 10646-1:1993; UTF-16, UCS Transformation Format 16-bit form

.. _Incompatibility_in_code_set_support:

Incompatibility in code set support
-----------------------------------

Some ORBs report incompatiblity in code sets because fallback
converters are not explicitly present in the reference.
To work around this issue, you may use the following parameters:


::

  [giop]
  giop.add_char_fallback_code_set=true
  giop.add_wchar_fallback_code_set=true
  

.. _Adding_support_for_new_code_sets:

Adding support for new code sets
--------------------------------

PolyORB allows users to extend the set of supported native character
code sets. Adding support for new character code set consists of the
following steps:

* Developing sets of Converters - special objects that do
  marshalling/unmarshalling operations of character data. At least two
  Converters are required: for direct marshalling character data in
  native code set and for marshalling/unmarshalling character data in
  fallback character code set (UTF-8 for char data and UTF-16 for wchar
  data). Additional Converters may be developed for marshalling character
  data in conversion code set.

* Developing converter factory subprogram for each Converter.

* Registering native code set, its native and fallback converters
  and optional conversion char sets and its converters.

.. _Character_data_Converter:

Character data Converter
------------------------

Character data converters do direct marshalling/unmarshalling of
character data (char or wchar - depending on `Converter`)
into/from PolyORB's buffer. This allows to minimize the speed penalty on
character data marshalling.

Character data Converters for char data have the following API (from
:file:`PolyORB.GIOP_P.Code_Sets.Converters` package:


::

     type Converter is abstract tagged private;

     procedure Marshall
       (C      : Converter;
        Buffer : access Buffers.Buffer_Type;
        Data   : Types.Char;
        Error  : in out Errors.Error_Container)
        is abstract;

     procedure Marshall
       (C      : Converter;
        Buffer : access Buffers.Buffer_Type;
        Data   : Types.String;
        Error  : in out Errors.Error_Container)
        is abstract;

     procedure Unmarshall
       (C      : Converter;
        Buffer : access Buffers.Buffer_Type;
        Data   :    out Types.Char;
        Error  : in out Errors.Error_Container)
        is abstract;

     procedure Unmarshall
       (C      : Converter;
        Buffer : access Buffers.Buffer_Type;
        Data   :    out Types.String;
        Error  : in out Errors.Error_Container)
        is abstract;
  

The Marshall subprograms do marshalling of one character or string of
characters into the buffer. The Unmarshall subprograms do unmarshalling
of one character or string of characters from the buffer.

*Note: Depending on the item size of the data (char/wchar) and GIOP version, marshalling/unmarshalling algorithms may vary. In some situations marshalling of string is not equivalent to marshalling its length and marshalling one by one each character. Please refere to GIOP specifications for more details.*

If marshalling/unmarshalling fails, subprograms must set the Error
parameter to the corresponding error, usually `Data_Conversion_E`.

*Note: We recommend to always use the Data_Conversion_E error code with Minor status 1.*

All `Converters` (native, fallback and conversion) have similar
APIs.  Wchar data converters differ only in parameter type.

.. _Converters_factories:

Converters factories
--------------------

To create new converters, PolyORB uses special factory subprograms
with the following profile:


::

     function Factory return Converter_Access;
  

or


::

     function Factory return Wide_Converter_Access;
  

This function must allocate a new `Converter` and initialize its state.

.. _Registering_new_code_sets:

Registering new code sets
-------------------------

Registering new native character data code sets begins from
registering new native character data code sets and its native and
fallback `Converters`. This is done using
`Register_Native_Code_Set`:


::

     procedure Register_Native_Code_Set
       (Code_Set : Code_Set_Id;
        Native   : Converter_Factory;
        Fallback : Converter_Factory);
  

or


::

     procedure Register_Native_Code_Set
       (Code_Set : Code_Set_Id;
        Native   : Wide_Converter_Factory;
        Fallback : Wide_Converter_Factory);
  

If you have additional conversion code sets Converters you may
register it by calling Register_Conversion_Code_Set subprogram:


::

     procedure Register_Conversion_Code_Set
       (Native     : Code_Set_Id;
        Conversion : Code_Set_Id;
        Factory    : Converter_Factory);
  

or


::

     procedure Register_Conversion_Code_Set
       (Native     : Code_Set_Id;
        Conversion : Code_Set_Id;
        Factory    : Wide_Converter_Factory);
  

Note: because of incompatibility in the support of code sets
negotiation in some ORB's it is recommend to recognize two boolean
PolyORB's parameters:


::

  [giop]
  giop.add_char_fallback_code_set=false
  giop.add_wchar_fallback_code_set=false
  

and also register a fallback Converter as conversion Converter if the
corresponding parameter is set to True.

Finally, define your preferred native character data code sets by
parameters (only integer code sets codes now supported):


::

  [giop]
  giop.native_char_code_set=16#00010001#
  giop.native_wchar_code_set=16#00010100#
  

