.. _Overview_of_PolyORB_personalities:

*********************************
Overview of PolyORB personalities
*********************************

.. index:: Personalities

A personality is an instantiation of specific PolyORB components. It
provides the mechanisms specified by a distribution model, e.g. an
API, a code generator or a protocol stack.

This section provides a brief overview of existing personalities.

*Note: some of these personalities are available only through
PolyORB's repository.*

.. _Application_personalities:

Application personalities
=========================

.. index:: Application personalities

Application personalities constitute the adaptation layer between
application components and middleware. They provide APIs and/or a code
generator to register application entities with PolyORB's core, and
interoperate with the core to allow the exchange of requests with
remote entities.

.. _CORBA:

CORBA
-----

.. index:: CORBA

.. index:: RT-CORBA

.. index:: CORBA, COS Services

CORBA is the OMG specification of a Distributed Object Computing (DOC)
distribution model (:cite:`corba`). It is now a well-known and
well-established specification, used in a wide range of industrial
applications.

PolyORB provides a CORBA-compatible implementation based on a mapping of
the IDL language version 1.2 described in :cite:`corba-ada-mapping1.2:2001` and CORBA
core specifications. PolyORB also provides an implementation of
various additional specifications described by the OMG, including
`COS Services: COS Naming, Notification, Event, Time`, and
additional specifications: `RT-CORBA`, `PortableInterceptors`,
`DynamicAny`.

.. _Distributed_Systems_Annex_of_Ada_(DSA):

Distributed Systems Annex of Ada (DSA)
--------------------------------------

.. index:: DSA, Distributed Systems Annex

The Distributed Systems Annex of Ada (DSA) :cite:`ada-rm` is a normative
part of the language specification. It was first introduced in the
'Ada 95' revision of the language (:cite:`ada-rm95`). It describes remote
invocation schemes applied to most language constructs.

.. _Message_Oriented_Middleware_for_Ada_(MOMA):

Message Oriented Middleware for Ada (MOMA)
------------------------------------------

.. index:: MOMA, Message Oriented Middleware for Ada

MOMA (Message Oriented Middleware for Ada) provides message passing
mechanisms. It is an Ada adaptation of Sun's Java Message Service
(JMS) :cite:`jms`, a standardized API for common message passing
models.

.. _Protocol_personalities:

Protocol personalities
======================

.. index:: Protocol personality

Protocol personalities handle the mapping of requests
(representing interactions between application entities) onto messages
exchanged through a communication network, according to a specific
protocol.

.. _GIOP:

GIOP
----

.. index:: GIOP

GIOP is the transport layer of the CORBA specifications. GIOP is a
generic protocol. This personality implements GIOP versions from 1.0
to 1.2 along with the CDR representation scheme to map data types
between the neutral core layer and CDR streams. It also provides the
following dedicated instances:

* IIOP supports synchronous request semantics over TCP/IP,
  .. index:: IIOP

* IIOP/SSIOP supports synchronous request semantics using SSL sockets,
  .. index:: SSLIOP

* MIOP instantiation of GIOP enables group communication over
  IP multicast,
  .. index:: MIOP

* DIOP relies on UDP/IP communications to transmit one-way
  requests only.
  .. index:: DIOP


.. _SOAP:

SOAP
----

.. index:: SOAP

The SOAP protocol :cite:`soap12primer` enables the exchange of structured
and typed information between peers. It is a self-describing XML
document :cite:`soap12primer` that defines both its data and
semantics. Basically, SOAP with `HTTP` bindings is used as a
communication protocol for Web Services.

