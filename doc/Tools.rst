.. _Tools:

*****
Tools
*****


.. _*po_catref*:

*po_catref*
===========

.. index:: *po_catref*

*po_catref* is a utility for viewing the components of a
stringified reference (CORBA IOR, corbaloc or URI). The reference's
components include references to access an object through multiple
protocols (e.g. CORBA IIOP, SOAP) and configuration parameters
associated with a reference (e.g. GIOP Service Contexts).


::

  Usage:
         po_catref <stringified reference>
  

*Note: @command{po_catref* can only process protocols PolyORB has
been configured with.}

.. _*po_dumpir*:

*po_dumpir*
===========

.. index:: *po_dumpir*

*po_dumpir* is a utility for viewing the content of an
instance of the CORBA Interface Repository.


::

  Usage:
         po_dumpir <stringified reference>
  

*Note: @command{po_dumpir* will be compiled and installed only if
the CORBA personality and the :file:`ir` service is compiled. Please
see :ref:`Building_an_application_with_PolyORB` for more details on how
to set up PolyORB.}

.. _*po_names*:

*po_names*
==========

.. index:: *po_names*

*po_names* is a stand-alone name server. It has an interface
similar to CORBA COS Naming, without dragging in any dependencies on
CORBA mechanisms. This name server is to be used when the CORBA
application personality is not required, e.g. with the DSA or MOMA
application personalities.

@appendix Performance considerations

This section discusses performance when using PolyORB. Many elements
can be configured, :ref:`Building_an_application_with_PolyORB`.
By carefully selecting them, you can increase the
throughput of your application.

We review some parameters that can impact performance.

* **Build options**:

  * For production use, you should not build PolyORB with debug
    activated.

* **Tasking policies**:

  * You should carefully select the tasking policy to reduce
    dynamic ressource allocation (tasks, entry points,
    etc.). :ref:`Tasking_model_in_PolyORB`.

* **Transport parameters**:

  * Setting `tcp.nodelay` to false will disable Nagle buffering.

* **GIOP parameters**:

  * Setting
    `polyorb.protocols.iiop.giop.1.X.locate_then_request`, where
    `X` is the GIOP version in use, to false will disable
    `Locate_Message`, reducing the number of requests exchanged,

  * Increasing
    `polyorb.protocols.iiop.giop.1.X.max_message_size`, where
    `X` is the GIOP version in use, will reduce GIOP fragmentation,
    reducing middleware processing.

@appendix Conformance to standards

.. _CORBA_standards_conformance:

CORBA standards conformance
===========================

The OMG defines a CORBA-compliant ORB as an implementation of the CORBA
specifications that supports CORBA Core and one mapping of CORBA's IDL.

Here is a summary of PolyORB's conformance issues with the latest CORBA
specifications (revision 3.0, formal/02-06-01).

.. _CORBA_IDL-to-Ada_mapping:

CORBA IDL-to-Ada mapping
------------------------

.. index:: CORBA IDL-to-Ada mapping

PolyORB supports the IDL-to-Ada specification :cite:`corba-ada-mapping1.2:2001`, with the
following limitations in both the CORBA API and the IDL-to-Ada compiler
`idlac`:

* no support for abstract interfaces, object-by-value, context data;
* no support for CORBA Components;
* implemented API may present some divergences with current mapping.

*Note: generated code is constrained by the limitations of the Ada compiler used. Please refer to its documentation for more information.*

Conforming to documentation requirements from section 4.11 of the
IDL-to-Ada specification :cite:`corba-ada-mapping1.2:2001`, note that PolyORB's
implementation of CORBA is *tasking-safe*. The use of the CORBA
personality on typical GNAT runtimes is *task-blocking*, unless
specified in platform notes.

.. _CORBA_Core:

CORBA Core
----------

This set encompasses chapters 1-11. Chapters 3 to 11 are normative.

* Chapter 3 describes OMG IDL syntax and semantics. See :ref:`CORBA_IDL-to-Ada_mapping` for a description of non-implemented features;

* Chapter 4 describes the ORB Interface.

  PolyORB partially supports this chapter.

* Chapter 5 describes Value Type Semantics.

  PolyORB does not support this chapter.

* Chapter 6 describes Abstract Interface Semantics.

  PolyORB does not support this chapter.

* Chapter 7 describes Dynamic Invocation Interface (DII)

  PolyORB supports only the following methods: `Create_Request`,
  `Invoke` and `Delete`.

* Chapter 8 describes Dynamic Skeleton Interface (DSI)

  PolyORB partially supports this chapter: this interface is fully
  implemented except for context data.

* Chapter 9 describes Dynamic Management of Any Values

  PolyORB partially supports this chapter: this interface is fully
  implemented except for object references and value types.

* Chapter 10 describes The Interface Repository

  PolyORB supports this chapter, except for the `ExtValueDef`
  interface, and all CORBA CCM related interfaces.

* Chapter 11 describes The Portable Object Adapter

  PolyORB supports this chapter with the following limitations:

  * the `USE_SERVANT_MANAGER` policy is partially supported: the
    `ServantLocator` object is not implemented;
  * support for `SINGLE_THREAD` policy is incomplete, reentrant
    calls may not work;
  * `Wait_For_Completion` and `Etherealize_Objects` are
    not taken into account in `PortableServer.POAManager`;
  * the `PortableServer.POAManagerFactory` API is not implemented.


.. _CORBA_Interoperability:

CORBA Interoperability
----------------------

This set encompasses chapters 12-16.


* See :ref:`CORBA-GIOP_standards_conformance` for more information
  on this point.


.. _CORBA_Interworking:

CORBA Interworking
------------------

This set encompasses chapters 17-21.


* Chapters 17 to 20 describe interoperability with Microsoft's
  COM/DCOM.

  PolyORB provides no support for these chapters.

* Chapter 21 describes `PortableInterceptor`.

  PolyORB provides partial support for this chapter.


.. _CORBA_Quality_Of_Service:

CORBA Quality Of Service
------------------------

This set encompasses chapters 22-24.


* Chapter 22 describes CORBA Messaging

* Chapter 23 describes Fault Tolerant CORBA

* Chapter 24 describes Secure Interoperability.


PolyORB provides no support for these chapters.

.. _CORBA_COS_Services:

CORBA COS Services
------------------

.. index:: CORBA, COS Services

COS Services are specifications of high level services that are
optional extensions to the CORBA specification. They provide helper
packages to build distributed applications.  PolyORB implements the
following COS Services:

* COS Event and TypedEvent;
* COS Naming;
* COS Notification;
* COS Time;

.. _CORBA_Specialized_services:

CORBA Specialized services
--------------------------

.. index:: CORBA, Specialized services

PolyORB supports the following specialized services:

* Unreliable Multicast (MIOP), proposed 1.0 specification :cite:`miop`.
  .. index:: MIOP

* RT-CORBA extensions, see :ref:`RT-CORBA` for more information
  on this point.

* CORBA security extensions, see :cite:`csiv2` for more information
  on this point.


.. _RT-CORBA_standards_conformance:

RT-CORBA standards conformance
==============================

RT-CORBA specifications rely on the CORBA application
personality; the same issues and implementation notes apply.

In addition, here is a list of issues with the implementation of
RT-CORBA static :cite:`rt-corba1.1:2002` and dynamic scheduling :cite:`rt-corba2.0:2003`
specifications.

* RT-CORBA static and dynamic scheduling (Chapter 2)

  Chapter 2 is common to these two specifications. It describes
  key mechanisms of RT-CORBA that are common to both specifications.

  PolyORB partially implements this chapter from section 2.1 up to
  section 2.10. PolyORB does not provide support for all
  connection-related policies.

  See implementation notes in the different package specifications for
  more details.

* RT-CORBA static scheduling (Chapter 3)

  PolyORB supports this chapter.

* RT-CORBA dynamic scheduling (Chapter 3)

  PolyORB does not support this chapter.


.. _CSIv2_standards_conformance:

CSIv2 standards conformance
===========================

PolyORB supports IIOP/SSL.

.. _CORBA/GIOP_standards_conformance:

CORBA/GIOP standards conformance
================================

GIOP supports part of the CORBA Interoperability specification, from
chapters 12 to 16 of CORBA specifications.

Chapter 12 defines general concepts about ORB interoperability. It
defines an *interoperbility-compliant ORB* as an ORB that supports:

* API that supports the construction of request-level inter-ORB
  bridges, Dynamic Invocation Interface, Dynamic Skeleton Interface and
  the object identity operations described in the Interface
  Repository. See :ref:`CORBA_standards_conformance` for more details.

* IIOP protocol as defined in chapter 15.

Support for other components is optional.


* Chapter 13 describes the ORB Interoperability Architecture.

  PolyORB fully supports this chapter.

* Chapter 14 describes how to build Inter-ORB Bridges.

  PolyORB fully supports this chapter.

* Chapter 15 describes the General Inter-ORB Protocol (GIOP).

  PolyORB supports GIOP version 1.0 to 1.2, the CDR representation scheme.
  Support for IOR and `corbaloc` addressing mechanisms is supported
  in the CORBA personality, see :ref:`CORBA` for more details.

  PolyORB does not support the optional IIOP IOR Profile Components,
  Bi-directional GIOP. PolyORB also does not support fragmentation
  in GIOP 1.1.

* Chapter 16 describes the DCE ESIOP protocol.

  PolyORB does not support this optional chapter.


SOAP standards conformance
==========================

*The documentation of the SOAP standards conformance of PolyORB will appear in a future revision of PolyORB.*

