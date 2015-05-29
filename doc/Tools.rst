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
