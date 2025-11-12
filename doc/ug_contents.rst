PolyORB User's Guide
====================

Robert Duff, Jérôme Hugues, Laurent Pautet,
Thomas Quinot, Samuel Tardieu

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.1 or
any later version published by the Free Software Foundation; with the
Invariant Sections being 'GNU Free Documentation License', with the
Front-Cover Texts being 'PolyORB User's Guide', and with no
Back-Cover Texts.  A copy of the license is included in the section
entitled 'GNU Free Documentation License'.

.. _End_of_Life_Notification:

End of Life Notification
~~~~~~~~~~~~~~~~~~~~~~~~~

PolyORB is a deprecated product. It will be baselined with the GNAT Pro
release 28. After this release, there will be no new versions of this
product. Contact AdaCore support to get recommendations for replacements.

.. toctree::
   :numbered:
   :maxdepth: 3

   Introduction_to_PolyORB
   Installation
   Overview_of_PolyORB_personalities
   Building_an_application_with_PolyORB
   Tasking_model_in_PolyORB
   CORBA
   RT-CORBA
   Ada_Distributed_Systems_Annex_(DSA)
   MOMA
   GIOP
   SOAP
   Tools
   Performance
   Conformance
   References

.. _About_This_Guide:

About This Guide
~~~~~~~~~~~~~~~~

This guide describes the use of PolyORB, a middleware that enables
the construction of distributed Ada applications.

It describes the features of the middleware and related APIs and
tools, and details how to use them to build Ada applications.

.. _What_This_Guide_Contains:

What This Guide Contains
~~~~~~~~~~~~~~~~~~~~~~~~

This guide contains the following chapters:


* :ref:`Introduction_to_PolyORB` provides a brief description of
  middleware and PolyORB's architecture.

* :ref:`Installation` details how to configure and install PolyORB
  on your system.

* :ref:`Overview_of_PolyORB_personalities` enumerates the different
  personalities, or distribution mechanisms, PolyORB provides.

* :ref:`Building_an_application_with_PolyORB` presents the
  different steps to build a distributed application using PolyORB.

* :ref:`Tasking_model_in_PolyORB` details the use of tasking
  constructs within PolyORB.

* :ref:`CORBA` describes PolyORB's implementation of OMG's CORBA.

* :ref:`RT-CORBA` describes PolyORB's implementation of RT-CORBA,
  the real-time extensions of OMG's CORBA.

* :ref:`Ada_Distributed_Systems_Annex_(DSA)` describes PolyORB's implementation of the Ada
  Distributed Systems Annex.

* :ref:`MOMA` describes PolyORB's implementation of MOMA, the
  Message Oriented Middleware for Ada.

* :ref:`GIOP` describes PolyORB's implementation of GIOP, the
  protocol defined as part of CORBA.

* :ref:`SOAP` describes PolyORB's implementation of SOAP.

* :ref:`Tools` describes PolyORB's tools.

* :ref:`Performance` discusses possible configuration adjustments
  to optimize PolyORB's run time performance.

* :ref:`Conformance` discusses the conformance of the
  PolyORB's personalities to the CORBA and SOAP standards.

* :ref:`References` provides a list of useful references to
  complete this documentation.

.. _Conventions:

Conventions
~~~~~~~~~~~

.. index:: Conventions

.. index:: Typographical conventions

Following are examples of the typographical and graphic conventions used
in this guide:

* 
  `Functions`, `utility program names`, `standard names`,
  and `classes`.

* 
  `Option flags`

* 
  :file:`File Names`, :file:`button names`, and :file:`field names`.

* 
  `Variables`.

* 
  *Emphasis*.

* 
  [optional information or parameters]

* 
  Examples are described by text

  ::

    and then shown this way.
    

Commands that are entered by the user are preceded in this manual by
the characters `'`$`'` (dollar sign followed by space). If
your system uses this sequence as a prompt, then the commands will
appear exactly as you see them in the manual. If your system uses some
other prompt, then the command will appear with the `$` replaced
by whatever prompt you are using.

Full file names are shown with the '`/`' character
as the directory separator; e.g., :file:`parent-dir/subdir/myfile.adb`.
If you are using GNAT on a Windows platform, please note that
the '`\\`' character should be used instead.

