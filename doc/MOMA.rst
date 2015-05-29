.. _MOMA:

****
MOMA
****

.. index:: MOMA, Message Oriented Middleware for Ada

.. _What_you_should_know_before_Reading_this_section:

What you should know before Reading this section
================================================

This section assumes that the reader is familiar with the JMS
specifications described in :cite:`jms`. MOMA is a thick adaptation
of the JMS specification to the Ada programming language, preserving
most of the JMS concepts.

.. _Installing_MOMA_application_personality:

Installing MOMA application personality
=======================================

Ensure PolyORB has been configured and then compiled with the MOMA
application personality. See :ref:`Building_an_application_with_PolyORB`
for more details on how to check installed personalities.

To build the MOMA application personality, :ref:`Installation`.

.. _Package_hierarchy:

Package hierarchy
=================

Packages installed in :file:`$INSTALL_DIR/include/polyorb/moma` hold
the MOMA API. MOMA is built around two distinct sets of packages:

* :file:`MOMA.*` hold the public MOMA library, all the constructs
  the user may use.

* :file:`POLYORB.MOMA_P.*` hold the private MOMA library, these
  packages shall not be used when building your application.


