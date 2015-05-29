.. _SOAP:

****
SOAP
****

.. index:: SOAP

.. _Installing_SOAP_protocol_personality:

Installing SOAP protocol personality
====================================

Ensure PolyORB has been configured and then compiled with the SOAP
protocol personality. See :ref:`Building_an_application_with_PolyORB`
for more details on how to check installed personalities.

To enable configuration of the SOAP application personality,
:ref:`Installation`.

.. _Configuring_the_SOAP_personality:

Configuring the SOAP personality
================================

The SOAP personality is configured using a configuration
file. See :ref:`Using_a_configuration_file` for more details.

Here is a summary of available parameters for each instance of SOAP.


::

  ###############################################################################
  # SOAP parameters
  #

  [soap]

  ###############################################################
  # SOAP Global Settings

  # Preference level for SOAP
  #polyorb.binding_data.soap.preference=0

  # SOAP's default address
  #polyorb.protocols.soap.default_addr=127.0.0.1

  # SOAP's default port
  #polyorb.protocols.soap.default_port=8080
  

