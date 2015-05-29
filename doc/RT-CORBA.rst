.. _RT-CORBA:

********
RT-CORBA
********

.. index:: RT-CORBA

.. _What_you_should_know_before_Reading_this_section:

What you should know before Reading this section
================================================

This section assumes that the reader is familiar with the Real-Time
CORBA specifications described in :cite:`rt-corba1.1:2002` and :cite:`rt-corba2.0:2003`.

.. _Installing_RT-CORBA:

Installing RT-CORBA
===================

The RT-CORBA library is installed as part of the installation of the
CORBA personality. Note that you may have to select specific run-time
options to enable full compliance with RT-CORBA specifications and
ensure real time behavior.

.. _Configuring_RT-CORBA:

Configuring RT-CORBA
====================

This section details how to configure your application to use the
RT-CORBA library.

.. _`PolyORB.RTCORBA_P.Setup`:

`PolyORB.RTCORBA_P.Setup`
-------------------------

.. index:: `PolyORB.RTCORBA_P.Setup`

The RT-CORBA specifications mandate that the implementation provide a
mechanism to set up some of its internals.

The package `PolyORB.RTCORBA_P.Setup` provides an API to set up
the `PriorityMapping` and `PriorityTransform` objects.

.. literalinclude:: polyorb-rtcorba_p-setup.ads
   :language: ada

.. _`RTCORBA.PriorityMapping`:

`RTCORBA.PriorityMapping`
=========================

.. index:: `RTCORBA.PriorityMapping`

PolyORB provides different implementations of this specification:


* `RTCORBA.PriorityMapping.Direct` maps CORBA priorities
  directly to native priorities. If the CORBA priority is not in
  `System.Priority'Range`, then the mapping is not possible.

* `RTCORBA.PriorityMapping.Linear` maps each individual
  native priority to a contiguous range of CORBA priorities, so that the
  complete CORBA priority range is used up for the mapping. See
  :file:`rtcorba-prioritymapping-linear.adb` for more details.


.. _RTCosScheduling_Service:

RTCosScheduling Service
=======================

.. index:: RTCosScheduling Service

.. _Overview:

Overview
--------

PolyORB provides an implementation of the RTCosScheduling service
defined in :cite:`rt-corba1.1:2002`.

PolyORB uses some permissions stated in the specifications to allow for
easy configuration of `ClientScheduler` and
`ServerScheduler`, defined in the following sections.

Additional information on the use of the API may be found in the
RTCosScheduling example in
:file:`examples/corba/rtcorba/rtcosscheduling`.

.. _`RTCosScheduling::ClientScheduler`:

`RTCosScheduling::ClientScheduler`
----------------------------------

Client side *activities* are defined in a configuration file that can
be loaded using
:file:`RTCosScheduling.ClientScheduler.Impl.Load_Configuration_File`

On the client side, the user can set up


* current task priority, using registered `PriorityMapping` object.


This file has the following syntax, derived from PolyORB configuration
file syntax:


::

  # Name of the activity
  [activity activity1]

  # Activity priority, in RTCORBA.Priority'Range
  priority=10000

  

In this example, activity `activity1` is defined with priority
`10'000`.

.. _`RTCosScheduling::ServerScheduler`:

`RTCosScheduling::ServerScheduler`
----------------------------------

Server side *POAs* and *objects* are defined in a configuration
file that can be loaded using
:file:`RTCosScheduling.ClientScheduler.Impl.Load_Configuration_File`

On the server side, the user can set up


* object priority, using registered `PriorityMapping` object.

* all RT-CORBA-specific POA configuration parameters.


This file has the following syntax, derived from PolyORB configuration
file syntax:


::

  # Name of the object
  [object object1]

  # Object priority, in RTCORBA.Priority'Range
  priority=10000

  

In this example, object `object1` is defined with priority
`10'000`.


::

  # Name of the POA
  [poa poa1]

  # PriorityModelPolicy for POA
  priority_model=CLIENT_PROPAGATED
  default_priority=0 # not meaningful for CLIENT_PROPAGATED

  # Threadpools attached to POA
  threadpool_id=1

  # Name of the POA
  [poa poa2]

  # PriorityModelPolicy for POA
  priority_model=SERVER_DECLARED
  default_priority=40

  # Threadpools attached to POA
  threadpool_id=2

  # Name of the POA
  [poa poa3]

  # POA with no defined policies

  

In this example, Two POAs are defined: POA `poa1` will use the
`CLIENT_PROPAGATED` PriorityModel Policy, default value is not
meaningful for this configuration, `poa1` will use the Threadpool
#1; POA `poa2` will use the `SERVER_DECLARED` PriorityModel
Policy, default server priority is 40, `poa2` will use the
Threadpool #2. Note that both policies are optional and can be omitted.

