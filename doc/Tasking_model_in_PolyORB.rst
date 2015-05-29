.. _Tasking_model_in_PolyORB:

************************
Tasking model in PolyORB
************************

.. index:: Tasking model

.. _PolyORB_Tasking_runtimes:

PolyORB Tasking runtimes
========================

.. index:: Tasking runtime

PolyORB may use any of three different tasking runtimes to manage and
synchronize tasks, if any. Tasking runtime capabilities are defined
in the Ada Reference Manual :cite:`ada-rm`.

The choice of a specific tasking runtime is a compile-time parameter,
:ref:`Tasking_runtimes` for more details on their configuration.

Full tasking runtime
--------------------

Full tasking runtime refers to the configuration in which there are
dependencies on the tasking constructs defined in chapter 9 of
:cite:`ada-rm`. It makes use of all capabilities defined in this section
to manage and synchronize tasks.

In this configuration, a PolyORB application must be compiled and
linked with a tasking-capable Ada runtime.

No tasking runtime
------------------

No tasking runtime refers to the configuration in which there is no
dependency on tasking constructs. Thus, no tasking is required.

In this configuration, a PolyORB application may be compiled and
linked with a tasking-capable Ada runtime or a no-tasking Ada runtime.

Ravenscar tasking runtime
-------------------------

.. index:: Ravenscar

Ravenscar tasking runtime refers to the configuration in which tasking
constructs are compliant with the *Ravenscar tasking restricted
profile*.

In this configuration, a PolyORB application may be compiled and
linked with a tasking-capable Ada runtime or a Ravenscar Ada runtime.

To configure tasking constructs used by PolyORB, one must instantiate
the `PolyORB.Setup.Tasking.Ravenscar` generic package shown below
to set up tasks and protected objects used by PolyORB core.

.. literalinclude:: ../../src/polyorb-setup-tasking-ravenscar.ads
   :language: ada

.. _PolyORB_ORB_Tasking_policies:

PolyORB ORB Tasking policies
============================

PolyORB ORB Tasking policies control the creation of tasks to
process all middleware internal jobs, e.g. request processing, I/O
monitoring.

*Note: there is a dependency between ORB Tasking policies, and the runtime used, as detailed below.*

No Tasking
----------

Under the No Tasking ORB policy, no tasks are created within the
middleware instance: it uses the environment task to process all
jobs. Note that this policy is not thread safe and is compatible with
the No tasking runtime only.

Thread Pool
-----------

Under the Thread Pool ORB policy, the middleware creates a pool of
threads during initialization of PolyORB. This pool processes all
jobs. The number of tasks in the thread pool can be configured by three
parameters in the `[tasking]` configuration section.


* `min_spare_threads` indicates the number of tasks
  created at startup.

* `max_spare_threads` is a ceiling. When a remote subprogram
  call is completed, its anonymous task is deallocated if the number of
  unused tasks already in the pool is greater than the ceiling. If not,
  then the task is queued in the pool.

* `max_threads` indicates the maximum number of tasks in the pool.


:ref:`PolyORB_Tasking_configuration`, for more information on how to
configure the number of tasks in the thread pool.

Thread Per Session
------------------

Under the Thread Per Session ORB policy, the middleware creates one
task when a new session (one active connection) is opened. The task
terminates when the session is closed.

Thread Per Request
------------------

Under the Thread Per Request ORB policy, the middleware creates one
task per incoming request. The task terminates when the request is
completed.

.. _PolyORB_Tasking_configuration:

PolyORB Tasking configuration
=============================

The following parameters allow the user to set up some of the tasking
parameters.


::

  ###############################################################################
  # Parameters for tasking
  #

  [tasking]
  # Default storage size for all threads spawned by PolyORB
  #storage_size=262144

  # Number of threads by Thread Pool tasking policy
  #min_spare_threads=4
  #max_spare_threads=4
  #max_threads=4

  

.. _PolyORB_ORB_Controller_policies:

PolyORB ORB Controller policies
===============================

The PolyORB ORB Controller policies are responsible for the management
of the global state of the middleware: they assign middleware internal
jobs, or I/Os monitoring to middleware tasks.

ORB Controller policies grant access to middleware internals and
affect one action for each middleware task. They ensure that all
tasks work concurrently in a thread-safe manner.

No Tasking
----------

The No Tasking ORB Controller policy is dedicated to no-tasking
middleware configurations; the middleware task executes the following
loop: process internal jobs, then monitor I/Os.

Workers
-------

The Workers ORB Controller policy is a simple controller policy: all
tasks are equal, they may alternatively and randomly process requests
or wait for I/O sources.

*Note: this is the default configuration provided by PolyORB sample setup files, :ref:`Sample_files*.`

Half Sync/Half Async
--------------------

The Half Sync/Half Async ORB Controller policy implements the ``Half
Sync/Half Async'' design pattern: it discriminates between one thread
dedicated to I/O monitoring that queue middleware jobs; another pool
of threads dequeue jobs and process them.

*Note: this pattern is well-suited to process computation-intensive requests.*

Leader/Followers
----------------

The Leader/Followers ORB Controller policy implements the
'Leader/Followers ' design pattern: multiple tasks take turns to
monitor I/O sources and then process requests that occur on the event
sources.

*Note: this pattern is adapted to process a lot of light requests.*

.. _PolyORB_ORB_Controller_configuration:

PolyORB ORB Controller configuration
====================================

The following parameters allow the user to set up parameters for ORB
Controllers.


::

  ###############################################################################
  # Parameters for ORB Controllers
  #

  [orb_controller]
  # Interval between two polling actions on one monitor
  #polyorb.orb_controller.polling_interval=0

  # Timeout when polling on one monitor
  #polyorb.orb_controller.polling_timeout=0

  

