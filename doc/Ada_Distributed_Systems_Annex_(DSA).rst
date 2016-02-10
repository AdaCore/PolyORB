.. _Ada_Distributed_Systems_Annex_(DSA):

***********************************
Ada Distributed Systems Annex (DSA)
***********************************

.. index:: DSA, Distributed Systems Annex

.. _Introduction_to_the_Ada_DSA:

Introduction to the Ada DSA
===========================

A critical feature of the Distributed Systems Annex (DSA) is that it allows the
user to develop his application the same way whether this application is
going to be executed as several programs on a distributed system, or as a
single program on a non-distributed system. The DSA has been designed
to minimize the source changes needed to convert
an ordinary non-distributed program into a distributed program.

The simplest way to start with DSA is to develop the
application on a non-distributed system. Of course, the design of the
application should take into account the fact that some units are going
to be accessed remotely. In order to write a distributed Ada program,
it is necessary for the user to label by means of categorization pragmas
some of library level compilation units of the application program. The units
that require
categorization are typically those that are called remotely, and those
that provide the types used in remote invocations.

In order to ensure that distributed execution is possible, these units are
restricted to contain only a limited set of Ada constructs.
For instance, if the distributed system has no shared memory,
shared variables must be forbidden. To specify the nature of these restrictions,
the DSA provides several categorization pragmas, each of which excludes some
language constructs from the categorized package.

Of course, the user can develop the non-distributed application with his
usual software engineering environment. It is critical to note that the
user needs no specialized tools to develop his/her distributed
application. For instance, he can debug his application with the usual
debugger. Note that a non-distributed program is not to be confused with
a distributed application composed of only one program. The latter is
built with the help of the configuration tool and includes the
communication library.

Once the non-distributed version of the program is complete, it has to
be configured into separate partitions. This step is surprisingly
simple, compared to that of developing the application itself. The
configuration step consists of mapping sets of compilation units into
individual partitions, and specifying the mapping between partitions and
nodes in the computer network. This mapping is specified and managed by
means of a gnatdist configuration.

The distributed version of the user application should work as is, but
even when a program can be built both as a non-distributed or a
distributed program using the same source code, there may still be
differences in program execution between the distributed and
non-distributed versions. These differences are discussed in subsequent
sections (see :ref:`Pragma_Asynchronous` and :ref:`Pragma_All_Calls_Remote`).

Developing a non-distributed application in order to distribute it
later is the natural approach for a novice. Of course, it is not always
possible to write a distributed application as a non-distributed
application. For instance, a client/server application does not belong
to this category because several instances of the client can be active
at the same time. It is very easy to develop such an application using
PolyORB; we shall describe how to do this in the following sections.

.. _Architecture_of_a_Distributed_Ada_Application:

Architecture of a Distributed Ada Application
---------------------------------------------

A distributed system is an interconnection of one or more processing
nodes and zero or more storage nodes. A distributed program comprises
one or more partitions. A partition is an aggregate of library
units. Partitions communicate through shared data or RPCs. A passive
partition has no thread of control. Only a passive partition can be
configured on a storage node. An active partition has zero or more
threads of control and has to be configured on a processing node.

The library unit is the core component of a distributed Ada
application. The user can explicitly assign library units to a
partition. Partitioning is a post-compilation process. The user
identifies interface packages at compile-time. These packages are
categorized using pragmas. Each of these pragmas supports the use of one of
the following classical paradigms:

* Remote subprograms:
  For the programmer, a remote subprogram call is similar to a regular
  subprogram call. Run-time binding using access-to-subprogram types can
  also be used with remote subprograms. These remote subprograms are
  declared in library units categorized as remote call interface
  (RCI).

* Distributed objects:
  Special-purpose access types can designate remote
  objects. When a primitive dispatching operation is invoked on an object
  designated by such a remote access, a remote call is performed transparently
  on the partition on which the object resides. The types of these distributed
  objects are declared in library units categorized as remote types (RT).

* Shared objects:
  Global data can be shared among active partitions, providing a
  repository similar to shared memory, a shared file system or a
  database. Entryless protected objects allow safe concurrent access and update
  of shared objects. This feature is orthogonal to the notion of distributed
  objects, which are only accessed through exported services. These shared
  objects are declared in library units categorized as shared passive (SP).


The remotely-called subprograms declared in a library unit categorized
as remote call interface (RCI) or remote types (RT) may be either
statically or dynamically bound. The partition on which a statically
bound remote subprogram is executed can be determined before the
call. This is a static remote subprogram call. In contrast, a remote method or
a dereference of an access to remote subprogram are dynamically bound remote
calls, because the partition on which the remote subprogram is executed
is determined at runtime, by the actuals of the call.

In the following example, Data_1 and Data_2 are shared passive (SP)
library units. Data_1 is configured on a passive partition mapped on a
storage node. Partition_1 and Partition_2 are active partitions. Note
that under some circumstances, a partition, for instance Partition_2,
can be duplicated. To be duplicated, Unit_2 and Unit_3 which are
configured on Partition_2 have to provide only dynamically bound remote
subprograms. Otherwise, a partition calling a remote subprogram on
Unit_2 would not be able to statically determine where to perform the
remote call between the two instances of Unit_2.

.. image:: xe-arch_fig.*

.. _Categorization_Pragmas:

Categorization Pragmas
----------------------

Library units can be categorized according to the role they play in a
distributed program. A categorization pragma is a library unit pragma
that restricts the kinds of declarations that can appear in a library unit
and possibly in its child units, as well as the legal semantic dependences
that the categorized unit can have. There are several categorization
pragmas:

* Remote_Call_Interface
* Remote_Types
* Shared_Passive
* Pure

The following paragraphs do not present the detailed semantics of these
pragmas (formal details will be found in the Ada Reference Manual).
Their purpose is to give the reader an intuitive overview of
the purpose of these pragmas. If a library unit is not categorized, this
unit is called a normal unit and plays no special role in the distributed
application. Such a unit is duplicated on any partition in which it is
mentioned.

A parenthetical remark: to avoid the need for specific run-time
libraries for the DSA, the notion of remote rendezvous does not exist
in Ada: tasks cannot be invoked directly from one partition
to another. Therefore, declarations of task types and general protected types
with entries are not allowed in categorized Ada library units.

.. _Pragma_Declared_Pure:

Pragma Declared Pure
--------------------

This pragma is not specific to the Distributed Systems Annex. A pure
package can appear in the context of any package, categorized or not.
A pure package is a preelaborable package that
does not contain variable data.
It is particularly useful to define types, constants and
subprograms shared by several categorized packages. In contrast, normal
packages cannot appear in the context of categorized package declarations.
Because a pure package has no state, it can be duplicated on several partitions.

.. _Pragma_Remote_Call_Interface:

Pragma Remote_Call_Interface
----------------------------


.. _Overview_of_Pragma_Remote_Call_Interface:

Overview of Pragma Remote_Call_Interface
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Library units categorized with this pragma declare subprograms that can be
called and executed remotely. An RCI unit acts as a server for remote calls.
There is no memory space shared between server and clients.
A subprogram call that invokes one such
subprogram is a classical RPC operation; it is a
statically bound operation, because the compiler can determine the identity
of the subprogram being called.

Dynamically bound calls are provided through two mechanisms:


* 
  The dereference of an access-to-subprogram value, i.e. a value whose type is
  a remote access-to-subprogram (RAS).

* 
  A dispatching call whose controlling argument is
  an access-to-class-wide operand.
  The formal is a remote access-to-class-wide (RACW)
  type. These remote access types can be declared in RCI
  packages as well.


A remote access type (RAS or RACW) can be viewed as a fat pointer, that is
to say a structure with a remote address and a local address (like a URL:
`<protocol>://<remote-machine>/<local-directory>`). The
remote address must denote the host of the partition on which the entity has
been created; the local address describes the local memory
address within the host.

It is very unlikely that RCI units can be duplicated in the distributed
system. An implementation may allow separate copies of a RCI unit as
long as it ensures that the copies present a consistent state to all clients.
In the general case, preserving consistency is very costly. For this reason, the
implementation may require a RCI unit to be unique in the distributed
system.

.. _Regular_Remote_Subprograms_(RCI):

Regular Remote Subprograms (RCI)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the following example, a RCIBank offers several remote services:
Balance, Transfer, Deposit and Withdraw. On the caller side, the bank
client uses the stub files of unit RCIBank. On the receiver side,
the bank receiver uses the skeleton files of unit RCIBank including
the body of this package.

.. literalinclude:: types.ads
   :language: ada
.. literalinclude:: rcibank.ads
   :language: ada
.. literalinclude:: rciclient.adb
   :language: ada

.. _Remote_Access_to_Subprograms_(RAS):

Remote Access to Subprograms (RAS)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the following example, several mirroring banks offer their services
through the same database. Each bank registers a reference to each of
its services with a central bank. A client of the central bank requests
a service from one of the mirroring banks. To satisfy requests, the RCI
unit RASBank defines Balance_Type, a remote access to subprogram. (Recall
that an access type declared in a remote unit has to be either remote
access to subprogram or remote access to class wide type).

Note that to obtain a remote access to subprogram, the subprogram that
delivers the remote access must be remote itself. Therefore, MirrorBank
is a RCI library unit.

.. literalinclude:: rasbank.ads
   :language: ada

In the code below, a mirroring bank registers its services to the
central bank.

.. literalinclude:: mirrorbank.ads
   :language: ada
.. literalinclude:: mirrorbank.adb
   :language: ada

In the code below, a central bank client asks for a mirroring bank and
calls the Balance service of this bank by dereferencing a remote access
type.

.. literalinclude:: bankclient.adb
   :language: ada

.. _Remote_Access_to_Class_Wide_Types_(RACW):

Remote Access to Class Wide Types (RACW)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A bank client is now connected to a bank through a terminal. The bank
wants to notify a connected client, by means of a message on its
terminal, when another client transfers a given amount of money to its
account. In the following example, a terminal is designed as a
distributed object. Each bank client will register its terminal object
to the bank server for further use. In the code below, Term_Type is the
root type of the distributed terminal hierarchy.

.. literalinclude:: terminal.ads
   :language: ada

In the code below, the RCI unit RACWBank defines Term_Access, a remote
access to class wide type. Term_Access becomes a reference to a
distributed object. In the next section, we will see how to derive and
extend Term_Type, how to create a distributed object and how to use a
reference to it.

.. literalinclude:: racwbank.ads
   :language: ada

.. _Summary_of_Pragma_Remote_Call_Interface:

Summary of Pragma Remote_Call_Interface
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Remote call interface units:

* 
  Allow subprograms to be called and executed remotely

* 
  Allow statically bound remote calls (remote subprogram)

* 
  Allow dynamically bound remote calls (remote access types)

* 
  Forbid variables and non-remote access types

* 
  Prevent specification from depending on normal units


.. _Pragma_Remote_Types:

Pragma Remote_Types
-------------------


.. _Overview_of_Pragma_Remote_Types:

Overview of Pragma Remote_Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Unlike RCI units, library units categorized with this pragma can define
distributed objects and remote methods on them. Both RCI and RT units
can define a remote access type as described above (RACW). A subprogram
defined in a RT unit is not a remote subprogram. Unlike RCI units, a RT
unit can be duplicated on several partitions, in which case all its
entities are distinct. This unit is duplicated on each
partition in which it is defined.

.. _Distributed_Object:

Distributed Object
^^^^^^^^^^^^^^^^^^

If we want to implement the notification feature proposed in the
previous section, we have to derive Term_Type. Such a derivation is
possible in a remote types unit like NewTerminal (see below). Any object
of type New_Term_Type becomes a distributed object and any reference to
such an object becomes a fat pointer or a reference to a distributed
object (see Term_Access declaration in :ref:`Remote_Access_to_Class_Wide_Types_(RACW)`).

.. literalinclude:: newterminal.ads
   :language: ada

In the code below, a client registers his name and his terminal with
RACWBank. Therefore, when any payer transfers some money to him,
RACWBank is able to notify the client of the transfer of funds.

.. literalinclude:: term1client.adb
   :language: ada

In the code below, a second client, the payer, registers his terminal
to the bank and executes a transfer to the first client.

.. literalinclude:: term2client.adb
   :language: ada

In the code below, we describe the general design of Transfer. Classical
operations of Withdraw and Deposit are performed. Then, RACWBank
retrieves the terminal of the payee (if present) and invokes a
dispatching operation by dereferencing a distributed object Term. The
reference is examined at run-time, and the execution of this operation
takes place on the partition on which the distributed object resides.

.. literalinclude:: racwbank.adb
   :language: ada

.. _Transmitting_Dynamic_Structure:

Transmitting Dynamic Structure
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: stringarraystream.ads
   :language: ada

Non-remote access types cannot be declared in the public part of a
remote types unit. However, it is possible to define private non-remote
access types as long as the user provides its marshalling procedures,
that is to say the mechanism needed to place a value of the type into a
communication stream.  The code below describes how to transmit a linked
structure.

The package declaration provides a type definition of single-linked
lists of unbounded strings. An implementation of the marshalling
operations could be the following:

.. literalinclude:: stringarraystream.adb
   :language: ada

.. _Summary_of_Remote_Types_Units:

Summary of Remote Types Units
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Remote types units:

* 
  Support the definition of distributed objects

* 
  Allow dynamically bound remote calls (via remote access types)

* 
  Allow non-remote access types (with marshalling subprograms)

* 
  Cannot have a specification that depends on normal units


.. _Pragma_Shared_Passive:

Pragma Shared_Passive
---------------------


.. _Overview_of_Pragma_Shared_Passive:

Overview of Pragma Shared_Passive
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The entities declared in such a categorized library unit are intended to
be mapped on a virtual shared address space (file, memory,
database). When two partitions use such a library unit, they can
communicate by reading or writing the same variable in the shared
unit. This supports the conventional shared variables paradigm.
Entryless protected objects can be declared in these units, to provide
an atomic access to shared data, thus implementing a simple transaction
mechanism. When the address space is a file or a database, the user can
take advantage of the persistency features provided by these storage
nodes.

.. _Shared_and_Protected_Objects:

Shared and Protected Objects
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the code below, we define two kinds of shared
objects. External_Synchronization requires that the different partitions
updating this data synchronize to avoid conflicting operations on shared
objects. Internal_Synchronization provides a way to get an atomic
operation on shared objects. Note that only entryless protected types are
allowed in a shared passive unit; synchronization must be done with
protected procedures.

.. literalinclude:: sharedobjects.ads
   :language: ada

.. _Summary_of_Pragma_Shared_Passive:

Summary of Pragma Shared_Passive
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Shared passive units:

* 
  Allow direct access to data from different partitions

* 
  Provide support for shared (distributed) memory

* 
  Support memory protection by means of entryless protected objects

* 
  Prevent specification from depending on normal units


.. _More_About_Categorization_Pragmas:

More About Categorization Pragmas
---------------------------------


.. _Variables_and_Non-Remote_Access_Types:

Variables and Non-Remote Access Types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In RT or RCI package declarations, variable declarations are forbidden,
and non-remote access types are allowed as long as their marshalling
subprograms are explicitly provided (see :ref:`Transmitting_Dynamic_Structure`).

.. _RPC_Failures:

RPC Failures
^^^^^^^^^^^^

Calls are executed at most once: they are made exactly one time or they
fail with an exception. When a communication error occurs,
*System.RPC.Communication_Error* is raised.

.. _Exceptions:

Exceptions
^^^^^^^^^^

Any exception raised in a remote method or subprogram call is propagated
back to the caller. Exception semantics are preserved in the regular
Ada way.

.. literalinclude:: internal.ads
   :language: ada
.. literalinclude:: rempkg2.ads
   :language: ada
.. literalinclude:: rempkg1.ads
   :language: ada

Let us say that RemPkg2, Internal and RemExcMain packages are on the same
partition Partition_1 and that RemPkg1 is on partition Partition_2.

.. literalinclude:: rempkg2.adb
   :language: ada
.. literalinclude:: rempkg1.adb
   :language: ada
.. literalinclude:: remexcmain.adb
   :language: ada

When RemPkg1.Subprogram on Partition_1 raises Internal.Exc, this
exception is propagated back to Partition_2. As Internal.Exc is not
defined on Partition_2, it is not possible to catch this exception
without an exception handler **when others**. When this exception is
reraised in RemPkg1.Subprogram, it is propagated back to Partition_1. But
this time, Internal.Exc is visible and can be handled as we would in a
single-partition Ada program. Of course, the exception message is also
preserved.

.. _Pragma_Asynchronous:

Pragma Asynchronous
^^^^^^^^^^^^^^^^^^^

By default, a remote call is blocking: the caller waits until the remote
call is complete and the output stream is received.
Just like a normal (nonremote) call, the caller does not proceed
until the call returns.
By contrast, a
remote subprogram labeled with pragma Asynchronous allows statically
and dynamically bound remote calls to it to be executed
asynchronously. A call to an asynchronous procedure doesn't wait for the
completion of the remote call, and lets the caller continue its
execution. The remote procedure must have only **in** parameters, and
any exception raised during the execution of the remote procedure is
lost.

When pragma Asynchronous applies to a regular subprogram with **in**
parameters, any call to this subprogram will be executed
asynchronously. The following declaration of
AsynchronousRCI.Asynchronous gives an example.

.. literalinclude:: asynchronousrci.ads
   :language: ada
.. literalinclude:: asynchronousrt.ads
   :language: ada

A pragma Asynchronous may apply to a remote access-to-subprogram (RAS) type.
An asynchronous RAS can be both asynchronous and synchronous depending
on the designated subprogram. For instance, in the code below, remote
call (1) is asynchronous but remote call (2) is synchronous.

A pragma Asynchronous may apply to a RACW as well. In this case, the
invocation of **any** method with **in** parameters is *always* performed
asynchronously. Remote method invocation (3) is asynchronous but remote
method invocation (4) is synchronous.

.. literalinclude:: asynchronousmain.adb
   :language: ada

This feature supports the conventional message passing paradigm. The
user must be aware that this paradigm, and asynchronous remote calls in
particular, has several drawbacks:


* 
  It violates the normal semantics of calls; the caller proceeds without
  awaiting the return. The semantics are more similar to a 'remote goto'
  than a remote call

* 
  It prevents easy development and debugging in a non-distributed context

* 
  It can introduce race conditions


To illustrate the latter, let us take the following example:

.. literalinclude:: node2.ads
   :language: ada
.. literalinclude:: node2.adb
   :language: ada
.. literalinclude:: node1.ads
   :language: ada
.. literalinclude:: node1.adb
   :language: ada
.. literalinclude:: nondeterministic.adb
   :language: ada

Let us say that Main is configured on Partition_0, Node1 on Partition_1
and Node2 on Partition_2. If Node1.Send and Node2.Send procedures were
synchronous or if no latency was introduced during network
communication, we would have the following RPC order: Main remotely
calls Node1.Send which remotely calls Node2.Send which sets V to
1. Then, Main remotely calls Node2.Send and sets V to 2.

Now, let us assume that both Send procedures are asynchronous and that
the connection between Partition_1 and Partition_2 is very slow. The
following scenario can very well occur. Main remotely calls Node1.Send
and is unblocked. Immediately after this call, Main remotely calls
Node2.Send and sets V to 2. Once this is done, the remote call to
Node1.Send completes on Partition_1 and it remotely calls Node2.Send
which sets V to 1.

.. _Pragma_All_Calls_Remote:

Pragma All_Calls_Remote
^^^^^^^^^^^^^^^^^^^^^^^

A pragma All_Calls_Remote in a RCI unit forces remote procedure
calls to be routed through the communication subsystem even for a local
call. This eases the debugging of an application in a non-distributed
situation that is very close to the distributed one, because the communication
subsystem (including marshalling and unmarshalling procedures) can be
exercised on a single node.

In some circumstances, a non-distributed application can behave
differently from an application distributed on only one partition. This
can happen when both All_Calls_Remote and Asynchronous features are used
at the same time (see :ref:`Pragma_Asynchronous` for an example). Another
circumstance occurs when the marshalling operations raise an
exception. In the following example, when unit ACRRCI is a
All_@-Calls_@-Remote package, the program raises Program_Error. When
unit ACRRCI is no longer a All_Calls_Remote package, then the program
completes silently.

.. literalinclude:: acrrt.ads
   :language: ada
.. literalinclude:: acrrt.adb
   :language: ada
.. literalinclude:: acrrci.ads
   :language: ada
.. literalinclude:: acrrci.adb
   :language: ada
.. literalinclude:: acrmain.adb
   :language: ada

.. _Generic_Categorized_Units:

Generic Categorized Units
^^^^^^^^^^^^^^^^^^^^^^^^^

.. literalinclude:: genericrci.ads
   :language: ada
.. literalinclude:: rciinstantiation.ads
   :language: ada
.. literalinclude:: normalinstantiation.ads
   :language: ada

Generic units may be categorized.  Instances do not automatically
inherit the categorization of their generic units, but they can be
categorized explicitly. If they are not, instances are normal
compilation units. Like any other categorized unit, a categorized
instance must be at the library level, and the restrictions of
categorized units apply on instantiation (in particular on generic
formal parameters).

.. _Categorization_Unit_Dependencies:

Categorization Unit Dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each categorization pragma has very specific visibility rules. As a
general rule, RCI > RT > SP > Pure, where the comparison indicates
allowed semantic dependencies. This means that a Remote_Types package
can make visible in its specification only Remote_Types, Shared_Passive
and Pure units.

.. _Partition_Communication_Subsystem:

Partition Communication Subsystem
=================================


.. _Marshalling_and_Unmarshalling_Operations:

Marshalling and Unmarshalling Operations
----------------------------------------

The Partition Communication Subsystem (PCS) is the runtime library for
distributed features. It marshals and unmarshals client and server requests
into a data stream suitable for network transmission.

Parameter streams are normally read and written using four attributes:


* Write: write an element into a stream, valid only for constrained
  types

* Read: read a constrained element from a stream

* Output: same as Write, but write discriminants or array bounds as well
  if needed

* Input: same as Read, but read discriminants or bounds from
  the stream (the Input attribute denotes a function)

An Ada compiler provides default 'Read and 'Write operations. But it is
up to the implementation of the PCS to provide default 'Read and 'Write
to ensure proper operation between heterogeneous architectures (see
:ref:`Heterogeneous_System`).

The user can override these operations, except for predefined
types. Overriding with a custom version provides the user with a way
to debug its application (even outside of the Distributed Systems
Annex). On the other hand, remaining with the default implementation
allows the user to take advantage of optimized and portable representations
provided by the PCS.

.. literalinclude:: new_integers.ads
   :language: ada
.. literalinclude:: new_integers.adb
   :language: ada

The language forces the user to provide Read and Write operations for
non-remote access types. Transmitting an access value by dumping its
content into a stream makes no sense when the value is going to be transmitted
to another partition (with a different memory space). To transmit non-remote
access types see :ref:`Transmitting_Dynamic_Structure`.

.. _Incorrect_Remote_Dispatching:

Incorrect Remote Dispatching
----------------------------

When a remote subprogram takes a class wide argument, there is a risk of
using an object of a derived type that will not be clean enough to be
transmitted. For example, given a type called Root_Type, if a remote
procedure takes a Root_Type'Class as an argument, the user can call it
with an instance of Derived_Type that is Root_Type enriched with a field
of a task type. This will lead to a non-communicable type to be
transmitted between partitions.

To prevent this, paragraph E.4(18) of the Ada Reference Manual explains that
any actual type used as parameter for a remote call whose formal type is
a class wide type must be declared in the visible part of a Pure or
Remote_Types package. This property also holds for remote functions
returning class wide types. To summarize, the actual type used should
have been eligible for being declared where the root type has been
declared. If a `bad' object is given to a remote subprogram,
*Program_Error* will be raised at the point of the call.

.. _Partition_Ids:

Partition Ids
-------------

U'Partition_ID identifies the partition where the unit U has been
elaborated. For this purpose, the PCS provides an integer type
Partition_ID to uniquely designate a partition. Note that a Partition_ID
is represented as a universal integer, and has no meaning outside of the
PCS. The RM requires that two partitions of a distributed program have
different Partition_ID's at a given time. A Partition_ID may or may not
be assigned statically (at compile or link time). A Partition_ID may or
may not be related to the physical location of the partition.

Partition_ID's can be used to check whether a RCI package is configured locally.

.. literalinclude:: check_pid.adb
   :language: ada

.. _Concurrent_Remote_Calls:

Concurrent Remote Calls
-----------------------

It is not defined by the PCS specification whether one or more threads
of control should be available to process incoming messages and to wait
for their completion. But the PCS implementation is required to be
reentrant, thereby allowing concurrent calls on it to service concurrent
remote subprogram calls into the server partition.  This means that at
the implementation level the PCS manages a pool of helper tasks. This
(apart from performance) is invisible to the user.

.. _Consistency_and_Elaboration:

Consistency and Elaboration
---------------------------

A library unit is consistent if the same version of its declaration is
used in all units that reference it. This requirement applies as well to
a unit that is referenced in several partitions of a distributed
program.  If a shared passive or RCI library unit U is included in some
partition P, It is a bounded error to elaborate another partition P1 of
a distributed program that that depends on a different version of U.  As
a result of this error, Program_Error can be raised in one or both
partitions during elaboration.

U'Version yields a string that identifies the version of the unit
declaration and any unit declaration on which it depends. U'Version_Body
yields a string that identifies the version of the unit body. These
attributes are used by the PCS to verify the consistency of an
application.

After elaborating the library units, but prior to invoking the main
subprogram, the PCS checks the RCI unit versions, and then accept any
incoming RPC. To guarantee that it is safe to call receiving stubs, any
incoming RPC is kept pending until the partition completes its
elaboration.

.. _Abortion_and_Termination:

Abortion and Termination
------------------------

If a construct containing a remote call is aborted, the remote
subprogram call is cancelled. Whether the execution of the remote
subprogram is immediately aborted as a result of the cancellation is
implementation defined.

An active partition terminates when its environment task terminates. In
other terms, a partition cannot terminate before the Ada program itself
terminates. The standard termination mechanism applies, but can be
extended with extra rules (see :ref:`Partition_Attribute_Termination` for
examples).

.. _Most_Features_in_One_Example:

Most Features in One Example
============================

The example shown on the following figure highlights most of the
features of DSA. The system is based on a set of factories and workers
and a storage.  Each entity is a partition itself. A factory hires a
worker from a pool of workers (hire - 1) and assigns a job (query - 2)
to him. The worker performs the job and saves the result (reply - 3) in
a storage common to all the factories.  The worker notifies the factory
of the end of his job (notify - 4).

.. image:: full-ex_fig.*

When a worker has completed his job, the result must be saved in a
common storage. To do this, we define a protected area in SP package
Storage (see following code). An entryless protected object ensures
atomic access to this area.

.. literalinclude:: storage.ads
   :language: ada

Common is a Remote_Types package that defines most of the remote
services of the above system (see following code). First, we define a
way for the workers to signal the completion of his job. This callback
mechanism is implemented using RAS Notify.

.. literalinclude:: common.ads
   :language: ada

We define an abstract tagged type Worker which is intended to be the
root type of the whole distributed objects hierarchy. Assign allows a
factory to specify a job to a worker and a way for the worker to signal
its employer the completion of this job. Any_Worker is a remote access
to class wide type (RACW). In other words, it is a reference to a
distributed object of any derived type from Worker class. Note that the
two remote access types (Any_Worker and Notify) are declared as
asynchronous. Therefore, any override of Assign will be executed
asynchronously. To be asynchronous, an object of type Notify has to be a
reference to an asynchronous procedure.

NewWorker is derived from type Worker and Assign is overridden.

.. literalinclude:: newworkers.ads
   :language: ada

The following code shows how to derive a second generation of workers
NewNewWorker from the first generation NewWorker. As mentioned above,
this RT package can be duplicated on several partitions to produce
several types of workers and also several remote workers.

.. literalinclude:: newnewworkers.ads
   :language: ada

In the following code, we define a unique place where workers wait for
jobs. WorkerCity is a Remote_Call_Interface package with services to
hire and free workers. Unlike Remote_Types packages,
Remote_Call_Interface packages cannot be duplicated, and are assigned to
one specific partition.

.. literalinclude:: workercity.ads
   :language: ada

In order to use even more DSA features, Factory is defined as a generic
RCI package (see sample above). Any instantiation defines a new factory
(see sample above). To be RCI, this instantiation has to be categorized
once again.

.. literalinclude:: factory.ads
   :language: ada
.. literalinclude:: newfactory.ads
   :language: ada

.. _A_small_example_of_a_DSA_application:

A small example of a DSA application
====================================

In this section we will write a very simple client-server
application using PolyORB DSA. The server will provide a `Remote Call Interface` composed of a single `Echo_String` function that
will take a String and return it to the caller.

Here is the code for the server:

:file:`server.ads`:
.. literalinclude:: ../examples/dsa/echo/server.ads
   :language: ada

:file:`server.adb`:
.. literalinclude:: ../examples/dsa/echo/server.adb
   :language: ada

And here is the code for the client:

:file:`client.adb`:
.. literalinclude:: ../examples/dsa/echo/client.adb
   :language: ada

For more details about the Distributed Systems Annex,
see the Ada Reference Manual :cite:`ada-rm`.

.. _Building_a_DSA_application_with_PolyORB:

Building a DSA application with PolyORB
=======================================

This section describes how to build a complete distributed Ada application
using the PolyORB implementation of the DSA.

.. _Introduction_to_PolyORB/DSA:

Introduction to PolyORB/DSA
---------------------------

A distributed Ada application comprises a number of partitions
which can be executed concurrently on the same machine or, and this is
the interesting part, can be distributed on a network of machines.
The way in which partitions communicate is described in Annex E of the
Ada Reference Manual.

A partition is a set of compilation units that are linked together to
produce an executable binary. A distributed program comprises two or
more communicating partitions.

The Distributed Systems Annex (DSA) does not describe how a distributed
application should be configured. It is up to the user to define what
are the partitions in his program and on which machines they should be
executed.

The tool `po_gnatdist` and its configuration language
allows the user to partition his program and to specify the
machines on which the individual partitions are to execute.

`po_gnatdist` reads a configuration file (whose syntax is described in
section :ref:`The_Configuration_Language`) and builds several
executables, one for each partition. It also takes care of launching the
different partitions (default) with parameters that can be specific to
each partition.

.. _How_to_Configure_a_Distributed_Application:

How to Configure a Distributed Application
------------------------------------------



* 
  Write a non-distributed Ada application, to get familiar with the PolyORB
  environment. Use the categorization pragmas to specify the packages that
  can be called remotely.

* 
  When this non-distributed application is working, write a configuration
  file that maps the user categorized packages onto specific
  partitions. This concerns particularly remote call interface and remote
  types packages.  Specify the main procedure of the distributed
  application (see :ref:`Partition_Attribute_Main`).

* 
  Type `po_gnatdist *<configuration-file>*'.

* 
  Start the distributed application by invoking the start-up shell script
  or default Ada program (depending on the Starter option, see :ref:`Pragma_Starter`).


.. _Gnatdist_Command_Line_Options:

Gnatdist Command Line Options
-----------------------------


::


  po_gnatdist [switches] configuration-file [list-of-partitions]

  

The switches of `po_gnatdist` are, for the time being, exactly the same
as those of gnatmake, with the addition of `--PCS`, which allows the
user to override the default selection of distribution runtime library
(PCS). By default `po_gnatdist` outputs a configuration
report and the actions performed. The switch -n allows `po_gnatdist` to
skip the first stage of recompilation of the non-distributed
application.

The names of all configuration files must have the suffix
`.cfg`. There may be several configuration files for the same
distributed application, as the user may want to use different
distributed configurations depending on load and other characteristics
of the computing environment.

If a list of partitions is provided on the command line of the po_gnatdist
command, only these partitions will be built. In the following
configuration example, the user can type :


::

  po_gnatdist *<configuration> <partition_2> <partition_3>*

  

.. _The_Configuration_Language:

The Configuration Language
--------------------------

The configuration language is *Ada-like*. As the capabilities of PolyORB
will evolve, so will this configuration language. Most of the attributes
and pragmas can be overridden at run-time by command line arguments or
environment variables.

.. _Language_Keywords:

Language Keywords
^^^^^^^^^^^^^^^^^

All the Ada keywords are reserved keywords of the configuration
language. `po_gnatdist` generates full Ada code in order to build the
different executables. To avoid naming conflicts between Ada and the
configuration language, all the Ada keywords have been reserved even if
they are not used in the configuration language.

In addition, the following keywords are defined:

* *configuration* to encapsulate a configuration
* *partition* that is a predefined type to declare partitions

.. _Pragmas_and_Representation_Clauses:

Pragmas and Representation Clauses
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It is possible to modify the default behavior of the configuration via
a pragma definition.


::

  PRAGMA ::=
     **pragma** PRAGMA_NAME [(PRAGMA_ARGUMENTS)];

  

It is also possible to modify the default behavior of all the partitions
via an attribute definition clause applied to the predefined type
**Partition**.


::

  REPRESENTATION_CLAUSE ::=
     **for** Partition'ATTRIBUTE_NAME **use** ATTRIBUTE_ARGUMENTS;

  

It is also possible to modify the default behavior of a given partition
via an attribute definition clause applied to the partition itself.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'ATTRIBUTE_NAME **use** ATTRIBUTE_ARGUMENTS;

  

When an attribute definition clause is applied to a given object of a
predefined type, this overrides any attribute definition of the
predefined type. In the next sections, attributes apply to a given
object rather than to the predefined type.

.. _Configuration_Declaration:

Configuration Declaration
^^^^^^^^^^^^^^^^^^^^^^^^^

The distribution of one or several Ada programs is described by a single
configuration unit. This configuration unit has a specification part and
an optional body part. A configuration unit is declared as an Ada
procedure would be. The keyword **configuration** is reserved for this
purpose.


::

  CONFIGURATION_UNIT ::=
     **configuration** IDENTIFIER **is**
        DECLARATIVE_PART
     [**begin**
        SEQUENCE_OF_STATEMENTS]
     **end** [IDENTIFIER];

  

.. _Partition_Declaration:

Partition Declaration
^^^^^^^^^^^^^^^^^^^^^

In the declarative part, the user declares his partitions and can change
their default behavior. `po_gnatdist` provides a predefined type
**Partition**. The user can declare a list of partitions and can also
initialize these partitions with an initial list of Ada units.


::

  DECLARATIVE_PART ::= {DECLARATIVE_ITEM}

  DECLARATIVE_ITEM ::=
     PARTITION_DECLARATION
   | REPRESENTATION_CLAUSE
   | SUBPROGRAM_DECLARATION
   | PRAGMA

  SUBPROGRAM_DECLARATION ::=
     MAIN_PROCEDURE_DECLARATION
   | PROCEDURE_DECLARATION
   | FUNCTION_DECLARATION

  PARTITION_DECLARATION ::=
     DEFINING_IDENTIFIER_LIST : Partition
        [:= ENUMERATION_OF_ADA_UNITS];

  DEFINING_IDENTIFIER_LIST ::=
     DEFINING_IDENTIFIER {, DEFINING_IDENTIFIER}

  STATEMENT ::=
     IDENTIFIER := ENUMERATION_OF_ADA_UNITS;

  SEQUENCE_OF_STATEMENTS ::=
     STATEMENT {STATEMENT}

  

Once declared, a partition is an empty list of Ada units. The operator
**":="** adds the Ada units list on the right side to the current list
of Ada units that are already mapped to the partition. This is a
non-destructive operation. Whether a unit is a relevant Ada unit or not
is checked later on by the back-end of `po_gnatdist`. These assignments
can occur in the declarative part as well as in the body part.


::

  ENUMERATION_OF_ADA_UNITS ::= ({ADA_UNIT {, ADA_UNIT}});

  

.. _Location_Declaration:

Location Declaration
^^^^^^^^^^^^^^^^^^^^

There are several kinds of location in the configuration
language. We shall present them in the next subsections, but here is a
short overview of these locations:


* Boot_Location defines the network locations to use to communicate
  with the the boot server during the boot phase

* Self_Location defines the network locations to use by others
  to communicate with the current partition

* Data_Location defines the data storage location used by the
  current partition to map its shared passive units


A location is composed of a support name and a specific data for this
support. For instance, a network location is composed of a protocol name
like *tcp* and a protocol data like *<machine>:<port>*. A storage
location is composed of a storage support name like *dfs* (for
Distributed File System) and a storage support data like a directory
*/dfs/glade*.


::

  LOCATION      ::= ([Support_Name =>] STRING_LITERAL,
                     [Support_Data =>] STRING_LITERAL)

  LOCATION_LIST ::= (LOCATION [,LOCATION)])

  

Note that a location may have an undefined or incomplete support
data. In this case, the support is free to compute a support data. For
instance, ("tcp", "") specifies that the protocol is used but that the
protocol data *<machine>:<port>* is to be determined by the protocol
itself.

A location or a list of locations can be can be concatenated into a
single string to be used as a command line option or an environment
variable (see :ref:`Partition_Runtime_Parameters`).

If a partition wants to communicate with another partition once the
location list of the latter is known, the caller will use the first
location of the callee whose protocol is locally available. For
instance, if a callee exports three locations ("N1", "D1"), ("N2", "D2")
and ("N3", "D3"), a caller with protocols N2 and N3 locally available
will try to communicate with the callee using the protocol of name N2
and of specific data D2.

.. _Partition_Attribute_Main:

Partition Attribute Main
^^^^^^^^^^^^^^^^^^^^^^^^

Basically, the distributed system annex (DSA) helps the user in building
a distributed application from a non-distributed application (Of course,
this is not the only possible model offered by DSA). The user can
design, implement and test his application in a non-distributed
environment, and then should be able to switch from the non-distributed
case to a distributed case. As mentioned before, this two-phase design
approach has several advantages.

In a non-distributed case, the user executes only one main executable
possibly with a name corresponding to the main unit name of his
application. With `po_gnatdist`, in a distributed case, a main executable
with a name corresponding to the main unit name is responsible for
starting the entire distributed application. Therefore, the user can
start his application the same way he used to do in the non-distributed
case.

For this reason, the configuration language provides a way to declare
the main procedure of the non-distributed application.


::

  MAIN_PROCEDURE_IDENTIFIER ::=
    ADA_UNIT
  MAIN_PROCEDURE_DECLARATION ::=
     **procedure** MAIN_PROCEDURE_IDENTIFIER **is in** PARTITION_IDENTIFIER;

  

In this case, the partition in which the main procedure has been mapped
is called the main partition. It includes in its code a call to this
main procedure. The main partition has an additional specific role,
because the boot server is located on it (see :ref:`PolyORB_PCS_Internals`).

The main procedures for the other partitions have a null body. However,
the user can also modify this behavior by providing an alternate main
procedure. To do this, an alternate main subprogram has to be declared
and assigned to the partition Main attribute.


::

  PROCEDURE_DECLARATION ::=
     **procedure** PROCEDURE_IDENTIFIER;

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Main **use** PROCEDURE_IDENTIFIER;

  

.. _Pragma_Starter:

Pragma Starter
^^^^^^^^^^^^^^

As a default, the main executable is a full Ada starter procedure. That
means that it launches all the other partitions from an Ada program. The
pragma Starter allows the user to ask for one starter or another. When
the partition host is not statically defined (see :ref:`Partition_Attribute_Host`), the starter subprogram will ask for it interactively
when it is executed.


::

  CONVENTION_LITERAL ::= Ada   |
                         Shell |
                         None

  PRAGMA ::=
     **pragma** Starter ([Convention =>] CONVENTION_LITERAL);

  


* 
  The default method consists in launching partitions from the main
  partition Ada subprogram using a remote shell (see below).

* 
  The user may ask for a Shell script that starts the different partitions
  one at a time on the appropriate remote machines, using a remote
  shell. As the Ada starter, the Shell script starter ask for partition
  hosts interactively when a partition host is not already defined. Having
  a textual shell script allows the user to edit it and to modify it
  easily.

* 
  The user may ask for a None starter. In this case, it is up to the user
  to launch the different partitions.


Pragma Remote_Shell
^^^^^^^^^^^^^^^^^^^

When pragma Starter is Ada or Shell, the main partition launches the
other partitions. The remote shell used as a default is determined
during PolyORB configuration and installation. It is either rsh, remsh or
the argument passed to --with-rshcmd=[ARG]. The pragma Remote_Shell
allows the user to override the default.


::

  PRAGMA ::=
     **pragma** Remote_Shell
       ([Command =>] STRING_LITERAL,
        [Options =>] STRING_LITERAL);

  

The Command parameter indicates the name of the remote shell command
name and the Options parameter corresponds to the additional flags to
pass to the remote shell command.

.. _Pragma_Name_Server:

Pragma Name_Server
^^^^^^^^^^^^^^^^^^


::


  NAME_SERVER_LITERAL ::= Embedded   |
                          Standalone |
                          None

  PRAGMA ::=
     **pragma** Name_Server ([Name_Server_Kind =>] NAME_SERVER_LITERAL);

  

By default, partitions in a PolyORB/DSA application rely on an external,
stand-alone name server launched by the user, and whose location is retrieved
from runtime configuration.

A pragma Name_Server with parameter Embedded can be used to request the
PCS to instead set up a name server within the main partition. If the
Ada starter is used, the location of the name server is passed automatically
to slave partitions.

A pragma Name_Server with parameter None specifies that no name server is
present in the application. In this case the location of each partition
must be specified in the `po_gnatdist` configuration file, or in
PolyORB run-time configuration.

.. _Pragma_Boot_Location:

Pragma Boot_Location
^^^^^^^^^^^^^^^^^^^^

When a partition starts executing, one of the first steps consists in a
connection to the boot server. This pragma provides one or more
locations in order to get a connection with the boot server.


::

  PRAGMA ::=
     PRAGMA_WITH_NAME_AND_DATA
   | PRAGMA_WITH_LOCATION
   | PRAGMA_WITH_LOCATION_LIST

  PRAGMA_WITH_NAME_AND_DATA ::=
     **pragma** Boot_Location
       ([Protocol_Name =>] STRING_LITERAL,
        [Protocol_Data =>] STRING_LITERAL);

  PRAGMA_WITH_LOCATION ::=
     **pragma** Boot_Location ([Location =>] LOCATION);

  PRAGMA_WITH_LOCATION_LIST ::=
     **pragma** Boot_Location ([Locations =>] LOCATION_LIST);

  

This boot server location can be concatenated into a single string to be
used as a command line option or an environment variable (see
:ref:`Partition_Runtime_Parameters`).

**Note: pragma Boot_Server is now obsolete. It is recommended to use pragma Boot_Location. This wording is more consistent with the rest of the configuration language (see Self_Location :ref:`Partition_Option_self_location**_and_Data_Location_@ref{Partition_Option_data_location`).}

.. _Partition_Attribute_Self_Location:

Partition Attribute Self_Location
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Except for the boot partition on which the boot server is located, a
partition is reachable through a dynamically computed location (for
instance, the partition looks for a free port when the protocol is
tcp). The user may want such a partition to be reachable from a given
fixed location defined in configuration.

This is achieved by setting the Self_Location attribute for the partition.
In particular a location must be defined for the main partition, and each
partition on which an RCI is assigned, if no name server is used.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Self_Location **use** LOCATION;
   | **for** PARTITION_IDENTIFIER'Self_Location **use** LOCATION_LIST;

  

If the attribute definition clause applies to the predefined type
**Partition**, the locations have to be incomplete. Otherwise, all the
partitions would be reachable through the same locations, which is
definitively not recommended.

When an attribute self_location definition clause applies to a given
partition, the protocol units needed for this partition are linked in
the executable. By default, when the self_location attribute is not
redefined, the default protocol used by the partition and loaded in its
executable is the *tcp* protocol.

.. _Partition_Attribute_Passive:

Partition Attribute Passive
^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, a partition is an active partition. This attribute allows to
define a passive partition. In this case, `po_gnatdist` checks that only
shared passive units are mapped on the partition. As this partition
cannot register itself, its location is hard-coded in all the partitions
that depend on its shared passive units.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Passive **use** BOOLEAN_LITERAL;

  

.. _Partition_Attribute_Data_Location:

Partition Attribute Data_Location
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Shared passive units can be mapped on passive or active partitions. In
both cases, it is possible to choose the data storage support and to
configure it with the specific data of a location.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Data_Location **use** LOCATION;
   | **for** PARTITION_IDENTIFIER'Data_Location **use** LOCATION_LIST;

  

When an attribute data_location definition clause applies to a given
partition, the data storage support units needed for this partition are
linked in the executable. By default, when the data_location attribute
is not redefined, the default storage support used by the partition and
loaded in its executable is the *dfs* support. *dfs*, Distributed
File System, is a storage support available as soon as files can be
shared between partitions.

It is not possible to map the different shared passive units of a given
partition on different data storage locations. PolyORB requires all the
shared passive units of a given partition to be mapped on the same
storage support. When the attribute data_location applied to a partition
is a list of locations, all the storage support units needed for this
partition are linked in the executable. By default, only the first one
is activated. The user can choose to change the activated support by
another one specified in the location list. This can be done using the
partition option data_location (see :ref:`Partition_Option_data_location`).

As passive partitions cannot be activated, it is not possible to provide
a location list as a data_location attribute. It is not possible to
change dynamically its location either.

.. _Partition_Attribute_Allow_Light_PCS:

Partition Attribute Allow_Light_PCS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On some circumstances, `po_gnatdist` can detect that a partition does not need
the full PCS functionalities. This occurs in particular when the
partition does use any task, any RCI unit or any RACW object. Therefore,
the partition does not receive any message that is not a reply to a
previous request. In this case, the PCS does not drag in the tasking
library and a light PCS is linked in the partition executable.
This specific configuration is automatically determined by
`po_gnatdist` with the ALI file information.

This optimization can be inappropriate especially when the user wants to
use the "Distributed Shared Memory" storage support which runs Li and
Hudak's algorithm. In this case, messages are exchanged without being
replies to previously sent requests and the normal PCS should be linked
instead of the light one. Note also that `po_gnatdist` cannot know for sure
that the DSM storage support assigned at configuration time is used at
run-time. The user can configure this optimization with the following
attribute.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Allow_Light_PCS **use** BOOLEAN_LITERAL;

  

.. _Pragma_Priority:

Pragma Priority
^^^^^^^^^^^^^^^

It might be necessary for real-time applications to get control over the
priority at which a remote procedure call is executed. By default,
the PCS sends the priority of the client to the server which sets the
priority of an anonymous task to this value. The pragma Priority allows
to decide which priority policy should apply in the distributed
application.


::

  PRIORITY_POLICY_LITERAL ::= Server_Declared
                            | Client_Propagated

  PRAGMA ::=
     **pragma** Priority ([Policy =>] PRIORITY_POLICY_LITERAL);

  


* 
  The default policy Client_Propagated consists in propagating the client
  priority to the server.

* 
  The policy Server_Declared consists in executing the remote procedure
  call at a priority specific to the partition. This priority can be
  set using the partition attribute Priority.


.. _Partition_Attribute_Priority:

Partition Attribute Priority
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This attribute allows to set the priority at which level a remote
procedure call is executed on a server when the priority policy is
Server_Declared. By default, the default priority of the anonymous task
is the default task priority.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Priority **use** INTEGER_LITERAL;

  

.. _Partition_Attribute_Host:

Partition Attribute Host
^^^^^^^^^^^^^^^^^^^^^^^^

Logical nodes (or partitions) can be mapped onto physical nodes. The
host-name can be either a static or dynamic value. In case of a static
value, the expression is a string literal. In case of a dynamic value,
the representation clause argument is a function that accepts a string
as parameter and that returns a string value. When the function is
called, the partition name is passed as parameter and the host-name is
returned.


::

  FUNCTION_DECLARATION ::=
     **function** FUNCTION_IDENTIFIER
       (PARAMETER_IDENTIFIER : [**in**] String)
        **return** String;

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Host **use** STRING_LITERAL;
   | **for** PARTITION_IDENTIFIER'Host **use** FUNCTION_IDENTIFIER;

  

The signature of the function must be the following : it takes a
string parameter which corresponds to a partition name. It returns
a string parameter which corresponds to the host-name. The function
that returns the host-name can be an Ada function (default) or a shell
script. A pragma Import is used to import a function defined in Ada or
in Shell (see :ref:`Pragma_Import`).

This function is called on the main partition by the PCS to launch
a given partition on a given logical node. In case of load balancing,
the function can return the most appropriate among a set of hosts.

.. _Pragma_Import:

Pragma Import
^^^^^^^^^^^^^

Two kinds of subprograms are allowed in the configuration
language. A main procedure is used as a partition Main attribute and a
function is used as a partition Host attribute.


::

  PROCEDURE_DECLARATION ::=
       **procedure** PROCEDURE_IDENTIFIER;
  FUNCTION_DECLARATION ::=
       **function** FUNCTION_IDENTIFIER
          (PARAMETER_IDENTIFIER : [**in**] String)
           **return** String;

  

The function can be an Ada function (default) or a shell script. To
import a shell script, the pragma Import must be used:


::

  PRAGMA ::=
     **pragma** Import
        ([Entity        =>] FUNCTION_IDENTIFIER,
         [Convention    =>] CONVENTION_LITERAL,
         [External_Name =>] STRING_LITERAL);

  **pragma** Import (Best_Node, Shell, "best-node");
  

In this case, the PCS invokes the shell script with the partition
name as a command line argument. The shell script is supposed to return
the partition host-name (see :ref:`Partition_Attribute_Host`).

.. _Partition_Attribute_Directory:

Partition Attribute Directory
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Directory allows the user to specify in which directory the partition
executable is stored. This can be useful in heterogeneous systems when
the user wants to store executables for the same target in a given
directory. Specifying the directory is also useful if the partition
executable is not directly visible from the user environment. For
instance, when a remote command like **rsh** is invoked, the executable
directory has to be present in the user path. If the Directory
attribute has been specified, the executable full name is used.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Directory **use** STRING_LITERAL;

  

.. _Partition_Attribute_Command_Line:

Partition Attribute Command_Line
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The user may want to pass arguments on the command line of a
partition. However, when a partition is launched automatically by the
main partition, the partition command line includes only PolyORB
arguments. To add arguments on the command line, the user can take
advantage of the following attribute.


::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Command_Line **use** STRING_LITERAL;

  

.. _Partition_Attribute_Environment_Variables:

Partition Attribute Environment_Variables
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The attribute Environment_Variables allows the user to specify a list of
environment variables that should be passed from the main partition
to slave partitions when using a generated (shell or Ada) launcher.

This attribute can be applied to all partitions by defining it for the
predefined type **Partition**, or to a specific partition. Note that in
the latter case, the list does not replace the default one but instead
complements it (i.e. variables specified for **Partition** are passed
in addition to the partition specific ones).

Use of this features requires that remote nodes provide the POSIX env(1)
command.


::

  STRING_LITERAL_LIST ::=
     STRING_LITERAL
     | STRING_LITERAL**,** STRING_LITERAL_LIST

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Environment_Variables **use (**STRING_LITERAL_LIST**);**

  

.. _Partition_Attribute_Termination:

Partition Attribute Termination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The Ada Reference Manual does not provide any specific rule to handle
global termination of a distributed application (see :ref:`Abortion_and_Termination`).

In PolyORB/DSA, by default, a set of partitions terminates when each partition
can terminate and when no message remains to be delivered. A distributed
algorithm that checks for this global condition is activated
periodically by the main boot server.


::

  TERMINATION_LITERAL ::= Global_Termination |
                          Local_Termination  |
                          Deferred_Termination

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Termination **use** TERMINATION_LITERAL;

  


* When a partition is configured with the global termination policy,
  it terminates as soon as the main boot server sends a signal to do
  so. The main boot server checks periodically whether the application can
  terminate. When all partitions are ready to terminate, the main boot
  server sends to each partition a termination request. The global
  termination policy is the default policy.

* The deferred termination policy is very similar to the global
  termination. The only difference is that when a partition with a
  deferred termination policy receives a termination request, it just
  ignores it. This policy allows a partition to run forever without
  preventing a set of partitions from terminating.

* When a partition is configured with the local termination policy,
  it terminates as soon as the classical Ada termination is detected by
  the partition. It means that this partition does not wait for the
  termination request of the main boot server.


.. _Partition_Attribute_Reconnection:

Partition Attribute Reconnection
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When no RCI package is configured on a partition, such a partition can
be launched several times without any problem. When one or more RCI
packages are configured on a partition, such a partition cannot be
launched more than once. If this partition were to be launched
repeatedly, it would not be possible to decide which partition instance
should execute a remote procedure call.

When a partition crashes or is stopped, one may want to restart this
partition and possibly restore its state - with Shared_Passive packages,
for instance. In such a situation, the partition is already known to
other partitions and possibly marked as a dead partition. Several
policies can be selected:


::

  RECONNECTION_LITERAL ::= Reject_On_Restart  |
                           Fail_Until_Restart |
                           Block_Until_Restart

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Reconnection **use** RECONNECTION_LITERAL;

  


* When this partition is configured with the Reject_On_Restart
  reconnection policy, the dead partition is kept dead and any attempt to
  restart it fails. Any remote call to a subprogram located on this
  partition results in a Communication_Error exception. The
  Reject_On_Restart policy is the default policy.

* When this partition is configured with the Fail_Until_Restart
  reconnection policy, the dead partition can be restarted. Any remote
  call to a subprogram located on this partition results in an exception
  Communication_Error as long as this partition has not been restarted. As
  soon as the partition is restarted, remote calls to this partition are
  executed normally.

* When this partition is configured with the Block_Until_Restart
  reconnection policy, the dead partition partition can be restarted. Any
  remote call to a subprogram located on this partition is suspended until
  the partition is restarted. As soon as the partition is restarted,
  remote calls to this partition are executed normally. The suspended
  remote procedure calls to this partition are resumed.


.. _Pragma_Version:

Pragma Version
^^^^^^^^^^^^^^

A library unit is consistent if the same version of its declaration is
used throughout (see :ref:`Consistency_and_Elaboration`). It can be
useful to deactivate these checks, especially when the user wants to be
able to update a server without updating a client.


::

  PRAGMA ::=
     **pragma** Version ([Check =>] BOOLEAN_LITERAL);

  

.. _Partition_Attribute_Task_Pool:

Partition Attribute Task_Pool
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When multiple remote subprogram calls occur on the same partition, they
are handled by several anonymous tasks. These tasks can be allocated
dynamically or re-used from a pool of (preallocated) tasks. When a
remote subprogram call is completed, the anonymous task can be
deallocated or queued in a pool in order to be re-used for further
remote subprogram calls. The number of tasks in the anonymous tasks pool
can be configured by means of three independent parameters.


* The task pool minimum size indicates the number of anonymous tasks
  preallocated and always available in the PCS. Preallocating
  anonymous tasks can be useful in real-time systems to prevent task
  dynamic allocation.

* The task pool high size is a ceiling. When a remote subprogram
  call is completed, its anonymous task is deallocated if the number of
  tasks already in the pool is greater than the ceiling. If not, then the
  task is queued in the pool.

* The task pool maximum size indicates the maximum number of anonymous
  tasks in the PCS. In other words, it provides a way to limit the
  number of remote calls in the PCS. When a RPC request is received, if
  the number of active remote calls is greater than the task pool maximum
  size, then the request is kept pending until an anonymous task completes
  its own remote call and becomes available.



::

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'Task_Pool **use** TASK_POOL_SIZE_ARRAY;

  TASK_POOL_SIZE_ARRAY ::=
    (NATURAL_LITERAL,  *--  Task Pool Minimum Size*
     NATURAL_LITERAL,  *--  Task Pool High Size*
     NATURAL_LITERAL); *--  Task Pool Maximum Size*

  

In order to have only one active remote call at a  time, the task pool
configuration is declared as follows:


::

  **for** Partition'Task_Pool **use** (0, 0, 1);

  

.. _Partition_Attribute_ORB_Tasking_Policy:

Partition Attribute ORB_Tasking_Policy
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, the Thread_Pool ORB tasking policy is used for all partitions.
This attribute allows selection of an alternate policy among those
provided by PolyORB (see :ref:`PolyORB_ORB_Tasking_policies`) for
each partition.


::

  ORB_TASKING_POLICY_LITERAL ::= Thread_Pool        |
                                 Thread_Per_Session |
                                 Thread_Per_Request

  REPRESENTATION_CLAUSE ::=
     **for** PARTITION_IDENTIFIER'ORB_Tasking_Policy **use** ORB_TASKING_POLICY_LITERAL;

  

*Note: @ref{Partition Attribute Task_Pool* has no effect when another policy
than Thread_Pool is activated.}

.. _A_Complete_Example:

A Complete Example
^^^^^^^^^^^^^^^^^^

Almost every keyword and construct defined in the configuration language
has been used in the following sample configuration file.

.. literalinclude:: myconfig.cfg


* **Line 01**
  Typically, after having created the following configuration file the user
  types:


  ::

    po_gnatdist myconfig.cfg

    

  If the user wants to build only some partitions then he will list the
  partitions to build on the `po_gnatdist` command line as follows:


  ::

    po_gnatdist myconfig.cfg partition_2 partition_3

    

  The name of the file prefix must be the same as the name of the
  configuration unit, in this example `myconfig.cfg`. The file suffix
  must be `cfg`. For a given distributed application the user can have
  as many different configuration files as desired.

* **Line 04**
  Partition 1 contains no RCI package. However, it will contain the main
  procedure of the distributed application, called *Master_Procedure* in
  this example. If the line *procedure Master_Procedure is in Partition_1;* was missing, Partition 1 would be completely empty. This is
  forbidden, because a partition has to contain at least one library unit.

  `po_gnatdist` produces an executable with the name of *Master_Procedure*
  which will start the various partitions on their host machines in the
  background. The main partition is launched in foreground. Note that by
  killing this main procedure the whole distributed application is terminated.

* **Line 08**
  Specify the host on which to run partition 2.

* **Line 12**
  Use the value returned by a program to figure out at execution time the
  name of the host on which partition 3 should execute.  For instance,
  execute the shell script `best-node` which takes the partition name as
  parameter and returns a string giving the name of the machine on which
  partition_3 should be launched.

* **Line 14**
  Partition 4 contains one RCI package RCI_B5 No host is specified for
  this partition. The startup script will ask for it interactively when it
  is executed.

* **Line 16**
  Specify the directory in which the executable of partition partition_1
  will be stored.

* **Line 17**
  Specify the directory in which all the partition executables will be
  stored (except partition_1, see :ref:`Pragmas_and_Representation_Clauses`). Default is the current directory.

* **Line 20**
  Specify the partition main subprogram to use in a given partition.

* **Line 22**
  Specify a reconnection policy in case of a crash of Partition_3. Any attempt to
  reconnect to Partition_3 when this partition is dead will be
  blocked until Partition_3 restarts. By default, any restart is
  rejected (Reject_On_Restart). Another policy is to raise
  Communication_Error on any reconnection attempt until Partition_3 has
  been restarted.

* **Line 23**
  Specify additional arguments to pass on the command line when a given
  partition is launched.

* **Line 24**
  Specify a termination mechanism for partition_4. The default is to
  compute a global distributed termination. When Local_Termination is
  specified a partition terminates as soon as local termination is
  detected (standard Ada termination).

* **Line 26**
  Specify the kind of startup method the user wants. There are 3
  possibilities: Shell, Ada and None. Specifying *Shell* builds a shell
  script. All the partitions will be launched from a shell script.  If
  *Ada* is chosen, then the main Ada procedure itself is used to launch
  the various partitions. If method *None* is chosen, then no launch
  method is used and the user must start each partition manually.

  If no starter is given, then an Ada starter will be used.

  In this example, Partition_2, Partitions_3 and Partition_4 will be
  started from Partition_1 (ie from the Ada procedure Master_Procedure).

* **Line 30**
  Specify the use of a particular boot server.

* **Line 32**
  It is a bounded error to elaborate a partition of a distributed
  program that contains a compilation unit that depends on a different
  version of the declaration of an RCI library unit than the one included in
  the partition to which the RCI library unit was assigned. When the
  pragma Version is set to False, no consistency check is performed.

* **Line 34**
  The configuration body is optional. The user may have fully described
  his configuration in the declaration part.

* **Line 35**
  Partition 2 contains two RCI packages RCI_B2 and RCI_B4 and a normal
  package. A normal package is not categorized.

* **Line 36**
  Partition 3 contains one RCI package RCI_B3


.. _Partition_Runtime_Parameters:

Partition Runtime Parameters
----------------------------

You can adjust some parameters of your DSA applications using the PolyORB
configuration file, :file:`polyorb.conf`. The parameters relevant to the
Ada Distributed Systems Annex are specified in the `[dsa]` section.

See :ref:`Run-time_configuration` for complete documentation of PolyORB's
runtime configuration facilities.



*name_service = [IOR/corbaloc]*
  You can set this parameter instead of the environment variable
  `POLYORB_DSA_NAME_SERVICE`.
  Though if you use a Starter, ensure that this parameter is set for
  all the partitions, as this is not done automatically as for the
  `POLYORB_DSA_NAME_SERVICE` environment variable.


*max_failed_requests = [integer]*
  Each partition will attempt a given number of requests to the
  name server before failing. This allows some time for every
  partition to register in the name server.


*delay_between_failed_requests = [duration in milliseconds]*
  As above, only this specifies the delay between requests.


*termination_initiator = [true/false]*
  Is this partition a termination initiator.


*termination_policy = [global_termination/deferred_termination/local_termination]*
  The termination policy for this partition.


*tm_time_between_waves = [duration in milliseconds]*
  The delay between termination waves.


*tm_time_before_start = [duration in milliseconds]*
  The delay before the termination manager starts sending waves.


*detach = [true/false]*
  If true, the partition will be detached.


*rsh_options = [string]*
  Options passed to the rsh command when using the module
  polyorb.dsa_p-remote_launch


*rsh_command = [string]*
  Which command should the module polyorb.dsa_p-remote_launch
  use to spawn remote programs.

.. _Gnatdist_Internals:

Gnatdist Internals
------------------

Here is what goes on in `po_gnatdist` when building a distributed application:

* 
  Each compilation unit in the program is compiled into an object module
  (as for non distributed applications). This is achieved by calling
  gnatmake on the sources of the various partitions.

* 
  Stubs and skeletons are compiled into object modules (these are pieces of
  code that allow a partition running on machine A to communicate with a
  partition running on machine B). Several timestamp checks are performed
  to avoid useless code recompilation and stub generation.

* 
  `po_gnatdist` performs a number of consistency checks. For instance it checks
  that all packages marked as remote call interface (RCI) and shared
  passive (SP) are mapped onto partitions. It also checks that a RCI or SP
  package is mapped onto only one partition.

* 
  Finally, the executables for each partition in the program are
  created. The code to launch partitions is embedded in the main partition
  except if another option has been specified (see :ref:`Pragma_Starter`). In this case, a shell script (or nothing) is generated to
  start the partitions on the appropriate machines. This is specially
  useful when one wants to write client / server applications where the
  number of instances of the partition is unknown.


All Gnatdist intermediate files (object files, etc) are stored under a
common directory named "dsa". The user may remove this whole directory and
its content when he does not intend to rebuild his distributed
applications.

.. _PolyORB_PCS_Internals:

PolyORB PCS Internals
---------------------

This section provides notes on the PolyORB implementation of the DSA PCS.
Some of these features are not configurable by the user.

.. _Application_Startup:

Application Startup
^^^^^^^^^^^^^^^^^^^

A name server normally needs to be started prior to starting any application
partition. Once the name server is started, its location must be passed
to all partitions as the `name_service` runtime parameter in the
`[dsa]` section of the configuration. When using an Ada starter, it is
sufficient to pass the name server location to the starter, and it will be
propagated automatically to all partitions. When using an embedded name
server, the name server is part of the main partition, and does not need
to be passed explicitly.

Upon elaboration, each partition registers its RCI packages with the
name server. Once this is done, remote calls to RCI subprograms can proceed.
Partitions cache the replies from the name server so that during the
course of normal execution, inter-partition calls only involve the caller
and callee partitions (not the name server).

When the name server kind is set to None, no name server is started, and
no attempt is made to register RCI units. Their locations must then be
set in the `po_gnatdist` configuration file using Self_Location attributes
for all partitions, or overridden in run-time configuration by setting the
`<partition>'location` parameter in the @t:cite:`dsa` section. A location
pair `(<protocol-name>, <protocol-data>)` is encoded as a URI:
`<protocol-name>://<protocol-data>`.

For example, to specify that a partition `server_part` is to be reachable
using TCP on host `somehost`, port 5555, either use the following
setting in the `gnatdist` configuration file:


::

     for server_part'Self_Location use ("tcp", "somehost:5555");
  

or the following settings in PolyORB runtime configuration:


::

  [dsa]
  server_part'location=tcp://somehost:5555
  

RCI units then act as 'clearinghouses' for other partitions to exchange
RACWs and set up dynamic communication paths.

.. _Heterogeneous_System:

Heterogeneous System
^^^^^^^^^^^^^^^^^^^^

The GNAT environment provides default stream attributes, except for non-remote
access types (see :ref:`Transmitting_Dynamic_Structure` and
:ref:`Marshalling_and_Unmarshalling_Operations`). The implementation of
the default attributes of predefined types can be found in
*System.Stream_Attributes* (s-stratt.adb).

The PolyORB PCS provides alternative data representations by default to
ensure portability of the data stream across partitions executing on
heterogeneous architectures. Users may override these representation aspects
by configuring the protocol personality of their choice.

.. _Allocating_Partition_Ids:

Allocating Partition Ids
^^^^^^^^^^^^^^^^^^^^^^^^

The Partition_ID is allocated dynamically, at run-time. Each partition
connects to a Partition ID Server which is located on the boot server
and asks for a free Partition_ID. The advantage of this approach is that
it supports easily client / server solution (client partitions may be
duplicated, they will obtain different Partition Ids). There is no need
to recompile or relink all the partitions when a new partition is added
to the system. The Partition_ID is not tied in any way to a specific
protocol or to a specific location.

.. _Executing_Concurrent_Remote_Calls:

Executing Concurrent Remote Calls
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

When multiple remote subprogram calls occur on the same partition, they
are handled by several anonymous tasks. The number of tasks in the
anonymous tasks pool can be configured by three figures (see
:ref:`Partition_Attribute_Task_Pool`). Therefore, the user may have to
synchronize global data in the Remote_Call_Interface or Remote_Types
unit to preserve concurrent access on data. If the user want to suppress
the multiple requests features, he can force the configuration of the
anonymous tasks pool to (0 | 1, 0 | 1, 1). That means that there will be
at most one anonymous task running at a time.

.. _Priority_Inheritance:

Priority Inheritance
^^^^^^^^^^^^^^^^^^^^

It is compiler-dependent whether the caller priority is preserved during
a remote procedure call. In fact, it can be unsafe to rely on
priorities, because two partitions may have different priority ranges
and policies. Nevertheless, PolyORB preserves the caller priority. This
priority is marshaled and unmarshaled during the remote procedure call
and the priority of the anonymous task on the server is set to the
caller priority.

This default policy can be modified by using pragma Priority
:ref:`Pragma_Priority` and partition attribute
Priority :ref:`Partition_Attribute_Priority`.

.. _Remote_Call_Abortion:

Remote Call Abortion
^^^^^^^^^^^^^^^^^^^^

When a remote procedure call is aborted, PolyORB will abort the calling
task on the caller side. It will also try to abort the remote anonymous
task performing the remote call, unless runtime parameter
`abortable_rpcs` in section `[tasking]` is set False on the server.

.. _Running_a_DSA_application:

Running a DSA application
=========================

By default *po_gnatdist* will use the Ada starter. So if you
have not specified `pragma Starter (None);` in the
*po_gnatdist* configuration file, you should have a starter in
your build directory, named after the main procedure defined in the
configuration file.  In this case you just have to run this program.

If you don't want to use the Starter and have specified `pragma Starter (None);` in your configuration file, then you should have, in
your Partition'Directory, one binary for each of your partitions.
You'll have to start each of these programs manually.

In both cases you must specify a name server for your application.
You can use for example the one included in PolyORB:
:file:`po_cos_naming`. When running this name server it will output its
IOR URI named `POLYORB_CORBA_NAME_SERVICE`.

Just ensure that you set the global environment variable
`POLYORB_DSA_NAME_SERVICE` to an IOR URI referencing the
running name server. When using the :file:`po_cos_naming` name server
just set `POLYORB_DSA_NAME_SERVICE` environment variable to the
first value output for `POLYORB_DSA_NAME_SERVICE` before
launching each DSA partition.

Here is a small trace output that demonstrates the setup


::

  polyorb/examples/dsa/echo% ../../../tools/po_cos_naming/po_cos_naming&
  polyorb/examples/dsa/echo% POLYORB_CORBA_NAME_SERVICE=''....''

  polyorb/examples/dsa/echo% export POLYORB_DSA_NAME_SERVICE=''...''
  polyorb/examples/dsa/echo% ./client
  The client has started!
  Thus spake my server upon me:Hi!
  

