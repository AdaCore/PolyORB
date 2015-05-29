.. _Introduction_to_PolyORB:

***********************
Introduction to PolyORB
***********************

.. index:: PolyORB

.. _Introduction_to_distributed_systems:

Introduction to distributed systems
===================================

A distributed system architecture comprises a network of computers and the
software components that execute on those computers. Such architectures are
commonly used to improve the performance, reliability, and reusability of
complex applications. Typically, there is no shared address space
available to remotely-located components (that is to say, components running
on different nodes of the network), and therefore these components must
communicate using some form of message-passing.

.. _Using_OS_Network_Services:

Using OS Network Services
-------------------------

There are several programming techniques for developing distributed
applications. These applications have traditionally been developed
using network programming interfaces such as sockets. Programmers have
to perform explicit calls to operating system services, a task that
can be tedious and error-prone. This includes initializing socket
connections and determining peer location, marshalling and unmarshalling
data structures, sending and receiving messages, debugging and testing
several programs at the same time, and porting the application to
several platforms to uncover subtle differences between various
network interfaces.

Of course, this communication code can be encapsulated in wrappers to reduce
its complexity, but it is clear that most of it can be automatically
generated. Message passing diverts developer's attention from the
application domain. The query and reply scenario is a classical scheme
in distributed applications; using message passing for such a scheme
can be compared to only using the 'goto' mechanism in a non-distributed
application.  This is considered unacceptable methodology in modern software
engineering. A cleaner and more structured approach consists in using
subprograms.

In some respects, network programming can be compared to parallel programming.
The user can decide to split his code
into several pieces and to multiplex the execution of threads himself, using
a table-driven model. The scheduling code ends up embedded in the user
code. This solution is error-prone and fragile in regard to any future
modification. Relying on an implementation of threads such as provided in a
POSIX operating environment is a better solution. Relying on language
primitives that support concurrency, such as Ada tasks, is best, as the
underlying parallelism support is thus entirely abstracted.

.. _Using_a_Middleware_Environment:

Using a Middleware Environment
------------------------------

A middleware environment is intended to provide high level abstractions
in order to easily develop user applications.  Environments like CORBA
or Distributed Computing Environment (DCE) provide a framework to
develop client/server applications based on the Remote Procedure Call model
(RPC). The RPC model is inspired by the query and reply
scheme. In rough analogy to a regular procedure call, arguments are pushed
onto a stream, along with some data specifying the remote procedure to
be executed. The stream is transmitted over the network to the
server. The server decodes the stream, performs the regular subprogram call
locally, and then puts the output parameters into another stream, along with the
exception (if any) raised by the subprogram execution. The server then
sends this stream back to the caller. The caller decodes the stream and raises
the exception locally if needed.

CORBA provides the same enhancements to the remote procedure model that
object-oriented languages provide to classical procedural languages.  These
enhancements include encapsulation, inheritance, type checking, and
exceptions. These features are offered through an Interface Definition
Language (IDL).

The middleware communication framework provides all the machinery to
perform, somewhat transparently, remote procedure calls or remote object
method invocations. For instance, each CORBA interface communicates
through an Object Request Broker (ORB). A communication subsystem such
as an ORB is intended to allow applications to use objects without being
aware of their underlying message-passing implementation. In addition. the user
may also require a number of more complex services to develop his
distributed application. Some of these services are indispensable, for example
a location service that allows clients to reference remote services via
high level names (as opposed to a low level addressing scheme involving
transport-specific endpoint addresses such as IP addresses and port numbers).
Other services provide domain-independent interfaces that are frequently
used by distributed applications.

If we return to the multi-threaded programming comparison, the
middleware solution is close to what a POSIX library or a language like
Esterel@footnote{`Esterel` is an imperative synchronous language
designed for the specification and the development of reactive systems.}
would provide for developing concurrent applications. A middleware
framework like DCE is close to a POSIX library in terms of abstraction
levels. Functionalities are very low-level and very complex. CORBA is
closer to Esterel in terms of development process.  The control part of
the application can be specified in a description language. The
developer then has to fill in automatically generated source code templates
(stubs and skeletons) to build the computational part of the application. The
distribution is a pre-compilation process and the distributed boundaries
are always explicit. Using CORBA, the distributed part is written in IDL
and the core of the application is written in a host language such as C++.

.. _Using_a_Distributed_Language:

Using a Distributed Language
----------------------------

Rather than defining a new language like the CORBA IDL, an alternative is
to extend an existing programming language with distributed
features. The distributed object paradigm provides a more
object-oriented approach to programming distributed systems. The notion
of a distributed object is an extension to the abstract data type that
allows the services provided in the type interface to be called
independently of where the actual service is executed. When combined
with object-oriented features such as inheritance and polymorphism,
distributed objects offer a more dynamic and structured computational
environment for distributed applications.

The Distributed Systems Annex (DSA) of Ada defines several
extensions that allow the user to write a distributed system entirely in
Ada.  The types of distributed objects, the services they provide, and
the bodies of the remote methods to be executed are all defined in conventional
Ada packages. The Ada model is analogous to the Java/RMI model.
In both languages, the IDL is replaced by well-defined language constructs.
Therefore, the language supports both remote procedure
calls and remote object method invocations transparently, and the semantics
of distribution are consistent with the rest of the language.

A program written in such a language is intended to communicate with a
program written in the same language, but this apparent restriction has
several useful consequences. The language can provide more powerful
features because it is not constrained by the common features available
in all host languages. In Ada, the user will define a specification of
remote services and implement them exactly as he would for ordinary,
non-distributed services. His Ada environment will compile them to
produce a stub file (on the caller side) and a skeleton file that
automatically includes the body of the services (on the receiver
side). Creating objects, obtaining or registering object references or
adapting the object skeleton to the user object implementation are made
transparent because the language environment has a full control over the
development process.

Comparing with multi-threaded programming once again, the language
extension solution is equivalent to the solution adopted for tasking
facilities in Ada.  Writing a distributed application is as simple as
writing a concurrent application: there is no binding consideration and
no code to wrap.  The language and its run-time system take care of
most issues that would divert the programmer's attention from the
application domain.

.. _Distribution_models_and_middleware_standards:

Distribution models and middleware standards
============================================

Middleware provides a framework that hides the complex issues of
distribution, and offers the programmer high-level abstractions that
allow easy and transparent construction of distributed applications.
A number of different standards exist for creating object-oriented
distributed applications. These standards define two subsystems that
enable interaction between application partitions:

* the API seen by the developer's applicative objects;

* the protocol used by the middleware environment to interact with
  other nodes in the distributed application.


Middleware implementations also offer programming guidelines and
development tools to ease the construction of large heterogeneous
distributed systems. Many issues typical to distributed programming
may still arise: application architectural choice, configuration or
deployment. Since there is no 'one size fits all' architecture,
choosing the adequate distribution middleware in its most appropriate
configuration is a key design point that dramatically impacts the
design and performance of an application.

Consequently, applications need to rapidly tailor middleware to the
specific distribution model they require. A distribution model is
defined by the combination of distribution mechanisms made available
to the application. Common examples of such mechanisms are Remote
Procedure Call (RPC), Distributed Objects or Message Passing. A
distribution infrastructure or middleware refers to software that
supports one distribution model (or several), e.g.: OMG CORBA, Java
Remote Method Invocation (RMI), the Distributed Systems Annex of Ada,
Java Message Service (MOM).

.. _The_PolyORB_generic_middleware:

The PolyORB generic middleware
==============================

Typical middleware implementations for one platform support only one
set of such interfaces, predefined configuration capabilities and
cannot interoperate with other platforms. In addition to traditional
middleware implementations, PolyORB provides an original architecture
to enable support for multiple interoperating distribution models in a
uniform canvas.

PolyORB is a polymorphic, reusable infrastructure for building or
prototyping new middleware adapted to specific application needs. It
provides a set of components on top of which various instances can be
elaborated. These instances (or personalities) are views on PolyORB
facilities that are compliant to existing standards, either at the API
level (application personality) or at the protocol level (protocol
personality). These personalities are mutually exclusive views of the
same architecture.

The decoupling of application and protocol personalities, and the
support for multiple simultaneous personalities within the same
running middleware, are key features required for the construction of
interoperable distributed applications. This allows PolyORB to
communicate with middleware that implements different distribution
standards: PolyORB provides middleware-to-middleware interoperability
(M2M).

PolyORB's modularity allows for easy extension and replacement of its
core and personality components, in order to meet specific requirements.
In this way, standard or application-specific personalities can be
created in a streamlined process, from early stage prototyping to
full-featured implementation. The PolyORB architecture also allows
the automatic, just-in-time creation of proxies between incompatible
environments.

You may find additional technical literature on PolyORB, including research
papers and implementation notes, on the project websites:
`http://libre.adacore.com/libre/tools/polyorb/ <http://libre.adacore.com/libre/tools/polyorb/>`_ and
`http://polyorb.objectweb.org/ <http://polyorb.objectweb.org/>`_.

*Note: PolyORB is the project formerly known as DROOPI, a Distributed Reusable Object-Oriented Polymorphic Infrastructure*

