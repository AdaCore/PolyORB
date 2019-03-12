PolyORB
=======

Copyright (C) 1999-2019, Free Software Foundation, Inc.

This is free software; you can redistribute it and/or modify it under
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3, or (at your option) any later version. This
software is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively. If not, see
<http://www.gnu.org/licenses/>.

*PolyORB is maintained by AdaCore (email: sales@adacore.com)*

This is the README file for PolyORB.

The home page of the project is located at <https://github.com/AdaCore/PolyORB/>

What is PolyORB?
----------------

PolyORB is a polymorphic, reusable infrastructure for building
object-oriented distributed systems.  Middleware environments are
software libraries that hide the complex issues of distribution and
provide the programmer with high-level abstractions that allow easy
and transparent construction of distributed applications.  A number
of different standards exist for creating object-oriented distributed
applications.  These standards define two things:

* the interface seen by the developer's applicative objects;
* the protocol used by the middleware environment to talk to other
  nodes in the distributed application.

Usually, middleware for one platform supports only one set of such interfaces,
and cannot interoperate with other platforms.

A polymorphic middleware allows the existence of several different
implementations of each of these aspects to be used within the same
middleware framework. In addition, PolyORB allows such different
personalities to coexist in the same instance of the running middleware;
it decouples the personality presented to applications on one side
("application personality"), and the personality presented to other
middleware on the other side ("protocol personality"). Multiple
implementations of each personalisable aspect can coexist within the same
instance of the running middleware: unlike previous generic middleware,
PolyORB is actually schizophrenic.

The decoupling of application and protocol personalities, and the
support for multiple simultaneous personalities within the same running
middleware are key features required for the construction of interoperable
distributed applications. This allows PolyORB to communicate with
middleware that implement different distribution standards: PolyORB
provides middleware-to-middleware interoperability.

The PolyORB architecture also permits the automatic,
just-in-time creation of proxies between incompatible environments
(although this feature is not implemented yet).

PolyORB can be used in Ada 95 and Ada 2005 applications alike. It is
implemented in Ada 2005 and C.

Installation
------------

See INSTALL file for more details on supported platforms and installation
process.

Documentation overview
----------------------

Documentation entry points can be found in the following files:

Filename    | Contents
------------|------------------------------
README.md   | This file, first instructions.
README.DSA  | Additional information for Ada Distributed Systems Annex
INSTALL     | Detail PolyORB installation process.
FEATURES    | List PolyORB's features.
src/ROADMAP | Overview of PolyORB source code.
docs/       | Documents describing PolyORB internals, and sources for the PolyORB User's Guide.

The PolyORB User's Guide is also available online at
<https://docs.adacore.com/polyorb-docs/html/ug_contents.html>

Bug reports
-----------

We gladly accept problem reports and patches from the community. Please
submit them through through Github issues and pull requests.

You can also seek community support through the public mailing list:
   polyorb-users@lists.adacore.com

Please include the complete output of "polyorb-config --version"
in any problem report.

If you are interested in becoming a supported PolyORB user, you should
send an email to sales@adacore.com.

Testsuite
---------

The PoyORB testsuite uses GNATPython.

To use it:

    git clone https://github.com/Nikokrock/gnatpython

Then install it in your python distribution (./setup.py install) or
export PYTHONPATH=/path/to/gnatpython and compile
gnatpython/src/rlimit/rlimit.c (or rlimit-NT.c if you are on a windows
machine) and add it to your PATH.

See output of testsuite.py -h for instructions.

Mailing lists
-------------

The mailing-list PolyORB-Users serves as a informal forum for technical
discussions about PolyORB among users.

You can subscribe to this list and browse the archive at the URL:

<http://lists.adacore.com/mailman/listinfo/polyorb-users>

Contributors:
-------------

PolyORB has been developed since January, 1999 by the following
contributors:

* Dmitriy Anisimkov
* Nicolas Archambault
* Fabien Azavant
* Benjamin Bagland
* Khaled Barbaria
* Nikolay Boshnakov
* Reto Buerki
* Emmanuel Chavane
* Karim Chine
* Jean-Marie Cottin
* Olivier Delalleau
* Cyril Domercq
* Robert Duff
* Michael Friess
* Nicolas Fritsch
* Jeremy Gibbons
* Vadim Godunko
* Jerome Guitton
* Jerome Hugues
* Mejdi Kaddour
* Oliver Kellogg
* Fabrice Kordon
* Narinder Kumar
* Laurent Kubler
* Stéphane Lanarre
* Lionel Litty
* Vincent Niebel
* Pascal Obry
* Pablo Oliveira
* Pierre Palatin
* Bertrand Paquet
* Laurent Pautet
* Sebastien Ponce
* Thomas Quinot
* Nicolas Roche
* Jerome Roussel
* Selvaratnam Senthuran
* Nicolas Setton
* Frank Singhoff
* Samuel Tardieu
* Santiago Urueña-Pascual
* Thomas Vergnaud
* Florian Villoing
* Guillaume Wisniewski
* Thomas Wolf
* Bechir Zalila
