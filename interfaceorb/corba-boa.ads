-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                    package Corba.Boa                          ----
----                                                               ----
----                                                               ----
----   Copyright (C) 1999 ENST                                     ----
----                                                               ----
----   This file is part of the AdaBroker library                  ----
----                                                               ----
----   The AdaBroker library is free software; you can             ----
----   redistribute it and/or modify it under the terms of the     ----
----   GNU Library General Public License as published by the      ----
----   Free Software Foundation; either version 2 of the License,  ----
----   or (at your option) any later version.                      ----
----                                                               ----
----   This library is distributed in the hope that it will be     ----
----   useful, but WITHOUT ANY WARRANTY; without even the implied  ----
----   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ----
----   PURPOSE.  See the GNU Library General Public License for    ----
----   more details.                                               ----
----                                                               ----
----   You should have received a copy of the GNU Library General  ----
----   Public License along with this library; if not, write to    ----
----   the Free Software Foundation, Inc., 59 Temple Place -       ----
----   Suite 330, Boston, MA 02111-1307, USA                       ----
----                                                               ----
----                                                               ----
----                                                               ----
----   Description                                                 ----
----   -----------                                                 ----
----                                                               ----
----     This package is wrapped around a C++ class whose name     ----
----   is BOA declared in file CORBA.h.                            ----
----     It provides two types of methods : the C functions        ----
----   of the BOA class and their equivalent in                    ----
----   Ada. (the first ones have a C_ prefix.)                     ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.CPP ;
with System ;

with Corba.Object ;
with Omniobject ;

package Corba.Boa is

   type Object is new System.Address ;


   procedure Object_Is_Ready(Self: in Object ;
                             Obj: in Omniobject.Implemented_Object'Class ) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an implemented object has been created
   -- to register it into the ORB
   -- (as a local object )
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT


   procedure Object_Is_Ready(Self: in Object ;
                             Obj: in Corba.Object.Ref'Class ) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an implemented object has been created
   -- to register it into the ORB
   -- (as a proxy object )
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT

end Corba.Boa ;

