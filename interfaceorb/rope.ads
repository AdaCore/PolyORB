-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                       package Rope                            ----
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
----     This package is wrapped around the C class Ada_Rope       ----
----   declared in Ada_Rope.hh.                                    ----
----     It does not provide any function or type since AdaBroker  ----
----   do not use Rope Object itself. It just manipulates C rope   ----
----   objects coming from C and passing to C functions.           ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.CPP ;

package Rope is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- this type is both a C and an Ada class
   -- it is wrapped around Ada_Rope
   -- (see Ada_Rope.hh)


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__8Ada_Rope");
   -- default constructor of the C class.
   -- Actually, this constructor does nothing.

end Rope ;
