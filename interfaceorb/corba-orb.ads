-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                    package Corba.Orb                          ----
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
----   This package implements the ORB facilities, as              ----
----   specified in CORBA 2.0                                      ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Interfaces.CPP ;

with Corba.Object ;
with Corba.Boa ;
with Omniobject ;


with Adabroker_Debug ;
pragma Elaborate(Adabroker_Debug) ;

package Corba.Orb is

   Debug : constant Boolean := Adabroker_Debug.Is_Active("corba.orb") ;
   -- debugging  flag

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object corresponds to CORBA::ORB

   type Object_Ptr is access all Object'Class ;


   --------------------------------------------------
   ---          specification CORBA 2.0          ----
   --------------------------------------------------

   function Object_To_String (Obj : in CORBA.Object.Ref'class)
                              return CORBA.String
     renames Corba.Object.Object_To_String ;
   -- string object_to_string (in Object obj);
   -- client-side


   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class)
     renames Corba.Object.String_To_Object ;
   -- Object string_to_object (in string str);
   -- client-side

   function Object_To_String (Obj : in Omniobject.Implemented_Object'class)
                              return CORBA.String
     renames Omniobject.Object_To_String ;
   -- string object_to_string (in Object obj);
   -- server-side



   --------------------------------------------------
   ---        ORB initialization                 ----
   --------------------------------------------------

   function ORB_Init(Orb_Name : in Standard.String) return Object_Ptr ;
   -- initializes the ORB with parameters of the command line
   -- and returns the ORB

   function BOA_Init(Self : in Object_ptr ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object_Ptr ;
   -- initializes the BOA with parameters of the command line
   -- and returns the BOA


private

   function Constructor return Object'Class;
   pragma Import (CPP,Constructor,"__Q25CORBA3ORB");
   pragma CPP_Constructor (Constructor);
   -- wrapped around the C constructor of CORBA::ORB


end Corba.Orb ;
