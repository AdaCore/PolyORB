-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                  package Corba.Forward                        ----
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
----   This package implements the corba specification,            ----
----   to cope with two packages that need one another.            ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Corba.Object ;
with Adabroker_Debug ;
with Ada.Unchecked_Deallocation;
pragma Elaborate(Adabroker_Debug) ;

generic
package Corba.Forward is


   Forward : constant Boolean := Adabroker_Debug.Is_Active("corba.forward") ;

   type Ref is new Corba.Object.Ref with null record;
   type Ref_Ptr is access all Ref ;
   -- just to give a name to pointers on Ref objects

   procedure Free(Self : in out Ref_Ptr) ;
   -- to deallocate Object_Ptr

   function To_Ref(The_Ref: in Corba.Object.Ref'Class) return Ref ;

   -- added in AdaBroker, because To_Ref has to be
   -- overriden for all the descendants of Corba.Object.Ref
   -- We just raise an exceptipon here
   -- The spec does not specify any function To_Ref in
   -- Corba.Object.Ref, but it must be a mistake,
   -- since they say later taht To_Ref has to be defined
   -- for all IDL interfaces (Corba.Object.Ref is indeed
   -- an IDL interface)

   --function Get_Nil_Ref(Self : in Ref) return Corba.Object.Constant_Ref_Ptr ;
   -- see comment for To_Ref

   generic
      type Ref_Type is new Corba.Object.Ref with private ;

   package Convert is
      function From_Forward(The_Forward : in Ref) return Ref_Type ;
      function To_Forward(The_Ref : in Ref_Type) return Ref ;
   end Convert ;


private

   procedure Private_Free is new Ada.Unchecked_Deallocation(Ref, Ref_Ptr) ;

end Corba.Forward ;



