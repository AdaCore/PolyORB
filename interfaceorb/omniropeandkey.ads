-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                  package Omniropeandkey                       ----
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
----     This package is wrapped around the C++ class              ----
----   Ada_OmniRopeAndKey declared in Ada_OmniRopeAndKey.          ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It includes a Init function since a Ada class has no      ----
----   constructor.                                                ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Finalization ;
with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with Sys_Dep ;

with Key ;
with Rope ;
with Corba ;
with Sys_Dep ;

package OmniRopeAndKey is

   type Object is tagged record
      C_Object : System.Address ;
      -- C field : pointer on the underlying C omniRopeAndKey object
      Init_Ok : Sys_Dep.C_Boolean ;
      -- C field : state of the object (initialized or not)
      Table : Interfaces.CPP.Vtable_Ptr ;
      -- Ada field : needed to interface C++ and Ada
   end record;
   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table,1) ;
   -- this type is both a C and an Ada class
   -- it is wrapped around Ada_OmniRopeAndKey
   -- (see Ada_OmniRopeAndKey.hh)


   type Controlled_Wrapper is new Ada.Finalization.Limited_Controlled with record
      Real : Object ;
   end record ;


   function Get_Rope (Self : in Object'Class) return Rope.Object;
   -- returns rope attribute of the OmniRopeAndKey object
   -- (see omniInternal.h L248 for more information)


   function Get_Key (Self : in Object'Class) return Key.Object;
   pragma Import (CPP,Get_Key,"key__18Ada_OmniRopeAndKey") ;
   -- returns the key attribute of the OmniRopeAndKey object
   -- (see omniInternal.h L250 for more information)
   -- wrapper around  Ada_OmniRopeAndKey function key
   -- (see Ada_OmniRopeAndKey.hh)


   function Key_Size (Self : in Object'Class) return Corba.Unsigned_Long ;
   -- returns the size of the key attribute of the OmniRopeAndKey object
   -- (see omniInternal.h L259 for more information)


   function Equals(Self : in Object'Class ;
                   Other : in Object'Class) return Boolean ;
   pragma Import(CPP, Equals, "equals__18Ada_OmniRopeAndKeyT0") ;
   -- redefinition of the operator to compare the C++ objects
   -- (see Ada_OmniRopeAndKey.hh for more details)


   function "="(Self : in Object'Class ;
                Other : in Object'Class) return Boolean
   renames Equals ;
   -- Comparaison between 2 OmniRopeAndKey Objects.
   -- uses the C function Equals.

private

   procedure Init (Self : in out Object'Class) ;
   pragma Import (CPP,Init,"Init__18Ada_OmniRopeAndKey") ;
   -- Ada constructor of the class.
   -- This function (or the other function Init) must be called
   -- after each declaration of an Object object. If it is not,
   -- you can not use the object.

   procedure Free(Self : in out Object'Class) ;
   pragma Import (CPP, Free, "Free__18Ada_OmniRopeAndKey") ;
   -- deletes the underlying C++ pointer

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__18Ada_OmniRopeAndKey");
   -- default constructor of the C class.
   -- Actually, this constructor does nothing and you must
   -- call Init to init properly an object.


   procedure Initialize(Self : in out Controlled_Wrapper) ;
   --calls Init
   procedure Finalize(Self : in out Controlled_Wrapper) ;
   -- calls Free

end OmniRopeAndKey ;






