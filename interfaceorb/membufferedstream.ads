-----------------------------------------------------------------------
-----------------------------------------------------------------------
----                                                               ----
----                         AdaBroker                             ----
----                                                               ----
----                 package membufferedstream                     ----
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
----   is Ada_memBufferedStream. (see Ada_memBufferedStream.hh)    ----
----     It provides two types of methods : the C functions        ----
----   of the Ada_memBufferedStream class and their equivalent     ----
----   in Ada. (he first ones have a C_ prefix.)                   ----
----     In addition, there is a raise_ada_exception function      ----
----   that allows C functions to raise the ada No_Initialisation  ----
----   exception.                                                  ----
----     At last, there is only one Init procedure in place of     ----
----   two in Ada_memBufferedStream since the second one is        ----
----   useless for AdaBroker.                                      ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
-----------------------------------------------------------------------
-----------------------------------------------------------------------


with Ada.Unchecked_Deallocation ;
with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;

with Corba ;
with Sys_Dep ;

package MemBufferedStream is

   type Object is tagged record
      C_Object : System.Address ;
      -- C field : pointer on the underlying C memBufferedStream object
      Init_Ok : Sys_Dep.C_Boolean ;
      -- C field : state of the object (initialized or not)
      Table : Interfaces.CPP.Vtable_Ptr ;
      -- Ada field : needed to interface C++ and Ada
   end record ;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- this type is both a C and an Ada class
   -- it is wrapped around Ada_MemBufferedStream
   -- (see Ada_MemBufferedStream.hh)


   type Object_Ptr is access all Object ;
   -- type pointer on type Object


   procedure Free is new Ada.Unchecked_Deallocation(Object, Object_Ptr) ;
   -- to deallocate Object_Ptr


   procedure Init (Self : in Object'Class ;
                   Bufsize : in Corba.Unsigned_Long) ;
   -- Ada constructor of the class.
   -- This function must be called after each declaration of
   -- an Object object. If it is not, you can not use the object.


   procedure Marshall (A : in Corba.Char ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Char into a membufferedstream object


   procedure UnMarshall (A : out Corba.Char ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Char from a membufferedstream object


   procedure Marshall (A : in Corba.Boolean ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Boolean into a membufferedstream object


   procedure UnMarshall (A : out Corba.Boolean ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Boolean from a membufferedstream object


   procedure Marshall (A : in Corba.Short ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Short into a membufferedstream object


   procedure UnMarshall (A : out Corba.Short ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Short from a membufferedstream object


   procedure Marshall (A : in Corba.Unsigned_Short ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Unsigned_Short into a membufferedstream object


   procedure UnMarshall (A : out Corba.Unsigned_Short ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Unsigned_Short from a membufferedstream object


   procedure Marshall (A : in Corba.Long ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Long into a membufferedstream object


   procedure UnMarshall (A : out Corba.Long ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Long from a membufferedstream object


   procedure Marshall (A : in Corba.Unsigned_Long ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Unsigned_Long into a membufferedstream object


   procedure UnMarshall (A : out Corba.Unsigned_Long ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Unsigned_Long from a membufferedstream object


   procedure Marshall (A : in Corba.Float ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Float into a membufferedstream object


   procedure UnMarshall (A : out Corba.Float ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Float from a membufferedstream object


   procedure Marshall (A : in Corba.Double ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Double into a membufferedstream object


   procedure UnMarshall (A : out Corba.Double ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Double from a membufferedstream object


   procedure Marshall (A : in Corba.Octet ;
                       S : in out Object'Class) ;
   -- Marshalls a Corba.Octet into a membufferedstream object


   procedure UnMarshall (A : out Corba.Octet ;
                         S : in out Object'Class) ;
   -- UnMarshalls a Corba.Octet from a membufferedstream object


   procedure Marshall (A : in Corba.String ;
                       S : in out Object'Class);
   -- Marshalls a Corba.String into a membufferedstream object


   procedure UnMarshall (A : out Corba.String ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.String from a membufferedstream object


   procedure Marshall (A : in Corba.Completion_Status ;
                       S : in out Object'Class);
   -- Marshalls a Corba.Completion_Status into a netbufferedstream object


   procedure UnMarshall (A : out Corba.Completion_Status ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba.Completion_Status from a netbufferedstream object


   procedure Marshall (A : in Corba.Ex_Body'Class ;
                       S : in out Object'Class);
   -- Marshalls a Corba system exception into a membufferedstream object


   procedure UnMarshall (A : out Corba.Ex_Body'Class ;
                         S : in out Object'Class);
   -- UnMarshalls a Corba system exception from a membufferedstream object


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__21Ada_memBufferedStream");
   -- default constructor of the C class.
   -- Actually, this constructor does nothing and you must
   -- call Init to init properly an object.

end MemBufferedStream ;

