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


with Ada.Exceptions ;
with System.Address_To_Access_Conversions ;

package body OmniRopeAndKey is


   -- C_Init
   ---------
   procedure C_Init (Self : in out Object'Class ;
                     R : System.Address ;
                     K : System.Address ;
                     Ksize : Interfaces.C.Unsigned_Long) ;
   pragma Import (C,C_Init,"Init__18Ada_OmniRopeAndKeyP4RopePUcUl") ;
   -- wrapper around  Ada_OmniRopeAndKey function Init
   -- (see Ada_OmniRopeAndKey.hh)
   -- called by the Ada equivalent : Init


   -- Init
   -------
   procedure Init (Self : in out Object'Class ;
                   R : in Rope.Object ;
                   K : in Corba.Octet ;
                   Ksize : in Corba.Unsigned_Long) is
      C_K : System.Address ;
      C_Ksize : Interfaces.C.Unsigned_Long ;
   begin
      -- transforms the arguments in a C type ...
      C_K := K'Address ;
      C_Ksize := Interfaces.C.Unsigned_Long(Ksize) ;
      -- ... and calls the C procedure
      C_Init (Self,System.Address (R),C_K,C_Ksize) ;
   end;


   -- C_Init2
   ----------
   procedure C_Init2 (Self : in out Object'Class) ;
   pragma Import (C,C_Init2,"Init__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function Init
   -- (see Ada_OmniRopeAndKey.hh)
   -- name was changed to avoid conflict
   -- called by the Ada equivalent : Init


   -- Init
   -------
   procedure Init (Self : in out Object'Class) is
   begin
      C_Init2 (Self) ;
      -- just call the C function
   end;


   -- C_Get_Rope
   -------------
   function C_Get_Rope (Self : in Object'Class) return System.Address ;
   pragma Import (C,C_Get_Rope,"rope__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function rope
   -- (see Ada_OmniRopeAndKey.hh)
   -- called by the Ada equivalent : Get_Rope


   -- Get_Rope
   -----------
   function Get_Rope (Self : in Object'Class)
                      return Rope.Object is
   begin
      -- just calls the C function
      return Rope.Object (C_Get_Rope (Self)) ;
   end;


   -- Address_To_Octet
   -------------------
   package Address_To_Octet is
     new System.Address_To_Access_Conversions (Corba.Octet) ;
   -- needed to interface System.Address and Corba.Octet


   -- C_Get_Key
   ------------
   function C_Get_Key (Self : in Object'Class) return System.Address;
   pragma Import (C,C_Get_Key,"key__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function key
   -- (see Ada_OmniRopeAndKey.hh)
   -- called by the Ada equivalent : Get_Key


   -- Get_Key
   ----------
   function Get_Key (Self : in Object'Class)
                     return CORBA.Octet is
      C_Result : System.Address ;
      Ada_Result_Ptr : Address_To_Octet.Object_Pointer ;
   begin
      -- calls the C function ...
      C_Result := C_Get_Key (Self) ;
      -- ... and transforms the result in Ada type
      Ada_Result_Ptr := Address_To_Octet.To_Pointer (C_Result) ;
      return Ada_Result_Ptr.all ;
   end;


   -- C_Key_Size
   -------------
   function C_Key_Size (Self : in Object'Class) return Interfaces.C.Unsigned_Long ;
   pragma Import (C,C_Key_Size,"keysize__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function keysize
   -- (see Ada_OmniRopeAndKey.hh)
   -- called by the Ada equivalent : Key_Size


   -- Key_Size
   -----------
   function Key_Size (Self : in Object'Class)
                      return CORBA.Unsigned_Long is
      C_Result : Interfaces.C.Unsigned_Long ;
   begin
      -- calls the C function ...
      C_Result := C_Key_Size (Self) ;
      -- ... and transforms the result in Ada type
      return Corba.Unsigned_Long (C_Result) ;
   end;

end OmniRopeAndKey ;
