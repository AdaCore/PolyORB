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


   -- Initialize
   -------------
   procedure Initialize(Self : in out Controlled_Wrapper) is
   begin
      Init(Self.Real) ;
   end ;


   -- Finalize
   -----------
   procedure Finalize(Self : in out Controlled_Wrapper) is
   begin
      Free(Self.Real) ;
   end ;

end OmniRopeAndKey ;
