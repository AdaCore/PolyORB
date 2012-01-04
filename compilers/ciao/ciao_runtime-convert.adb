------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 C I A O _ R U N T I M E . C O N V E R T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The CIAO run-time library.
package body CIAO_Runtime.Convert is

   function To_Ada   (Val : CORBA.Boolean)          return Boolean is
   begin
      return Val;
   end To_Ada;

   function To_Ada   (Val : CORBA.Short)            return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Long)             return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Unsigned_Short)   return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Unsigned_Long)    return Integer is
   begin
      return Integer (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Float)            return Float is
   begin
      return Float (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Double)           return Float is
   begin
      return Float (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.Char)             return Character is
   begin
      return Character (Val);
   end To_Ada;

   function To_Ada   (Val : CORBA.String)           return String is
   begin
      return CORBA.To_Standard_String (Val);
   end To_Ada;

   function To_CORBA (Val : Boolean)                return CORBA.Boolean is
   begin
      return Val;
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Short is
   begin
      return CORBA.Short (Val);
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Long is
   begin
      return CORBA.Long (Val);
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Short is
   begin
      return CORBA.Unsigned_Short (Val);
   end To_CORBA;

   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long (Val);
   end To_CORBA;

   function To_CORBA (Val : Float)                  return CORBA.Float is
   begin
      return CORBA.Float (Val);
   end To_CORBA;

   function To_CORBA (Val : Float)                  return CORBA.Double is
   begin
      return CORBA.Double (Val);
   end To_CORBA;

   function To_CORBA (Val : Character)              return CORBA.Char is
   begin
      return Val;
   end To_CORBA;

   function To_CORBA (Val : String)                 return CORBA.String is
   begin
      return CORBA.To_CORBA_String (Val);
   end To_CORBA;

end CIAO_Runtime.Convert;
