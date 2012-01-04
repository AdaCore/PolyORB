------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 C I A O _ R U N T I M E . C O N V E R T                  --
--                                                                          --
--                                 S p e c                                  --
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
with CORBA;

package CIAO_Runtime.Convert is

   function To_Ada   (Val : CORBA.Boolean)          return Boolean;

   function To_Ada   (Val : CORBA.Short)            return Integer;
   function To_Ada   (Val : CORBA.Long)             return Integer;
   function To_Ada   (Val : CORBA.Unsigned_Short)   return Integer;
   function To_Ada   (Val : CORBA.Unsigned_Long)    return Integer;

   function To_Ada   (Val : CORBA.Float)            return Float;
   function To_Ada   (Val : CORBA.Double)           return Float;

   function To_Ada   (Val : CORBA.Char)             return Character;
   --  function To_Ada   (Val : CORBA.Octet)            return ;
   function To_Ada   (Val : CORBA.String)           return String;

   pragma Inline (To_Ada);

   function To_CORBA (Val : Boolean)                return CORBA.Boolean;

   function To_CORBA (Val : Integer)                return CORBA.Short;
   function To_CORBA (Val : Integer)                return CORBA.Long;
   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Short;
   function To_CORBA (Val : Integer)                return CORBA.Unsigned_Long;

   function To_CORBA (Val : Float)                  return CORBA.Float;
   function To_CORBA (Val : Float)                  return CORBA.Double;

   function To_CORBA (Val : Character)              return CORBA.Char;
   function To_CORBA (Val : String)                 return CORBA.String;

   pragma Inline (To_CORBA);

end CIAO_Runtime.Convert;
