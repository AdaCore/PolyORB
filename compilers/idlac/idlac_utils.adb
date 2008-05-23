------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                U T I L S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Utilities for the IDLAC compiler.

with System.Address_Image;

package body Idlac_Utils is

   ---------
   -- Img --
   ---------

   function Img (N : Character) return String is
   begin
      return (1 => N);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Idl_Integer) return String is
      S : constant String := Idl_Integer'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Long_Integer_Img (N : Long_Integer) return String is
   begin
      return Img (Idl_Integer (N));
   end Long_Integer_Img;

   ---------
   -- Img --
   ---------

   function Img (N : Integer) return String is
   begin
      return Img (Idl_Integer (N));
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Idl_Float) return String is
   begin
      return Idl_Float'Image (N);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Id) return String is
   begin
      if N = No_Node then
         return "No_Node";
      else
         return Img (Natural (N));
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (N : Node_Kind) return String is
   begin
      return Node_Kind'Image (N);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (B : Boolean) return String is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (A : System.Address) return String
     renames System.Address_Image;

   ---------
   -- Img --
   ---------

   function Img (A : Constant_Value_Ptr) return String is
   begin
      case A.all.Kind is
         when C_Octet
           | C_Short
           | C_Long
           | C_LongLong
           | C_UShort
           | C_ULong
           | C_ULongLong
           | C_General_Integer =>
            return Img (A.all.Integer_Value);
         when C_Char =>
            --  FIXME : not a correct image
            return "";
         when C_WChar =>
            --  FIXME : not a correct image
            return "";
         when C_Boolean =>
            --  FIXME : not a correct image
            return "";
         when C_Float
           | C_Double
           | C_LongDouble
           | C_General_Float =>
            --  FIXME : not a correct image
            return "";
         when C_String =>
            --  FIXME : not a correct image
            return "";
         when C_WString =>
            --  FIXME : not a correct image
            return "";
         when C_Fixed
           | C_General_Fixed =>
            --  FIXME : not a correct image
            return "";
         when C_Enum =>
            --  FIXME : not a correct image
            return "";
         when C_No_Kind =>
            return "";
      end case;
   end Img;

end Idlac_Utils;
