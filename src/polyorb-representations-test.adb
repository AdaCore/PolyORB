------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . R E P R E S E N T A T I O N S . T E S T          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  A dummy data representation method, just for show.

with Ada.Streams;
with PolyORB.Utils.Buffers;

package body PolyORB.Representations.Test is

   use Ada.Streams;
   use PolyORB.Utils.Buffers;

   procedure Marshall_From_Any
     (R      : access Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any_Container'Class;
      Error  : in out Errors.Error_Container)
   is
   begin
      raise Program_Error;
   end Marshall_From_Any;

   procedure Unmarshall_To_Any
     (R      : access Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any_Container'Class;
      Error  : in out Errors.Error_Container)
   is
   begin
      raise Program_Error;
   end Unmarshall_To_Any;

   procedure Marshall_Char (B : access Buffer_Type; C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'(1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   function Unmarshall_Char (B : access Buffer_Type) return Character
   is
      A : Stream_Element_Array (1 .. 1);
   begin
      Align_Unmarshall_Copy (B, Align_1, A);
      return Character'Val (A (1));
   end Unmarshall_Char;

   procedure Marshall_String
     (R : Rep_Test;
      B : access Buffer_Type;
      S : String)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
      for I in S'Range loop
         Marshall_Char (B, S (I));
      end loop;
   end Marshall_String;

   function Unmarshall_String
     (R : Rep_Test;
      B : access Buffer_Type)
     return String
   is
      S : String (1 .. 1024);
      C : Character;
      Last : Integer := S'First - 1;
      Max : constant Stream_Element_Count := Length (B.all);
   begin
      pragma Warnings (Off);
      pragma Unreferenced (R);
      pragma Warnings (On);
      loop
         exit when Last - S'First + 1 = Integer (Max);
         C := Unmarshall_Char (B);
         if C = ASCII.CR then
            C := Unmarshall_Char (B);
            pragma Assert (C = ASCII.LF);

            exit;
         end if;
         Last := Last + 1;
         S (Last) := C;
         exit when Last = S'Last;
      end loop;

      return S (S'First .. Last);
   end Unmarshall_String;

end PolyORB.Representations.Test;
