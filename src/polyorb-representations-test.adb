------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . R E P R E S E N T A T I O N S . T E S T          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  A dummy data representation method, just for show.

--  $Id$

with Ada.Streams; use Ada.Streams;
with PolyORB.Utils.Buffers; use PolyORB.Utils.Buffers;

package body PolyORB.Representations.Test is

   procedure Marshall_From_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : Any.Any) is
   begin
      raise Not_Implemented;
   end Marshall_From_Any;

   procedure Unmarshall_To_Any
     (R      : Rep_Test;
      Buffer : access Buffers.Buffer_Type;
      Data   : in out Any.Any) is
   begin
      raise Not_Implemented;
   end Unmarshall_To_Any;

   procedure Marshall_Char
     (B : access Buffer_Type;
      C : Character) is
   begin
      Align_Marshall_Copy
        (B, Stream_Element_Array'
         (1 => Stream_Element (Character'Pos (C))));
   end Marshall_Char;

   function Unmarshall_Char
     (B : access Buffer_Type)
     return Character
   is
      A : constant Stream_Element_Array
        := Align_Unmarshall_Copy (B, 1);
   begin
      return Character'Val (A (A'First));
   end Unmarshall_Char;

   procedure Marshall_String
     (R : access Rep_Test;
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
     (R : access Rep_Test;
      B : access Buffer_Type)
     return String
   is
      S : String (1 .. 1024);
      C : Character;
      Last : Integer := S'First - 1;
      Max : constant Stream_Element_Count
        := Length (B);
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
