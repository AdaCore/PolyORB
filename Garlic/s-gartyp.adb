------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T Y P E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams; use Ada.Streams;
with Ada.Unchecked_Conversion;

package body System.Garlic.Types is

   Version_Id_Window : constant Version_Id := Version_Id'Last / 2;

   function Convert is
      new Ada.Unchecked_Conversion (Stamp_Type, Stamp_Stream_Element_Array);
   function Convert is
      new Ada.Unchecked_Conversion (Stamp_Stream_Element_Array, Stamp_Type);

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Version_Id) return Boolean is
   begin
      return Integer (R - L) < Integer (Version_Id_Window);
   end "<";

   --------------
   -- From_SEA --
   --------------

   function  From_SEA (S : Stamp_Stream_Element_Array) return Stamp_Type is
   begin
      return Convert (S);
   end From_SEA;

   ----------
   -- Read --
   ----------

   function Read (S : Partition_ID_SEA) return Partition_ID
   is
      P : Partition_ID := 0;
   begin
      for N in S'Range loop
         P := P * 256 + Partition_ID (S (N));
      end loop;

      return P;
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : out Partition_ID)
   is
      SEA  : Partition_ID_SEA;
      Last : Ada.Streams.Stream_Element_Count;

   begin
      Ada.Streams.Read (S.all, SEA, Last);
      if Last /= SEA'Last then
         raise Constraint_Error;
      end if;
      X := Read (SEA);
   end Read;

   ------------
   -- To_SEA --
   ------------

   function  To_SEA   (S : Stamp_Type) return Stamp_Stream_Element_Array is
   begin
      return Convert (S);
   end To_SEA;

   -----------
   -- Write --
   -----------

   function Write (P : Partition_ID) return Partition_ID_SEA
   is
      S : Partition_ID_SEA;
      X : Partition_ID := P;

   begin
      for N in reverse S'Range loop
         S (N) := Ada.Streams.Stream_Element (X mod 256);
         X := X / 256;
      end loop;

      return S;
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      X : Partition_ID)
   is
      SEA  : Partition_ID_SEA;

   begin
      SEA := Write (X);
      Ada.Streams.Write (S.all, SEA);
   end Write;

end System.Garlic.Types;
