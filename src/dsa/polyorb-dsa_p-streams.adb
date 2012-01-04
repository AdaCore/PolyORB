------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . D S A _ P . S T R E A M S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

package body PolyORB.DSA_P.Streams is

   -----------
   -- Reset --
   -----------

   procedure Reset (This : access Memory_Resident_Stream) is
   begin
      This.Count    := 0;
      This.Next_In  := 1;
      This.Next_Out := 1;
   end Reset;

   -----------
   -- Write --
   -----------

   procedure Write
      (This : in out Memory_Resident_Stream;
       Item : Stream_Element_Array)
   is
   begin
      for K in Item'Range loop
         This.Values (This.Next_In) := Item (K);
         This.Next_In := (This.Next_In mod This.Size) + 1;
      end loop;
      This.Count := This.Count + Item'Length;
   end Write;

   ----------
   -- Read --
   ----------

   procedure Read
      (This : in out Memory_Resident_Stream;
       Item :    out Stream_Element_Array;
       Last :    out Stream_Element_Offset)
   is
   begin
      if This.Count = 0 then
         Last := Item'First - 1;
         return;
      end if;
      Last := Item'First;
      for K in Item'Range loop
         Item (K) := This.Values (This.Next_Out);
         This.Next_Out := (This.Next_Out mod This.Size) + 1;
         This.Count := This.Count - 1;
         Last := Last + 1;
         exit when This.Count = 0;
      end loop;
   end Read;

   -------------------
   -- Reset_Reading --
   -------------------

   procedure Reset_Reading (This : access Memory_Resident_Stream) is
   begin
      This.Next_Out := 1;
   end Reset_Reading;

   -------------------
   -- Reset_Writing --
   -------------------

   procedure Reset_Writing (This : access Memory_Resident_Stream) is
   begin
      This.Next_In := 1;
   end Reset_Writing;

   -----------
   -- Empty --
   -----------

   function Empty (This : Memory_Resident_Stream) return Boolean is
   begin
      return This.Count = 0;
   end Empty;

   ------------
   -- Extent --
   ------------

   function Extent (This : Memory_Resident_Stream)
                    return Stream_Element_Count is
   begin
      return This.Count;
   end Extent;

end PolyORB.DSA_P.Streams;
