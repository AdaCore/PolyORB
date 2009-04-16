------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . D S A _ P . S T R E A M S                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008, Free Software Foundation, Inc.               --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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
