------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . S T R E A M S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
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

with Ada.Streams;         use Ada.Streams;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.RPC;          use System.RPC;

package body System.Garlic.Streams is

   Hex : constant String (1 .. 16) := "0123456789ABCDEF";
   Nil : constant String (1 .. 48) := (others => ' ');

   Node_Size : constant Stream_Element_Count := 4096;

   type Node;
   type Node_Ptr is access all Node;

   type Node is record
      Content : Stream_Element_Array (1 .. Node_Size);
      Last    : Stream_Element_Offset;
      Next    : Node_Ptr;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Level  : in System.Garlic.Debug.Debug_Level;
      Stream : in Ada.Streams.Stream_Element_Array;
      Key    : in System.Garlic.Debug.Debug_Key) is
      Index   : Natural := 1;
      Output  : String (1 .. 48);

   begin
      if Debug_Mode (Level, Key) then
         for I in Stream'Range loop
            Output (Index)     := ' ';
            Output (Index + 1) := Hex (Natural (Stream (I) / 16) + 1);
            Output (Index + 2) := Hex (Natural (Stream (I) mod 16) + 1);
            Index := Index + 3;

            if Index > Output'Length then
               Print_Debug_Info (Level, Output, Key);
               Index := 1;
               Output := Nil;
            end if;
         end loop;

         if Index /= 1 then
            Print_Debug_Info (Level, Output (1 .. Index - 1), Key);
         end if;
      end if;
   end Dump;

   ---------------------------
   -- To_Params_Stream_Type --
   ---------------------------

   procedure To_Params_Stream_Type
     (Content : Stream_Element_Array;
      Params  : access Params_Stream_Type)
   is
   begin
      System.RPC.Write (Params.all, Content);
   end To_Params_Stream_Type;

   ------------------------------
   -- To_Stream_Element_Access --
   ------------------------------

   function To_Stream_Element_Access
     (Params : access System.RPC.Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
     return Stream_Element_Access
   is
      First   : Node_Ptr := new Node;
      Current : Node_Ptr := First;
      Total   : Stream_Element_Count := 0;
   begin
      loop
         System.RPC.Read (Params.all, Current.Content, Current.Last);
         Total := Total + Current.Last;
         exit when Current.Last < Node_Size;
         Current.Next := new Node;
         Current := Current.Next;
      end loop;
      declare
         Result : constant Stream_Element_Access :=
           new Stream_Element_Array (1 .. Total + Unused);
         Index  : Stream_Element_Offset := 1 + Unused;
      begin
         Current := First;
         while Current /= null loop
            Result (Index .. Index + Current.Last - 1) :=
              Current.Content (1 .. Current.Last);
            Index := Index + Current.Last;
            First := Current.Next;
            Free (Current);
            Current := First;
         end loop;
         return Result;
      end;
   end To_Stream_Element_Access;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Params : access System.RPC.Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
      return Stream_Element_Array
   is
      Data   : Stream_Element_Access := To_Stream_Element_Access (Params);
      Result : constant Stream_Element_Array := Data.all;
   begin
      Free (Data);
      return Result;
   end To_Stream_Element_Array;

end System.Garlic.Streams;
