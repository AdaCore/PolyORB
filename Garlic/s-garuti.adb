------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . U T I L S                    --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body System.Garlic.Utils is

   use Ada.Streams;

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

   -------------
   -- Barrier --
   -------------

   protected body Barrier is

      ------------
      -- Signal --
      ------------

      procedure Signal (How_Many : Positive := 1) is
      begin
         if not Perm then
            Free := Free + How_Many;
         end if;
      end Signal;

      ----------------
      -- Signal_All --
      ----------------

      procedure Signal_All (Permanent : Boolean) is
      begin
         if not Perm then
            if Permanent then
               Perm := True;
            else
               Free := Free + Wait'Count;
            end if;
         end if;
      end Signal_All;

      ----------
      -- Wait --
      ----------

      entry Wait when Perm or else Free > 0 is
      begin
         if not Perm then
            Free := Free - 1;
         end if;
      end Wait;

   end Barrier;

   ---------------------------
   -- To_Params_Stream_Type --
   ---------------------------

   procedure To_Params_Stream_Type
     (Content : Stream_Element_Array;
      Params  : access System.RPC.Params_Stream_Type)
   is
   begin
      System.RPC.Write (Params.all, Content);
   end To_Params_Stream_Type;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Params : access System.RPC.Params_Stream_Type;
      Unused : Ada.Streams.Stream_Element_Count := 0)
      return Stream_Element_Array
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
         Result : Stream_Element_Array (1 .. Total + Unused);
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
   end To_Stream_Element_Array;

end System.Garlic.Utils;
