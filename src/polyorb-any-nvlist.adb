------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . N V L I S T                    --
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

--  $Id$

with Ada.Strings.Unbounded;

with PolyORB.Log;

package body PolyORB.Any.NVList is

   use PolyORB.Log;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any.nvlist");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : in Identifier;
      Item       : in Any;
      Item_Flags : in Flags)
   is
   begin
      pragma Debug (O ("Add_Item (4 params) : enter"));
      --  pragma Debug (O ("Add_Item (4 params) : Item type is "
      --    & Ada.Tags.External_Tag (Get_Value (Item).all'Tag)));
      --  Item may be an empty any, in which case Get_Value (Item) is
      --  a null pointer.
      pragma Debug (O ("Add_Item (4 params) : ref_counter = "
                       & Positive'Image (Get_Counter (Item))));

      Add_Item (Self, (Name => Item_Name,
                       Argument => Item,
                       Arg_Modes => Item_Flags));

      pragma Debug (O ("Add_Item (4 params) : ref_counter = "
                       & Positive'Image (Get_Counter (Item))));
      pragma Debug (O ("Add_Item (4 params) : end"));
   end Add_Item;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self : Ref;
      Item : NamedValue)
   is
      Obj : Object_Ptr := Object_Ptr (Entity_Of (Self));
   begin
      pragma Debug (O ("Add_Item (2 params) : enter"));
      pragma Debug (O ("Add_Item (2 params) : ref_counter = "
                       & Positive'Image (Item.Argument.Ref_Counter.all)));
      NV_Sequence.Append (Obj.List, Item);
      pragma Debug (O ("Add_Item (2 params) : ref_counter = "
                       & Positive'Image (Item.Argument.Ref_Counter.all)));
      pragma Debug (O ("Add_Item (2 params) : end"));
   end Add_Item;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Ref) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      null;
   end Free;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return Types.Long
   is
   begin
      if Is_Null (Self) then
         return 0;
      else
         declare
            Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));
         begin
            return Types.Long (NV_Sequence.Length (Obj.List));
         end;
      end if;
   end Get_Count;

   ------------
   -- Create --
   ------------

   procedure Create (NVList : out Ref) is
   begin
      Set (NVList, PolyORB.Smart_Pointers.Entity_Ptr'(new Object));
   end Create;

   function Image (NVList : Ref) return Standard.String
   is
      use NV_Sequence;

      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (NVList));
      Result : Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Obj /= null then
         declare
            NVs : constant Element_Array := To_Element_Array (Obj.List);
         begin
            for I in NVs'Range loop
               Ada.Strings.Unbounded.Append (Result, Image (NVs (I)));
               if I /= NVs'Last then
                  Ada.Strings.Unbounded.Append (Result, ' ');
               end if;
            end loop;

            return Ada.Strings.Unbounded.To_String (Result);
         end;
      else
         return ("(null list)");
      end if;
   end Image;

   package body Internals is

      function List_Of (NVList : Ref) return NV_Sequence_Access
      is
         use type PolyORB.Smart_Pointers.Entity_Ptr;
         Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
            := Entity_Of (NVList);
      begin
         if Entity /= null then
            return Object_Ptr (Entity_Of (NVList)).List'Access;
         end if;
         return null;
      end List_Of;

   end Internals;

end PolyORB.Any.NVList;
