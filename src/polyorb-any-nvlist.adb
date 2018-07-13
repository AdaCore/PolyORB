------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . A N Y . N V L I S T                    --
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

pragma Ada_2012;

with PolyORB.Log;

package body PolyORB.Any.NVList is

   use PolyORB.Log;
   use PolyORB.Types;
   use Internals;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any.nvlist");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self       : Ref;
      Item_Name  : Identifier;
      Item       : Any;
      Item_Flags : Flags) is
   begin
      pragma Debug (C, O ("Add_Item (4 params) : enter"));

      Add_Item (Self, (Name      => Item_Name,
                       Argument  => Item,
                       Arg_Modes => Item_Flags));

      pragma Debug (C, O ("Add_Item (4 params) : end"));
   end Add_Item;

   procedure Add_Item
     (Self : Ref;
      Item : NamedValue)
   is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));
   begin
      pragma Debug (C, O ("Add_Item (2 params) : enter"));
      NV_Lists.Append (Obj.List, Item);
      pragma Debug (C, O ("Add_Item (2 params) : end"));
   end Add_Item;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (X : in out Object) is
   begin
      Internals.NV_Lists.Deallocate (X.List);
   end Finalize;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return Types.Long is
   begin
      if Is_Null (Self) then
         return 0;
      else
         return Types.Long (NV_Lists.Length (List_Of (Self).all));
      end if;
   end Get_Count;

   ------------
   -- Create --
   ------------

   procedure Create (NVList : out Ref) is
   begin
      Set (NVList, PolyORB.Smart_Pointers.Entity_Ptr'(new Object));
   end Create;

   -----------
   -- Image --
   -----------

   function Image (NVList : Ref) return Standard.String
   is
      use NV_Lists;

      Obj    : constant Object_Ptr := Object_Ptr (Entity_Of (NVList));
      Result : PolyORB.Types.String := To_PolyORB_String ("");
   begin
      if Obj /= null then
         declare
            It : Iterator := First (Obj.List);
         begin
            while not Last (It) loop
               Result := Result & Image (Value (It).all);
               Next (It);
               if not Last (It) then
                  Result := Result & " ";
               end if;
            end loop;

            return PolyORB.Types.To_Standard_String (Result);
         end;
      else
         return ("(null list)");
      end if;
   end Image;

   --------------------------------
   -- Package body for Internals --
   --------------------------------

   package body Internals is

      -------------
      -- List_Of --
      -------------

      function List_Of (NVList : Ref) return NV_List_Access
      is
         use type PolyORB.Smart_Pointers.Entity_Ptr;
         Entity : constant PolyORB.Smart_Pointers.Entity_Ptr :=
           Entity_Of (NVList);
      begin
         if Entity /= null then
            return Object_Ptr (Entity_Of (NVList)).List'Access;
         end if;
         return null;
      end List_Of;

   end Internals;

end PolyORB.Any.NVList;
