------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A N Y . E X C E P T I O N L I S T             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

with PolyORB.Log;

package body PolyORB.Any.ExceptionList is

   use PolyORB.Log;
   use PolyORB.Types;
   use Exception_Lists;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any.exceptionlist");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return PolyORB.Types.Unsigned_Long is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      if Obj = null then
         return 0;
      end if;

      return PolyORB.Types.Unsigned_Long (Exception_Lists.Length (Obj.List));
   end Get_Count;

   ---------
   -- Add --
   ---------

   procedure Add (Self : Ref; Exc : PolyORB.Any.TypeCode.Local_Ref) is
   begin
      Exception_Lists.Append (Object_Ptr (Entity_Of (Self)).List, Exc);
   end Add;

   ----------
   -- Item --
   ----------

   function Item
     (Self  : Ref;
      Index : PolyORB.Types.Unsigned_Long) return TypeCode.Local_Ref
   is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));
      It : Iterator := First (Obj.List);
      Counter : PolyORB.Types.Unsigned_Long := 1;

   begin
      while not Last (It) loop
         exit when Counter = Index;

         Counter := Counter + 1;
         Next (It);
      end loop;

      return Value (It).all;
   end Item;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self  : Ref;
      Index : PolyORB.Types.Unsigned_Long)
   is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));
      It : Iterator := First (Obj.List);
      Counter : PolyORB.Types.Unsigned_Long := 1;

   begin
      while not Last (It) loop
         exit when Counter = Index;

         Counter := Counter + 1;
         Next (It);
      end loop;

      Remove (Obj.List, It);
   end Remove;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List (Self : out Ref) is
   begin
      Set (Self, PolyORB.Smart_Pointers.Entity_Ptr'(new Object));
   end Create_List;

   -------------------------
   -- Search_Exception_Id --
   -------------------------

   function Search_Exception_Id
     (Self : Ref;
      Name : Types.String) return Types.Unsigned_Long
   is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      pragma Debug (O ("Search_Exception_Id : Obj.list length is " &
                       PolyORB.Types.Unsigned_Long'Image (Get_Count (Self))));
      pragma Debug (O ("Search_Exception_Id : Name = """ &
                         To_Standard_String (Name) & """"));

      if Obj = null then
         pragma Debug (O ("Search_Exception_Id: null list."));
         return 0;
      end if;

      declare
         It : Iterator := First (Obj.List);
         Counter : PolyORB.Types.Unsigned_Long := 1;

      begin
         while not Last (It) loop
            exit when PolyORB.Any.TypeCode.Id (Value (It).all)
              =  RepositoryId (Name);

            Counter := Counter + 1;
            Next (It);
         end loop;

         return Counter;
      end;
   end Search_Exception_Id;

end PolyORB.Any.ExceptionList;
