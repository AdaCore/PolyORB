------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         C O R B A . N V L I S T                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

package body CORBA.NVList is

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Self       :    Ref;
      Item_Name  : Identifier;
      Item       : CORBA.Any;
      Item_Flags : Flags)
   is
   begin
      PolyORB.Any.NVList.Add_Item
        (Internals.To_PolyORB_Ref (Self),
         PolyORB.Types.Identifier (Item_Name),
         CORBA.Internals.To_PolyORB_Any (Item),
         PolyORB.Any.Flags (Item_Flags));
   end Add_Item;

   procedure Add_Item
     (Self :    Ref;
      Item : CORBA.NamedValue)
   is
   begin
      Add_Item (Self, Item.Name, Item.Argument, Item.Arg_Modes);
   end Add_Item;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return CORBA.Long is
   begin
      return CORBA.Long
        (PolyORB.Any.NVList.Get_Count
         (Internals.To_PolyORB_Ref (Self)));
   end Get_Count;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Ref) is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      null;
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Ref) is
      Res : PolyORB.Any.NVList.Ref;

   begin
      CORBA.AbstractBase.Initialize (CORBA.AbstractBase.Ref (Self));

      PolyORB.Any.NVList.Create (Res);
      Set (Self, PolyORB.Any.NVList.Entity_Of (Res));
   end Initialize;

   package body Internals is

      --------------------
      -- Clone_Out_Args --
      --------------------

      procedure Clone_Out_Args (Self : Ref) is
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Lists;

         It : Iterator;
      begin
         It := First (List_Of (To_PolyORB_Ref (Self)).all);
         while not Last (It) loop
            declare
               use PolyORB.Any;
               NV : PolyORB.Any.NamedValue renames Value (It).all;
            begin
               if        NV.Arg_Modes = PolyORB.Any.ARG_OUT
                 or else NV.Arg_Modes = PolyORB.Any.ARG_INOUT
               then
                  NV.Argument := Copy_Any (NV.Argument);
               end if;
            end;
            Next (It);
         end loop;
      end Clone_Out_Args;

      ----------
      -- Item --
      ----------

      function Item (Self : Ref; Index : CORBA.Long) return CORBA.NamedValue is
         use PolyORB.Any.NVList.Internals;
         use PolyORB.Any.NVList.Internals.NV_Lists;
      begin
         return
           To_CORBA_NV
           (Element
            (List_Of (To_PolyORB_Ref (Self)).all, Integer (Index)).all);
      end Item;

      ------------------
      -- To_CORBA_Ref --
      ------------------

      function To_CORBA_Ref (Self : PolyORB.Any.NVList.Ref) return Ref is
         Res : Ref;

      begin
         Set (Res, PolyORB.Any.NVList.Entity_Of (Self));
         return Res;
      end To_CORBA_Ref;

      --------------------
      -- To_PolyORB_Ref --
      --------------------

      function To_PolyORB_Ref (Self : Ref) return PolyORB.Any.NVList.Ref is
         Res : PolyORB.Any.NVList.Ref;

      begin
         PolyORB.Any.NVList.Set (Res, Entity_Of (Self));
         return Res;
      end To_PolyORB_Ref;

   end Internals;

end CORBA.NVList;
