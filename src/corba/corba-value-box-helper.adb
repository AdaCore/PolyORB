------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               C O R B A . V A L U E . B O X . H E L P E R                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2011-2012, Free Software Foundation, Inc.          --
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

with CORBA.Impl;
with PolyORB.Utils.Unchecked_Deallocation;

package body CORBA.Value.Box.Helper is

   use type PolyORB.Any.Content_Ptr;

   --  Global data

   Initialized : Boolean := False;
   Box_Ref_TC  : CORBA.TypeCode.Object;
   Element_TC  : CORBA.TypeCode.Object;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (ACC  : Box_Ref_Content;
      Into : PolyORB.Any.Content_Ptr := null) return PolyORB.Any.Content_Ptr
   is
   begin
      if Into /= null then
         if Into.all not in Box_Ref_Content then
            return null;
         end if;

         Box_Ref_Content (Into.all).V.all := ACC.V.all;

         return Into;
      else
         return
           new Box_Ref_Content'
           (PolyORB.Any.Aggregate_Content with V => new Box_Ref'(ACC.V.all));
      end if;
   end Clone;

   --------------------
   -- Finalize_Value --
   --------------------

   overriding procedure Finalize_Value (ACC : in out Box_Ref_Content) is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Box_Ref,
         Name => Box_Ref_Ptr);
   begin
      Free (ACC.V);
   end Finalize_Value;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : CORBA.Any) return Box_Ref is
      pragma Assert (Initialized);
      Result : Box_Ref;

   begin
      if CORBA.Internals.Get_Aggregate_Count (Item) = 1 then
         Result :=
           Create
           (Element_From_Any
            (CORBA.Internals.Get_Aggregate_Element (Item, Element_TC, 0)));
      end if;

      return Result;
   end From_Any;

   -------------------------
   -- Get_Aggregate_Count --
   -------------------------

   overriding function Get_Aggregate_Count
     (ACC : Box_Ref_Content) return PolyORB.Types.Unsigned_Long is
   begin
      if Is_Null (ACC.V.all) then
         return 0;

      else
         return 1;
      end if;
   end Get_Aggregate_Count;

   ---------------------------
   -- Get_Aggregate_Element --
   ---------------------------

   overriding function Get_Aggregate_Element
     (ACC   : not null access Box_Ref_Content;
      TC    : PolyORB.Any.TypeCode.Object_Ptr;
      Index : PolyORB.Types.Unsigned_Long;
      Mech  : not null access PolyORB.Any.Mechanism)
      return PolyORB.Any.Content'Class
   is
      pragma Unreferenced (TC);

      use type PolyORB.Types.Unsigned_Long;

      pragma Assert (Index = 0);

   begin
      Mech.all := PolyORB.Any.By_Reference;
      return Element_Wrap (Contents (ACC.V.all));
   end Get_Aggregate_Element;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Element_TC, Box_Ref_TC : CORBA.TypeCode.Object) is
   begin
      Helper.Element_TC := Element_TC;
      Helper.Box_Ref_TC := Box_Ref_TC;
      Initialized := True;
   end Initialize;

   -------------------------
   -- Set_Aggregate_Count --
   -------------------------

   overriding procedure Set_Aggregate_Count
     (ACC   : in out Box_Ref_Content;
      Count : PolyORB.Types.Unsigned_Long)
   is
      use type PolyORB.Types.Unsigned_Long;

   begin
      if Count = 0 then
         if not Is_Nil (ACC.V.all) then
            Release (ACC.V.all);
         end if;

      else
         declare
            pragma Assert (Count = 1);
            Ptr : constant Object_Ptr := new Object;

         begin
            Ptr.Content := new Boxed;
            Set (ACC.V.all, CORBA.Impl.Object_Ptr (Ptr));
         end;
      end if;
   end Set_Aggregate_Count;

--   ---------------------------
--   -- Set_Aggregate_Element --
--   ---------------------------
--
--   procedure Set_Aggregate_Element
--     (ACC    : in out Box_Ref_Content;
--      TC     : TypeCode.Object;
--      Index  : Types.Unsigned_Long;
--      From_C : in out Any_Container'Class)
--   is
--   begin
--      --  For a valuebox aggregate, item is by reference
--
--      raise Program_Error;
--   end Set_Aggregate_Element;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Box_Ref) return CORBA.Any is
      pragma Assert (Initialized);
      Result : CORBA.Any
        := CORBA.Internals.Get_Empty_Any_Aggregate (Box_Ref_TC);

   begin
      if not Is_Null (Item) then
         CORBA.Internals.Add_Aggregate_Element
           (Result, Element_To_Any (Contents (Item).all));
      end if;

      return Result;
   end To_Any;

   ----------
   -- Wrap --
   ----------

   function Wrap (X : access Box_Ref) return PolyORB.Any.Content'Class is
   begin
      return Box_Ref_Content'(PolyORB.Any.Aggregate_Content with
                               V => Box_Ref_Ptr (X));
   end Wrap;

end CORBA.Value.Box.Helper;
