------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A N Y . E X C E P T I O N L I S T             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Any;
with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);
with PolyORB.Types;

with PolyORB.Utils.Chained_Lists;

package PolyORB.Any.ExceptionList is

   pragma Elaborate_Body;

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   Nil_Ref : constant Ref;

   function Get_Count (Self : Ref) return PolyORB.Types.Unsigned_Long;

   procedure Add (Self : Ref; Exc : TypeCode.Local_Ref);

   function Item
     (Self  : Ref;
      Index : Types.Unsigned_Long) return TypeCode.Local_Ref;

   procedure Remove
     (Self  : Ref;
      Index : PolyORB.Types.Unsigned_Long);

   procedure Create_List (Self : out Ref);

   function Search_Exception_Id
     (Self : Ref;
      Name : Types.String) return Types.Unsigned_Long;

private

   use PolyORB.Any.TypeCode;

   Nil_Ref : constant Ref := (PolyORB.Smart_Pointers.Ref with null record);

   --  The actual implementation of an ExceptionList: a list of TypeCodes

   package Exception_Lists is new PolyORB.Utils.Chained_Lists
     (PolyORB.Any.TypeCode.Local_Ref, Doubly_Chained => True);

   type Object is new PolyORB.Smart_Pointers.Non_Controlled_Entity with record
      List : Exception_Lists.List;
   end record;
   type Object_Ptr is access all Object;

end PolyORB.Any.ExceptionList;
