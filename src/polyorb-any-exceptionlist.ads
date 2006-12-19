------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A N Y . E X C E P T I O N L I S T             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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

   procedure Add (Self : Ref; Exc : PolyORB.Any.TypeCode.Object);

   function Item
     (Self  : Ref;
      Index : PolyORB.Types.Unsigned_Long)
     return PolyORB.Any.TypeCode.Object;

   procedure Remove
     (Self  : Ref;
      Index : PolyORB.Types.Unsigned_Long);

   procedure Create_List (Self : out Ref);

   function Search_Exception_Id
     (Self : Ref;
      Name : PolyORB.Types.String)
     return PolyORB.Types.Unsigned_Long;

private

   use PolyORB.Any.TypeCode;

   Nil_Ref : constant Ref := (PolyORB.Smart_Pointers.Ref with null record);

   --  The actual implementation of an ExceptionList:
   --  a list of TypeCode

   package Exception_Lists is new PolyORB.Utils.Chained_Lists
     (PolyORB.Any.TypeCode.Object, Doubly_Chained => True);

   type Object is new PolyORB.Smart_Pointers.Non_Controlled_Entity with record
      List : Exception_Lists.List;
   end record;
   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);

end PolyORB.Any.ExceptionList;
