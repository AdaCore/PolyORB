------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A N Y . E X C E P T I O N L I S T             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

with PolyORB.Any;
with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);
with PolyORB.Types;

with Sequences.Unbounded;
pragma Elaborate (Sequences.Unbounded);

package PolyORB.Any.ExceptionList is

   pragma Elaborate_Body;

   type Ref is new PolyORB.Smart_Pointers.Ref with null record;
   Nil_Ref : constant Ref;

   type Object is new PolyORB.Smart_Pointers.Entity with private;
   type Object_Ptr is access all Object;

   procedure Finalize (Obj : in out Object);

   function Get_Count
     (Self : in Ref)
     return PolyORB.Types.Unsigned_Long;

   procedure Add
     (Self : in Ref;
      Exc : in PolyORB.Any.TypeCode.Object);

   function Item
     (Self : in Ref;
      Index : in PolyORB.Types.Unsigned_Long)
     return PolyORB.Any.TypeCode.Object;

   procedure Remove
     (Self : in Ref;
      Index : in PolyORB.Types.Unsigned_Long);

   procedure Create_List (Self : out Ref);

   function Search_Exception_Id
     (Self : in Ref;
      Name : in PolyORB.Types.String)
     return PolyORB.Types.Unsigned_Long;

private

   --  The actual implementation of an ExceptionList:
   --  a list of TypeCode

   package Exception_Sequences is new Sequences.Unbounded
     (PolyORB.Any.TypeCode.Object);

   type Object is new PolyORB.Smart_Pointers.Entity with record
      List : Exception_Sequences.Sequence
        := Exception_Sequences.Null_Sequence;
   end record;

   Nil_Ref : constant Ref
     := (PolyORB.Smart_Pointers.Ref with null record);

end PolyORB.Any.ExceptionList;
