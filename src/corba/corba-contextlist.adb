------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . C O N T E X T L I S T                     --
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

package body CORBA.ContextList is

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (Obj : in out Object) is
   begin
      Context_Sequence.Delete (Obj.List,
                               1,
                               Context_Sequence.Length (Obj.List));
   end Finalize;

   -----------------
   --  Get_Count  --
   -----------------
   function Get_Count
     (Self : in Ref)
      return CORBA.Unsigned_Long is
      Obj : constant Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return CORBA.Unsigned_Long (Context_Sequence.Length (Obj.List));
   end Get_Count;

   -----------
   --  Add  --
   -----------
   procedure Add
     (Self : in Ref;
      Exc : in CORBA.String) is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Context_Sequence.Append (Obj.List, Exc);
   end Add;

   ----------
   -- Item --
   ----------
   function Item
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
      return CORBA.String is
      Obj : constant Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return Context_Sequence.Element_Of (Obj.List, Positive (Index));
   end Item;

   --------------
   --  Remove  --
   --------------
   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long) is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Context_Sequence.Delete (Obj.List, Positive (Index), 1);
   end Remove;

   -------------------
   -- Create_Object --
   -------------------
   function Create_Object return Object_Ptr
   is
      Actual_Ref : constant CORBA.ContextList.Object_Ptr
        := new Object;
   begin
      Actual_Ref.List := Context_Sequence.Null_Sequence;
      return Actual_Ref;
   end Create_Object;

end CORBA.ContextList;
