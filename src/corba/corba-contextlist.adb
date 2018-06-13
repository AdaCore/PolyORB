------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    C O R B A . C O N T E X T L I S T                     --
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

package body CORBA.ContextList is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Obj : in out Object) is
   begin
      Context_Sequence.Delete
        (Obj.List,
         1,
         Context_Sequence.Length (Obj.List));
   end Finalize;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long (Context_Sequence.Length
                                  (Object_Ptr (Object_Of (Self)).List));
   end Get_Count;

   ---------
   -- Add --
   ---------

   procedure Add (Self : Ref; Exc : CORBA.String) is
   begin
      Context_Sequence.Append (Object_Ptr (Object_Of (Self)).List, Exc);
   end Add;

   ----------
   -- Item --
   ----------

   function Item
     (Self  : Ref;
      Index : CORBA.Unsigned_Long)
     return CORBA.String
   is
   begin
      return Context_Sequence.Get_Element
        (Object_Ptr (Object_Of (Self)).List, Positive (Index));
   end Item;

   ------------
   -- Remove --
   ------------

   procedure Remove (Self : Ref; Index : CORBA.Unsigned_Long) is
   begin
      Context_Sequence.Delete
        (Object_Ptr (Object_Of (Self)).List, Positive (Index), 1);
   end Remove;

   -------------------
   -- Create_Object --
   -------------------

   function Create_Object return Object_Ptr is
      Actual_Ref : constant CORBA.ContextList.Object_Ptr := new Object;

   begin
      Actual_Ref.List := Context_Sequence.Null_Sequence;
      return Actual_Ref;
   end Create_Object;

end CORBA.ContextList;
