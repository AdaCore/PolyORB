------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . E X C E P T I O N L I S T                   --
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

package body CORBA.ExceptionList is

   package body Internals is

      --------------------
      -- To_PolyORB_Ref --
      --------------------

      function To_PolyORB_Ref
        (Self : Ref) return PolyORB.Any.ExceptionList.Ref
      is
         Result : PolyORB.Any.ExceptionList.Ref;

      begin
         PolyORB.Any.ExceptionList.Set (Result, Entity_Of (Self));
         return Result;
      end To_PolyORB_Ref;

      ------------------
      -- To_CORBA_Ref --
      ------------------

      function To_CORBA_Ref
        (Self : PolyORB.Any.ExceptionList.Ref)
        return Ref
      is
         Result : Ref;

      begin
         Set (Result, PolyORB.Any.ExceptionList.Entity_Of (Self));
         return Result;
      end To_CORBA_Ref;

   end Internals;

   ---------
   -- "+" --
   ---------

   function "+" (Self : Ref) return PolyORB.Any.ExceptionList.Ref
     renames Internals.To_PolyORB_Ref;

   function "+" (Self : PolyORB.Any.ExceptionList.Ref) return Ref
     renames Internals.To_CORBA_Ref;

   use PolyORB.Any.ExceptionList;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count (Self : Ref) return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long (Get_Count (+Self));
   end Get_Count;

   ---------
   -- Add --
   ---------

   procedure Add (Self : Ref; Exc : CORBA.TypeCode.Object) is
   begin
      Add (+Self, CORBA.TypeCode.Internals.To_PolyORB_Object (Exc));
   end Add;

   ----------
   -- Item --
   ----------

   function Item
     (Self  : Ref;
      Index : CORBA.Unsigned_Long) return CORBA.TypeCode.Object
   is
   begin
      return CORBA.TypeCode.Internals.To_CORBA_Object
        (Item (+Self, PolyORB.Types.Unsigned_Long (Index)));
   end Item;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self  : Ref;
      Index : CORBA.Unsigned_Long)
   is
   begin
      Remove (+Self, PolyORB.Types.Unsigned_Long (Index));
   end Remove;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List (Self : out Ref) is
      Result : PolyORB.Any.ExceptionList.Ref;

   begin
      Create_List (Result);
      Self := +Result;
   end Create_List;

   -------------------------
   -- Search_Exception_Id --
   -------------------------

   function Search_Exception_Id
     (Self : Ref;
      Name : CORBA.RepositoryId)
     return CORBA.Unsigned_Long
   is
   begin
      return CORBA.Unsigned_Long
        (Search_Exception_Id (+Self, PolyORB.Types.String (Name)));
   end Search_Exception_Id;

end CORBA.ExceptionList;
