------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . E X C E P T I O N L I S T                   --
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

package body CORBA.ExceptionList is

   function To_PolyORB_Ref
     (Self : Ref)
     return PolyORB.Any.ExceptionList.Ref
   is
      Result : PolyORB.Any.ExceptionList.Ref;
   begin
      PolyORB.Any.ExceptionList.Set (Result, Entity_Of (Self));
      return Result;
   end To_PolyORB_Ref;

   function To_CORBA_Ref
     (Self : PolyORB.Any.ExceptionList.Ref)
     return Ref
   is
      Result : Ref;
   begin
      Set (Result, PolyORB.Any.ExceptionList.Entity_Of (Self));
      return Result;
   end To_CORBA_Ref;

   ---------------
   -- Shortcuts --
   ---------------

   function "+" (Self : Ref) return PolyORB.Any.ExceptionList.Ref
     renames To_PolyORB_Ref;
   function "+" (Self : PolyORB.Any.ExceptionList.Ref) return Ref
     renames To_CORBA_Ref;

   use PolyORB.Any.ExceptionList;

   function Get_Count
     (Self : in Ref)
     return CORBA.Unsigned_Long is
   begin
      return CORBA.Unsigned_Long (Get_Count (+Self));
   end Get_Count;

   procedure Add
     (Self : in Ref;
      Exc : in CORBA.TypeCode.Object)
   is
   begin
      Add (+Self, Exc);
   end Add;

   function Item
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
   begin
      return Item (+Self, PolyORB.Types.Unsigned_Long (Index));
   end Item;

   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
   is
   begin
      Remove (+Self, PolyORB.Types.Unsigned_Long (Index));
   end Remove;

   procedure Create_List (Self : out Ref) is
      Result : PolyORB.Any.ExceptionList.Ref;
   begin
      Create_List (Result);
      Self := +Result;
   end Create_List;

   function Search_Exception_Id
     (Self : in Ref;
      Name : in CORBA.RepositoryId)
     return CORBA.Unsigned_Long
   is
   begin
      return CORBA.Unsigned_Long
        (Search_Exception_Id (+Self, PolyORB.Types.String (Name)));
   end Search_Exception_Id;

end CORBA.ExceptionList;
