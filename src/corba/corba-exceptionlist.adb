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

with Sequences.Unbounded.Search;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Smart_Pointers;

package body CORBA.ExceptionList is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("corba.exceptionlist");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Obj : in out Object) is
   begin
      Exception_Sequence.Delete
        (Obj.List, 1, Exception_Sequence.Length (Obj.List));
   end Finalize;

   -----------------
   --  Get_Count  --
   -----------------
   function Get_Count
     (Self : in Ref)
      return CORBA.Unsigned_Long is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return CORBA.Unsigned_Long (Exception_Sequence.Length (Obj.List));
   end Get_Count;

   -----------
   --  Add  --
   -----------
   procedure Add
     (Self : in Ref;
      Exc : in CORBA.TypeCode.Object)
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Exception_Sequence.Append (Obj.List, Exc);
   end Add;

   ----------
   -- Item --
   ----------

   function Item
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
      return CORBA.TypeCode.Object
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return Exception_Sequence.Element_Of (Obj.List, Positive (Index));
   end Item;

   --------------
   --  Remove  --
   --------------
   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long)
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Exception_Sequence.Delete (Obj.List, Positive (Index), 1);
   end Remove;

   -------------------
   -- Create_Object --
   -------------------

   function Create_Object return Object_Ptr;

   function Create_Object return Object_Ptr
   is
      Result : constant CORBA.ExceptionList.Object_Ptr
        := new Object;
   begin
      Result.List := Exception_Sequence.Null_Sequence;
      return Result;
   end Create_Object;

   procedure Create_List (Self : out Ref) is
      Result : Ref;
   begin
      Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Create_Object));
      Self := Result;
   end Create_List;

   ---------------------------
   --  Search_Exception_Id  --
   ---------------------------
   function Search_Exception_Id
     (Self : in Ref;
      Name : in CORBA.RepositoryId)
     return CORBA.Unsigned_Long
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));

      use PolyORB.Types;

      function Match
        (Item : TypeCode.Object;
         Needle : CORBA.RepositoryId)
        return Boolean;

      function Match
        (Item : TypeCode.Object;
         Needle : CORBA.RepositoryId)
        return Boolean is
      begin
         pragma Debug
           (O ("Match : Id (Item) = """ &
               To_Standard_String (CORBA.TypeCode.Id (Item)) &
               """ and Needle = """ &
               To_Standard_String (Needle) &
               """"));
         return CORBA.TypeCode.Id (Item)
           = PolyORB.Types.RepositoryId (Needle);
      end Match;

      package Exception_Search is new Exception_Sequence.Search
        (CORBA.RepositoryId, Match);

   begin
      pragma Debug (O ("Search_Exception_Id : Obj.list length is " &
                       CORBA.Unsigned_Long'Image (Get_Count (Self))));
      pragma Debug (O ("Search_Exception_Id : Name = """ &
                       To_Standard_String (Name) & """"));
      pragma Debug (O ("Search_Exception_Id : first excpt id = """ &
                       To_Standard_String
                       (TypeCode.Id
                        (Exception_Sequence.Element_Of
                         (Obj.List, 1))) & """"));
      return CORBA.Unsigned_Long
        (Exception_Search.Index (Obj.List, Name));
   end Search_Exception_Id;

end CORBA.ExceptionList;
