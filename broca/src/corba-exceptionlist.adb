------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                  C O R B A . E X C E P T I O N L I S T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Sequences.Unbounded.Search;

with Broca.Debug;

package body CORBA.ExceptionList is

   -------------
   --  Debug  --
   -------------
   Flag : constant Natural
     := Broca.Debug.Is_Active ("corba.exceptionlist");
   procedure O is new Broca.Debug.Output (Flag);

   ----------------
   --  Finalize  --
   ----------------
   procedure Finalize (Obj : in out Object) is
   begin
      Exception_Sequence.Delete (Obj.List,
                                 1,
                                 Exception_Sequence.Length (Obj.List));
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
      Exc : in CORBA.TypeCode.Object) is
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
      return CORBA.TypeCode.Object is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      return Exception_Sequence.Element_Of (Obj.List, Positive (Index));
   end Item;

   --------------
   --  Remove  --
   --------------
   procedure Remove
     (Self : in Ref;
      Index : in CORBA.Unsigned_Long) is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
   begin
      Exception_Sequence.Delete (Obj.List, Positive (Index), 1);
   end Remove;

   ---------------------
   --  Create_Object  --
   ---------------------
   function Create_Object return Object_Ptr
   is
      Actual_Ref : constant CORBA.ExceptionList.Object_Ptr
        := new Object;
   begin
      Actual_Ref.List := Exception_Sequence.Null_Sequence;
      return Actual_Ref;
   end Create_Object;

   ---------------------------
   --  Search_Exception_Id  --
   ---------------------------
   function Search_Exception_Id
     (Self : in Ref;
      Name : in CORBA.RepositoryId)
     return CORBA.Unsigned_Long is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));
      function Match (Item : TypeCode.Object;
                      Needle : CORBA.RepositoryId)
                      return Boolean;
      function Match (Item : TypeCode.Object;
                      Needle : CORBA.RepositoryId)
                      return Boolean is
      begin
         pragma Debug (O ("Match : Id (Item) = """ &
                          CORBA.To_Standard_String (CORBA.TypeCode.Id (Item)) &
                          """ and Needle = """ &
                          CORBA.To_Standard_String (Needle) &
                          """"));
         return CORBA.TypeCode.Id (Item) = Needle;
      end Match;
      package Exception_Search is
        new Exception_Sequence.Search
        (CORBA.RepositoryId,
         Match);
   begin
      pragma Debug (O ("Search_Exception_Id : Obj.list length is " &
                       CORBA.Unsigned_Long'Image (Get_Count (Self))));
      pragma Debug (O ("Search_Exception_Id : Name = """ &
                       CORBA.To_Standard_String (Name) & """"));
      pragma Debug (O ("Search_Exception_Id : first excpt id = """ &
                       CORBA.To_Standard_String
                       (TypeCode.Id
                        (Exception_Sequence.Element_Of
                         (Obj.List, 1))) & """"));
      return CORBA.Unsigned_Long
        (Exception_Search.Index (Obj.List, Name));
   end Search_Exception_Id;

end CORBA.ExceptionList;
