--  $Id$

with Sequences.Unbounded.Search;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body CORBA.ExceptionList is

   -----------
   -- Debug --
   -----------

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("corba.exceptionlist");
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
     return CORBA.Unsigned_Long
   is
      Obj : Object_Ptr := Object_Ptr (Object_Of (Self));

      use Droopi.Types;

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
               CORBA.To_Standard_String (CORBA.TypeCode.Id (Item)) &
               """ and Needle = """ &
               CORBA.To_Standard_String (Needle) &
               """"));
         return CORBA.TypeCode.Id (Item) = Needle;
      end Match;

      package Exception_Search is new Exception_Sequence.Search
        (CORBA.RepositoryId, Match);

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
