with Ada.Exceptions;
with CORBA.Object;
with Broca.Exceptions;
with Broca.Refs;
with Broca.Repository;
use Ada.Exceptions;
use CORBA.Object;
use Broca.Exceptions;
use Broca.Refs;
use Broca.Repository;
pragma Elaborate_All (Broca.Repository);

package body myexceptions is

   function Unchecked_To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref
   is
      Result : Ref;
   begin
      Broca.Refs.Set (Broca.Refs.Ref (Result),
                      Broca.Refs.Get (Broca.Refs.Ref (Self)));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (Self : in CORBA.Object.Ref'Class)
      return Ref
   is
      Res : Ref;
   begin
      Res := Unchecked_To_Ref (Self);
      if Is_A (Res, Repository_Id) then
         return Res;
      else
         Broca.Exceptions.Raise_Bad_Param;
      end if;
   end To_Ref;

   procedure Get_Members
     (From : in Ada.Exceptions.Exception_Occurrence;
      To   : out toto_Members) is
   begin
      Broca.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   function Is_A
     (Self   : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean
   is
      Type_Id : String := CORBA.To_Standard_String (Logical_Type_Id);
   begin
      if Type_Id = "IDL:myexceptions:1.0" or else
        Type_Id = "IDL:omg.org/CORBA/OBJECT:1.0" then
         return True;
      else
         return False;
      end if;
   end Is_A;

   type myexceptions_Factory_Type is new Broca.Repository.Factory_Type
      with null record;
   function Create (Factory : access myexceptions_Factory_Type)
                    return CORBA.Object.Ref'Class;

   function Create (Factory : access myexceptions_Factory_Type)
                    return CORBA.Object.Ref'Class is
      Res : Ref;
   begin
      Broca.Refs.Set (Broca.Refs.Ref (Res),
                      new Broca.Object.Object_Type);
      return Res;
   end Create;

   myexceptions_Factory : constant Broca.Repository.Factory_Ptr :=
      new myexceptions_Factory_Type'
        (Next => null, Type_Id => CORBA.RepositoryId (Repository_Id));
begin
   Broca.Repository.Register (myexceptions_Factory);
end myexceptions;
