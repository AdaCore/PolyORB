with CORBA.Object;
with Broca.Exceptions;
with Broca.Refs;
with Broca.Repository;
with CORBA;
with Echo.Stream;
with Broca.GIOP;
with Broca.Object;
with Broca.CDR;
use CORBA.Object;
use Broca.Exceptions;
use Broca.Refs;
use Broca.Repository;
use CORBA;
use Echo.Stream;
use Broca.GIOP;
use Broca.Object;
use Broca.CDR;
pragma Elaborate_All (Broca.Repository);

package body Echo is

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

   echoString_Operation : constant CORBA.Identifier :=
     CORBA.To_CORBA_String ("echoString");

   function echoString
     (Self : in Ref;
      Mesg : in CORBA.String)
      return CORBA.String
   is
      Returns : CORBA.String;
      use Broca.CDR;
      use Broca.Refs;
      Handler : Broca.GIOP.Request_Handler;
      Sr_Res : Broca.GIOP.Send_Request_Result_Type;
   begin
      loop
         Broca.GIOP.Send_Request_Marshall
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, echoString_Operation);
         Marshall (Handler.Buffer'Access, Mesg);
         Broca.GIOP.Send_Request_Send
           (Handler, Broca.Object.Object_Ptr (Get (Self)),
            True, Sr_Res);
         case Sr_Res is
            when Broca.GIOP.Sr_Reply =>
               --  Inout and out parameters.
               Returns := Unmarshall (Handler.Buffer'Access);
               return Returns;
            when Broca.GIOP.Sr_No_Reply =>
               raise Program_Error;
            when Broca.GIOP.Sr_User_Exception =>
               raise Program_Error;
            when Broca.GIOP.Sr_Forward =>
               null;
         end case;
      end loop;
   end echoString;

   function Is_A
     (Self   : in Ref;
      Logical_Type_Id : in CORBA.String)
      return CORBA.Boolean
   is
      Type_Id : String := CORBA.To_Standard_String (Logical_Type_Id);
   begin
      if Type_Id = "IDL:Echo:1.0" or else
        Type_Id = "IDL:omg.org/CORBA/OBJECT:1.0" then
         return True;
      else
         return False;
      end if;
   end Is_A;

   type Echo_Factory_Type is new Broca.Repository.Factory_Type
      with null record;
   function Create (Factory : access Echo_Factory_Type)
                    return CORBA.Object.Ref'Class;

   function Create (Factory : access Echo_Factory_Type)
                    return CORBA.Object.Ref'Class is
      Res : Ref;
   begin
      Broca.Refs.Set (Broca.Refs.Ref (Res),
                      new Broca.Object.Object_Type);
      return Res;
   end Create;

   Echo_Factory : constant Broca.Repository.Factory_Ptr :=
      new Echo_Factory_Type'
        (Next => null, Type_Id => CORBA.RepositoryId (Repository_Id));
begin
   Broca.Repository.Register (Echo_Factory);
end Echo;
