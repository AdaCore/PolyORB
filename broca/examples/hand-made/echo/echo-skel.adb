with Echo.Stream;
with CORBA;
with Broca.CDR;
with Broca.GIOP;
with Broca.Exceptions;
use Echo.Stream;
use CORBA;
use Broca.CDR;
use Broca.GIOP;
use Broca.Exceptions;
package body Echo.Skel is

   type Object_Ptr is access all Object'Class;

   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:Echo:1.0");
   end Get_Type_Id;

   procedure GIOP_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Request_Buffer : access Broca.Buffers.Buffer_Type;
      Reply_Buffer   : access Broca.Buffers.Buffer_Type)
   is
      use Broca.CDR;
      use Broca.Buffers;
   begin
      if Operation = "echoString" then
         declare
            IDL_Mesg : CORBA.String;
            Returns : CORBA.String;
         begin
            --  Unmarshalls arguments
            IDL_Mesg := Unmarshall (Request_Buffer);

            --  Call implementation
            Returns := echoString (Object_Ptr (Obj), IDL_Mesg);

            --  service context
            Marshall (Reply_Buffer,
                      CORBA.Unsigned_Long (Broca.GIOP.No_Context));
            --  request id
            Marshall (Reply_Buffer,
                      Request_Id);
            --  reply status
            Broca.GIOP.Marshall (Reply_Buffer,
                                 Broca.GIOP.No_Exception);
            --  return value
            Marshall (Reply_Buffer, Returns);

            return;
         end;
      end if;

      Broca.Exceptions.Raise_Bad_Operation;
   end GIOP_Dispatch;

end Echo.Skel;
