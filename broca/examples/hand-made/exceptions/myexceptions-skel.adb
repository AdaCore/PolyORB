with myexceptions.Stream;
with CORBA;
with Broca.Marshalling;
with Broca.Refs;
with Broca.Giop;
with Broca.Exceptions;
use myexceptions.Stream;
use CORBA;
use Broca.Marshalling;
use Broca.Refs;
use Broca.Giop;
use Broca.Exceptions;
package body myexceptions.Skel is

   type Object_Ptr is access all Object'Class;

   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:myexceptions:1.0");
   end Get_Type_Id;

   procedure Giop_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Stream : in out Broca.Buffers.Buffer_Descriptor)
   is
      use Broca.Marshalling;
      use Broca.Refs;
      use Broca.Buffers;
   begin
      Broca.Exceptions.Raise_Bad_Operation;
   end Giop_Dispatch;

end myexceptions.Skel;
