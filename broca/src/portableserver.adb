with System.Address_To_Access_Conversions;
with Ada.Exceptions;
with CORBA;
with Broca.Exceptions;
with Broca.Refs;
with Broca.ORB;
with PortableServer.POA;

package body PortableServer is

   package Address_To_Ref_Ptr_Conversions is
     new System.Address_To_Access_Conversions (Broca.Refs.Ref_Type);
   use Address_To_Ref_Ptr_Conversions;

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id (Obj : Servant_Base) return CORBA.RepositoryId is
   begin
      return CORBA.To_CORBA_String ("IDL:omg.org/CORBA/OBJECT:1.0");
   end Get_Type_Id;

   -------------------
   -- GIOP_Dispatch --
   -------------------

   procedure GIOP_Dispatch
     (Obj : access Servant_Base;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Stream : in out Broca.Buffers.Buffer_Descriptor) is
   begin
      Broca.Exceptions.Raise_Bad_Operation;
   end GIOP_Dispatch;

   ---------------------
   -- Get_Default_POA --
   ---------------------

   function Get_Default_POA
     (For_Servant : Servant_Base)
     return POA_Forward.Ref is
   begin
      return PortableServer.POA.Convert.To_Forward
        (POA.To_Ref
         (Broca.ORB.Resolve_Initial_References (Broca.ORB.Root_POA_ObjectId)));
   end Get_Default_POA;

   ---------------------------
   -- Raise_Forward_Request --
   ---------------------------

   procedure Raise_Forward_Request (Reference : CORBA.Object.Ref) is
   begin
      Broca.Exceptions.Raise_With_Address
        (ForwardRequest'Identity,
         To_Address (Object_Pointer (CORBA.Object.Get (Reference))));
   end Raise_Forward_Request;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out ForwardRequest_Members)
   is
      use Ada.Exceptions;
      Addr : System.Address;
      Res : CORBA.Object.Ref;
   begin
      if Exception_Identity (From) /= ForwardRequest'Identity then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      Broca.Exceptions.Get_Member (From, Addr);
      CORBA.Object.Set (Res, Broca.Refs.Ref_Ptr (To_Pointer (Addr)));
      To := ForwardRequest_Members'(CORBA.IDL_Exception_Members with Res);
   end Get_Members;

end PortableServer;
