with Broca.Exceptions;
with Broca.POA; use Broca.POA;
with Broca.Refs;

package body PortableServer.POAManager is
   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                          To   : out AdapterInactive_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= AdapterInactive'Identity then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      To := AdapterInactive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   --  Convert a REF to a POAManager_Object_Access.
   --  Check the type of the referenced object.
   function To_Poa_Manager (Self : Ref) return POAManager_Object_Access;

   function To_Poa_Manager (Self : Ref)
                            return POAManager_Object_Access
   is
      use Broca.Refs;
      Res : Broca.Refs.Ref_Ptr;
   begin
      Res := Get (Self);
      if Res = null or else Res.all not in POAManager_Object'Class then
         Broca.Exceptions.Raise_Bad_Param;
      else
         return POAManager_Object_Access (Res);
      end if;
   end To_Poa_Manager;

   procedure Activate (Self : Ref) is
      Poa_Manager : POAManager_Object_Access;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Activate (Poa_Manager.all);
      end if;
   end Activate;

   procedure Hold_Requests (Self : Ref; Wait_For_Completion : CORBA.Boolean)
   is
      Poa_Manager : POAManager_Object_Access;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Hold_Requests (Poa_Manager.all, Wait_For_Completion);
      end if;
   end Hold_Requests;

   procedure Discard_Requests (Self : Ref; Wait_For_Completion : CORBA.Boolean)
   is
      Poa_Manager : POAManager_Object_Access;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Discard_Requests (Poa_Manager.all, Wait_For_Completion);
      end if;
   end Discard_Requests;

   procedure Deactivate
     (Self : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      Poa_Manager : POAManager_Object_Access;
   begin
      Poa_Manager := To_Poa_Manager (Self);
      if Is_Inactive (Poa_Manager.all) then
         raise AdapterInactive;
      else
         Deactivate
           (Poa_Manager.all, Etherealize_Objects, Wait_For_Completion);
      end if;
   end Deactivate;

end PortableServer.POAManager;
