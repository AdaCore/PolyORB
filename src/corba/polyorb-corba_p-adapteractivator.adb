with CORBA;

with PolyORB.Smart_Pointers;

package body PolyORB.CORBA_P.AdapterActivator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self :    out PPT.AdapterActivator_Access;
      AA   : access PortableServer.AdapterActivator.Ref'Class) is
   begin
      Self := new CORBA_AdapterActivator;

      CORBA_AdapterActivator (Self.all).AA
        := PortableServer.AdapterActivator.AA_Ptr (AA);
   end Create;

   ---------------------------
   -- Get_Adapter_Activator --
   ---------------------------

   function Get_Adapter_Activator
     (Self : CORBA_AdapterActivator)
     return PortableServer.AdapterActivator.Ref'Class is
   begin
      return Self.AA.all;
   end Get_Adapter_Activator;

   ---------------------
   -- Unknown_Adapter --
   ---------------------

   procedure Unknown_Adapter
     (Self   : access CORBA_AdapterActivator;
      Parent : access PPT.Obj_Adapter'Class;
      Name   : in     String;
      Result :    out Boolean;
      Error  : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;

      CORBA_POA : PortableServer.POA_Forward.Ref;

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Parent));

      Result := PortableServer.AdapterActivator.Unknown_Adapter
        (PortableServer.AdapterActivator.Ref'Class (Self.AA.all),
         CORBA_POA,
         CORBA.To_CORBA_String (Name));

   exception
      when others =>
         Result := False;

         Throw (Error,
                Obj_Adapter_E,
                System_Exception_Members'
                (Minor     => 1,
                 Completed => Completed_No));
   end Unknown_Adapter;

end PolyORB.CORBA_P.AdapterActivator;
