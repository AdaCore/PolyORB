with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA;

with PolyORB.Utils.Report;

package body Test_AdapterActivator is

   use PolyORB.Utils.Report;

   ---------------------
   -- Unknown_Adapter --
   ---------------------

   Null_Activator_Called : Boolean := False;

   function Unknown_Adapter
     (Self   : NullAdapter_Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean is
   begin
      Null_Activator_Called := True;

      return False;
   end Unknown_Adapter;

   Meta_Child_POA : PortableServer.POA.Ref;

   Simple_Activator_Called : Boolean := False;

   function Unknown_Adapter
     (Self   : SimpleAdapter_Ref;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String)
     return Boolean
   is
      package Convert is new
        PortableServer.POA_Forward.Convert (PortableServer.POA.Ref);

      Policies : CORBA.Policy.PolicyList;

      POA : constant PortableServer.POA.Ref := Convert.To_Ref (Parent);

   begin
      Simple_Activator_Called := True;

      Meta_Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (POA,
          Name,
          PortableServer.POA.Get_The_POAManager (POA),
          Policies));

      return True;
   end Unknown_Adapter;

   -------------------------------
   -- Run_Test_AdapterActivator --
   -------------------------------

   procedure Run_Test_AdapterActivator
   is
      NullAdapter : PortableServer.AdapterActivator.AA_Ptr
        := new NullAdapter_Ref;

      SimpleAdapter : PortableServer.AdapterActivator.AA_Ptr
        := new SimpleAdapter_Ref;

      Policies : CORBA.Policy.PolicyList;

      Root_POA : constant PortableServer.POA.Ref :=
        PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Child_POA : PortableServer.POA.Ref;
      Foo_POA : PortableServer.POA.Ref;

   begin
      New_Test ("Adapter Activator");

      --  Register a Child POA
      Child_POA := PortableServer.POA.Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Created child POA", True);

      --  Look for a non existent child POA without AdapterActivator

      begin
         Foo_POA := PortableServer.POA.Ref
           (PortableServer.POA.Find_POA
            (Child_POA,
             CORBA.To_CORBA_String ("Foo"),
             True));
      exception
         when PortableServer.POA.AdapterNonExistent =>
            null;
      end;

      --  Set 'Null' Adapter Activator
      PortableServer.POA.Set_The_Activator
        (Child_POA,
         NullAdapter);

      --  Look for a non existent child POA with Null AdapterActivator

      begin
         Foo_POA := PortableServer.POA.Ref
           (PortableServer.POA.Find_POA
            (Child_POA,
             CORBA.To_CORBA_String ("Foo"),
             True));
      exception
         when PortableServer.POA.AdapterNonExistent =>
            null;
      end;

      Output ("Null Unknown_Adapter invoked", Null_Activator_Called);

      --  Set 'Simple' Adapter Activator
      PortableServer.POA.Set_The_Activator
        (Child_POA,
         SimpleAdapter);

      --  Look for a non existent child POA with Simple AdapterActivator

      Foo_POA := PortableServer.POA.Ref
        (PortableServer.POA.Find_POA
         (Child_POA,
          CORBA.To_CORBA_String ("Foo"),
          True));

      Output ("Simple Unknown_Adapter invoked", Simple_Activator_Called);

      --  Simple check

      Foo_POA := PortableServer.POA.Ref
        (PortableServer.POA.Find_POA
         (Child_POA,
          CORBA.To_CORBA_String ("Foo"),
          True));

   end Run_Test_AdapterActivator;

end Test_AdapterActivator;
