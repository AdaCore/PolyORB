--  Echo server.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

with CORBA; use CORBA;
with CORBA.ORB; use CORBA.ORB;
with CORBA.Object;
with Echo.My_Impl;
with Broca.Rootpoa;
with PortableServer;
with PortableServer.POA;
with PortableServer.POAManager;

with Server; use Server;

procedure Serverp is
   use CORBA.ORB.IDL_SEQUENCE_ObjectId;

   Poa : PortableServer.POA.Ref;
   Poa_Manager : PortableServer.POAManager.Ref;
   Oid : PortableServer.ObjectId;

   Ior : CORBA.String;
   Oid_List : ObjectIdList;
begin
   Decode_Options;

   --  Disp the list of known initial services.
   if Flag_Verbose then
      Put_Line ("list of initial services:");
      Oid_List := List_Initial_Services;
      for I in 1 .. Length (Oid_List) loop
         Put_Line (To_Standard_String (Element_Of (Oid_List, I)));
      end loop;
   end if;

   --  Create and activate the implementation.
   My_Echo := new Echo.My_Impl.Object;
   if Flag_Delay then
      My_Echo.Delay_Time := 5.0;
   else
      My_Echo.Delay_Time := 0.0;
   end if;

   --  Find the Root POA.
   declare
      Root_Poa : PortableServer.POA.Ref;
      Object_Ref : CORBA.Object.Ref;
   begin
      Object_Ref := Resolve_Initial_References (To_CORBA_String ("RootPOA"));
      Root_Poa := PortableServer.POA.To_Ref (Object_Ref);
      Poa := Server.Build_Poa_Tree (Root_Poa);
      Poa_Manager := PortableServer.POA.Get_The_POAManager (Root_Poa);
   exception
      when InvalidName =>
         Put_Line ("No RootPOA (add `with broca.rootpoa;')");
         return;
   end;

   --  displays the IOR of the implementation object.
   Ior := CORBA.Object.Object_To_String (Ref_To_Export);
   Put_Line ("'" & To_Standard_String (Ior) & "'");

   if Flag_Ior_File /= null then
      declare
         use Ada.Text_IO;
         File : File_Type;
      begin
         Create (File, Out_File, Flag_Ior_File.all);
         Put_Line (File, To_Standard_String (Ior));
         Close (File);
      end;
   end if;

   if Flag_Discard then
      PortableServer.POAManager.Discard_Requests (Poa_Manager, False);
   else
      --  Activate the RootPOA.
      PortableServer.POAManager.Activate (Poa_Manager);
   end if;

   --  Run the ORB: accept requests.
   if Flag_Tilt then
      declare
         task Orb;

         task body Orb is
         begin
            --  Run the ORB: accept requests.
            CORBA.ORB.Run;
         end Orb;
      begin
         loop
            Put ("activate RootPOA...");
            --  Activate the RootPOA.
            PortableServer.POAManager.Activate (Poa_Manager);
            Put_Line ("done");
            delay 5.0;

            Put ("discarding requests for RootPOA...");
            --  Activate the RootPOA.
            PortableServer.POAManager.Discard_Requests (Poa_Manager, True);
            Put_Line ("done");
            delay 5.0;

            Put ("suspending requests for RootPOA...");
            --  Activate the RootPOA.
            PortableServer.POAManager.Hold_Requests (Poa_Manager, True);
            Put_Line ("done");
            delay 5.0;
         end loop;
      end;
   else
      CORBA.ORB.Run;
   end if;

exception
      when Bad_Option =>
         null;

      when E : CORBA.Internal =>
         declare
            Memb : System_Exception_Members;
         begin
            Get_Members (E, Memb);
            Put ("received exception ");
            Put (Ada.Exceptions.Exception_Name (E));
            Put (", minor");
            Put (Unsigned_Long'Image (Memb.Minor));
            Put (", completion status: ");
            Put_Line (Completion_Status'Image (Memb.Completed));
         end;

      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));


end Serverp;
