with Ada.Command_Line;
with Ada.Text_IO;

with CORBA;
with CORBA.Object;
with PortableServer.POA;
with PortableServer.POAManager;
with PortableServer.AdapterActivator;
with PortableServer.AdapterActivator.Impl;
with PortableServer.ServantActivator.Impl;
with PortableServer.ServantLocator.Impl;
with PortableServer.ServantManager.Impl;
pragma Elaborate_All (PortableServer.AdapterActivator);
pragma Elaborate_All (PortableServer.AdapterActivator.Impl);

package body Server is
   Object_Ref : CORBA.Object.Ref;

   procedure Decode_Options is
      use Ada.Command_Line;
      use Ada.Text_IO;
      Flag_Ignore_Next : Boolean := False;
   begin
      --  Scan command line
      for I in 1 .. Argument_Count loop
         if Flag_Ignore_Next and Argument (I)(1) /= '-' then
            Flag_Ignore_Next := False;
         elsif Argument (I)(1 .. 4) = "-ORB" then
            Flag_Ignore_Next := True;
         else
            Flag_Ignore_Next := False;
            if Argument (I) = "-v" then
               Flag_Verbose := True;
            elsif Argument (I) = "--no-wall" then
               Flag_Wall := False;
            elsif Argument (I) = "--discard" then
               Flag_Discard := True;
            elsif Argument (I) = "--delay" then
               Flag_Delay := True;
            elsif Argument (I) = "--tilt" then
               Flag_Tilt := True;
            elsif Argument (I) = "--serial" then
               Flag_Serial := True;
            elsif Argument (I) = "--servant=default" then
               Flag_Servant_Policy := PortableServer.USE_DEFAULT_SERVANT;
            elsif Argument (I) = "--servant=active" then
               Flag_Servant_Policy :=
                 PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY;
            elsif Argument (I) = "--servant=manager" then
               Flag_Servant_Policy :=
                 PortableServer.USE_SERVANT_MANAGER;
            elsif Argument (I) = "--save-ior" then
               Flag_Ior_File := new String'(Argument (I + 1));
               Flag_Ignore_Next := True;
            else
               Put_Line (Command_Name & " [-v] [--discard] [--delay]");
               Put_Line ("  -v         disp list of initial services");
               Put_Line ("  --discard  discard all requests");
               Put_Line ("  --delay    echoString is delaying");
               Put_Line ("  --tilt     RootPOA state is changed");
               Put_Line ("  --serial   Serial access to echo server");
               Put_Line ("  --save-ior FILE  save ior into FILE");
               Put_Line ("  --servant=X  X: active, default, manager");
               Set_Exit_Status (Success);
               raise Bad_Option;
            end if;
         end if;
      end loop;
   end Decode_Options;

   --  An servant locator.

   type My_Sl_Object is new PortableServer.ServantLocator.Impl.Object
     with null record;

   procedure Preinvoke
     (Self : in out My_Sl_Object;
      Oid : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : out PortableServer.ServantLocator.Cookie;
      Returns : out PortableServer.Servant);

   procedure Postinvoke
     (Self : in out My_Sl_Object;
      Oid : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : in PortableServer.ServantLocator.Cookie;
      The_Servant : in PortableServer.Servant);

   procedure Preinvoke
     (Self : in out My_Sl_Object;
      Oid : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : out PortableServer.ServantLocator.Cookie;
      Returns : out PortableServer.Servant) is
   begin
      Ada.Text_IO.Put_Line ("preinvoke: forward request");
      PortableServer.Raise_Forward_Request (Object_Ref);
      The_Cookie := null;
      Returns := null;
   end Preinvoke;

   procedure Postinvoke
     (Self : in out My_Sl_Object;
      Oid : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Operation : in CORBA.Identifier;
      The_Cookie : in PortableServer.ServantLocator.Cookie;
      The_Servant : in PortableServer.Servant) is
   begin
      null;
   end Postinvoke;

   --  An adapter activator.

   type My_Aa_Object is new PortableServer.AdapterActivator.Impl.Object
     with null record;
   procedure Unknown_Adapter
     (Self   : in out My_Aa_Object;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String;
      Returns : out Boolean);

   procedure Unknown_Adapter
     (Self   : in out My_Aa_Object;
      Parent : PortableServer.POA_Forward.Ref;
      Name   : CORBA.String;
      Returns : out Boolean)
   is
      use PortableServer.POA;
      use PortableServer;
      Res : Ref;
      Obj : CORBA.Object.Ref;
      Nil : PortableServer.POAManager.Ref;
   begin
      Ada.Text_IO.Put_Line ("unknown_adapter called");
      if CORBA.To_Standard_String (Name) = "test_1" then
         Res := Ref (Create_POA
                     (PortableServer.POA.Convert.From_Forward (Parent),
                      CORBA.To_CORBA_String ("test_1"),
                      Nil,
                      ORB_CTRL_MODEL,
                      TRANSIENT,
                      UNIQUE_ID,
                      SYSTEM_ID,
                      NO_IMPLICIT_ACTIVATION,
                      NON_RETAIN,
                      USE_SERVANT_MANAGER));
         Set_Servant_Manager
           (Res, ServantManager.Impl.To_Ref (new My_Sl_Object));
         Obj := Create_Reference
           (Res, CORBA.To_CORBA_String ("IDL:Echo:1.0"));

         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Res));

         Ref_To_Export := Obj;

         Returns := True;
      else
         Returns := False;
      end if;
   end Unknown_Adapter;

   My_Aa_Ref : PortableServer.AdapterActivator.Ref;

   type My_Sa_Object is new PortableServer.ServantActivator.Impl.Object
     with null record;
   procedure Incarnate
     (Self    : in out My_Sa_Object;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Returns : out PortableServer.Servant);

   procedure Etherealize
     (Self    : in out My_Sa_Object;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref;
      Serv    : in PortableServer.Servant;
      Cleanup_In_Progress : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean);

   procedure Incarnate
     (Self    : in out My_Sa_Object;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA.Ref;
      Returns : out PortableServer.Servant) is
   begin
      Ada.Text_IO.Put_Line ("incarnate called");
      Returns := PortableServer.Servant (My_Echo);
   end Incarnate;

   procedure Etherealize
     (Self    : in out My_Sa_Object;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref;
      Serv    : in PortableServer.Servant;
      Cleanup_In_Progress : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean) is
   begin
      null;
   end Etherealize;


   function Build_Poa_Tree (Poa : PortableServer.POA.Ref)
     return PortableServer.POA.Ref
   is
      use PortableServer.POA;
      use PortableServer;
      Res : Ref;
      Thread_Policy : ThreadPolicyValue;
      Nil : PortableServer.POAManager.Ref;
      A_Ref : CORBA.Object.Ref;
      Oid : PortableServer.ObjectId;
   begin
      My_Aa_Ref := AdapterActivator.Impl.To_Ref (new My_Aa_Object);
      Set_The_Activator (Poa, My_Aa_Ref);
      Res := Ref (Find_POA (Poa, CORBA.To_CORBA_String ("test_1"), True));

      My_Servant_Manager :=
        ServantManager.Impl.To_Ref (new My_Sa_Object);

      if Flag_Serial then
         Thread_Policy := SINGLE_THREAD_MODEL;
      else
         Thread_Policy := ORB_CTRL_MODEL;
      end if;

      --  The type conversion prevents a dynamically tagged expression!
      Res := Ref (Create_POA (Poa,
                              CORBA.To_CORBA_String ("test_st"),
                              Nil,
                              Thread_Policy,
                              TRANSIENT,
                              UNIQUE_ID,
                              SYSTEM_ID,
                              NO_IMPLICIT_ACTIVATION,
                              RETAIN,
                              Flag_Servant_Policy));


      case Flag_Servant_Policy is
         when PortableServer.USE_DEFAULT_SERVANT =>
            A_Ref := PortableServer.POA.Create_Reference
              (Poa, CORBA.To_CORBA_String ("IDL:Echo:1.0"));
            PortableServer.POA.Set_Servant
              (Poa, PortableServer.Servant (My_Echo));
         when PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY =>
            Oid := PortableServer.POA.Activate_Object
              (Poa, PortableServer.Servant (My_Echo));
            A_Ref := PortableServer.POA.Servant_To_Reference
              (Poa, PortableServer.Servant (My_Echo));
         when PortableServer.USE_SERVANT_MANAGER =>
            A_Ref := PortableServer.POA.Create_Reference
              (Poa, CORBA.To_CORBA_String ("IDL:Echo:1.0"));
            PortableServer.POA.Set_Servant_Manager (Poa, My_Servant_Manager);
      end case;

      Object_Ref := A_Ref;

      if CORBA.Object.Is_Nil (Ref_To_Export) then
         Ref_To_Export := A_Ref;
      end if;
      return Res;
   end Build_Poa_Tree;
end Server;
