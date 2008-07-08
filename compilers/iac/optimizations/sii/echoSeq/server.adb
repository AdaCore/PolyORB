
with Ada.Text_IO;
with GNAT.Command_Line;  use GNAT.Command_Line;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;

with CORBA;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with PortableServer;

with echoSeq.Impl;

procedure Server is
   use PolyORB.CORBA_P.Server_Tools;

   Ref             : CORBA.Object.Ref;
   Register_Server : Boolean := False;
   --  Use_Delegate    : Boolean := False;

begin

   Ada.Text_IO.Put_Line ("Server starting.");
   CORBA.ORB.Initialize ("ORB");

   --  Parse command line

   loop
      case Getopt ("d s") is
         when ASCII.NUL => exit;
            --  when 'd'       => Use_Delegate := True;
         when 's'       => Register_Server := True;
         when others    => raise Program_Error;
      end case;
   end loop;

   --  Should we use the Delegate or the regular version?

   declare
--       use CORBA.Impl;

      Obj : constant CORBA.Impl.Object_Ptr
        := new echoSeq.Impl.Object;
   begin
      Initiate_Servant (PortableServer.Servant (Obj), Ref);
      --  Note that Ref is a smart pointer to a Reference_Info, *not*
      --  to a CORBA.Impl.Object.
   end;

   --  If the server is to be registered, check whether there is a name
   --  given on the command line, use "echo" otherwise.

   if Register_Server then
      declare
         Name : constant String := Get_Argument;
      begin
         if Name = "" then
            Register ("echoSeq", Ref);
         else
            Register (Name, Ref);
         end if;
      end;
   end if;

   --  Print IOR so that we can give it to a client

   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");

   --  Launch the server
   Initiate_Server;
end Server;
