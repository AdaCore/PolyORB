------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Delegated_Server;

with Echo.Impl;

with CORBA;
with CORBA.Object;

with Broca.Server_Tools; use Broca.Server_Tools;
pragma Elaborate (Broca.Server_Tools);

with GNAT.Command_Line;  use GNAT.Command_Line;
with Ada.Text_IO;

with PortableServer;

with Broca.Naming_Tools; use Broca.Naming_Tools;

procedure Server is
   Ref             : CORBA.Object.Ref;
   Register_Server : Boolean := False;
   Use_Delegate    : Boolean := False;
begin

   --  Parse command line

   loop
      case Getopt ("d s") is
         when ASCII.NUL => exit;
         when 'd'       => Use_Delegate := True;
         when 's'       => Register_Server := True;
         when others    => raise Program_Error;
      end case;
   end loop;

   --  Should we use the Delegate or the regular version?

   if Use_Delegate then
      Initiate_Servant
        (PortableServer.Servant
         (Delegated_Server.Delegated.Create (Delegated_Server.Dummy'Access)),
         Ref);
   else
      Initiate_Servant (new Echo.Impl.Object, Ref);
   end if;

   --  If the server is to be registered, check whether there is a name
   --  given on the command line, use "echo" otherwise.

   if Register_Server then
      declare
         Name : constant String := Get_Argument;
      begin
         if Name = "" then
            Register ("echo", Ref);
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
