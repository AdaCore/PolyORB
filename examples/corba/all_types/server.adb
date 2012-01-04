------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with GNAT.Command_Line;  use GNAT.Command_Line;
--  with GNAT.Exception_Traces; use GNAT.Exception_Traces;

with PolyORB.CORBA_P.CORBALOC;
with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
--  with PolyORB.Setup.Ravenscar_TP_Server;
--  pragma Warnings (Off, PolyORB.Setup.Ravenscar_TP_Server);
--  with PolyORB.Setup.Thread_Pool_Server;
--  pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with PolyORB.CORBA_P.Naming_Tools; use PolyORB.CORBA_P.Naming_Tools;

with CORBA;
with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with PortableServer;

with all_types.Impl;

procedure Server is
   use PolyORB.CORBA_P.Server_Tools;

   Ref             : CORBA.Object.Ref;
   Register_Server : Boolean := False;
   --  Use_Delegate    : Boolean := False;

begin
   --  Trace_On (Every_Raise);
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
      Obj : constant CORBA.Impl.Object_Ptr
        := new all_types.Impl.Object;
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
            Register ("all_types", Ref);
         else
            Register (Name, Ref);
         end if;
      end;
   end if;

   --  Print IOR so that we can give it to a client

   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String
              (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref)) & "'");

   --  Launch the server
   Initiate_Server;
end Server;
