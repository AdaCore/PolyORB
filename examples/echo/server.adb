------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id: //droopi/main/src/server.adb#9 $

with Ada.Text_IO;
with GNAT.Command_Line;  use GNAT.Command_Line;

with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;
pragma Elaborate (PolyORB.CORBA_P.Server_Tools);
with PolyORB.ORB.No_Tasking;
pragma Warnings (Off, PolyORB.ORB.No_Tasking);
with PolyORB.POA_Types;
with PolyORB.Setup.Test;

with CORBA;
with CORBA.Object;
with CORBA.Impl;
with PortableServer;

with Echo.Impl;

procedure Server is
   Ref             : CORBA.Object.Ref;
   --  Register_Server : Boolean := False;
   --  Use_Delegate    : Boolean := False;

begin

   --  Parse command line

   loop
      case Getopt ("d s") is
         when ASCII.NUL => exit;
         --  when 'd'       => Use_Delegate := True;
         --  when 's'       => Register_Server := True;
         when others    => raise Program_Error;
      end case;
   end loop;

   --  Should we use the Delegate or the regular version?

   declare
      use CORBA.Impl;

      Obj : constant Object_Ptr
        := new Echo.Impl.Object;
   begin
      PolyORB.Setup.Test.Initialize_Test_Server;
      PolyORB.Setup.Test.Initialize_Test_Access_Points;
      --  XXX PolyORB initialisation.
      --  Should be automated by PolyORB Automatic Configurator.

      PolyORB.POA_Types.Servant_Access
        (To_PolyORB_Servant
         (CORBA.Impl.Object (Obj.all)'Access)).If_Desc.External_Name
        := CORBA.To_CORBA_String ("IDL:Echo:1.0");

      --  XXX Set the interface description for Echo
      --  Not quite the best way to do this.
      --  Actually the POA should not depend on an ifdesc
      --  to be present in every servant object. Better
      --  would be to define a primitive Get_If_Desc on Servant_Base
      --  or somesuch, but we cannot that (becaause that would mean
      --  mix generated code (impl of Get_If_Desc) with user code (other
      --  primitives of Servant). What should be done is: keep in
      --  the POA a repository of Servant'Tag -> If_Desc mappings.
      --  (interesting: this is exactly what was done in AB : actually
      --  Skel == If_Desc (mostly).

      Initiate_Servant (PortableServer.Servant (Obj), Ref);
      --  Note that Ref is a smart pointer to a Reference_Info, *not*
      --  to a CORBA.Impl.Object.
   end;

   --  If the server is to be registered, check whether there is a name
   --  given on the command line, use "echo" otherwise.

--   if Register_Server then
--      declare
--         Name : constant String := Get_Argument;
--      begin
--         if Name = "" then
--            Register ("echo", Ref);
--         else
--            Register (Name, Ref);
--         end if;
--      end;
--   end if;

   --  Print IOR so that we can give it to a client

   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");



   --  Launch the server
   Initiate_Server;
end Server;
