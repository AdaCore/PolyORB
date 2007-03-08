------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O _ C O S _ N A M I N G                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Stand-alone server with a CORBA COS Naming's Root Context

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNAT.Command_Line;

with CORBA.Object;
with CORBA.ORB;
with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.CORBA_P.CORBALOC;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with CosNaming.NamingContextExt.Impl;

procedure PO_COS_Naming is

   use Ada.Text_IO;

   Print_To_File : Boolean := False;
   Filename : Ada.Strings.Unbounded.Unbounded_String;
   Scan_Succesful : Boolean := False;

   procedure Scan_Command_Line;
   --  Scan the command line

   procedure Usage;
   --  Print usage information

   -----------------------
   -- Scan_Command_Line --
   -----------------------

   procedure Scan_Command_Line is
      use GNAT.Command_Line;
   begin
      loop
         case Getopt ("file:") is
            when ASCII.NUL =>
               exit;

            when 'f' =>
               if Full_Switch = "file" then
                  Print_To_File := True;
                  Filename := Ada.Strings.Unbounded.To_Unbounded_String
                    (Parameter);
               end if;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      Scan_Succesful := True;

   exception
      when Invalid_Switch =>
         Put_Line (Standard_Error, "Error: Invalid Switch " & Full_Switch);
         Usage;

      when Invalid_Parameter =>
         Put_Line (Standard_Error, "Error: No parameter for " & Full_Switch);
         Usage;
   end Scan_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      New_Line;
      Put_Line (Standard_Error, "Usage: po_cos_naming"
                & " -file <filename> : output COS Naming IOR to 'filename'");
      New_Line;
   end Usage;

   --  Main procedure begins here

   Root_NC  : CosNaming.NamingContextExt.Impl.Object_Ptr;
   Ref      : CORBA.Object.Ref;

begin
   Scan_Command_Line;
   if not Scan_Succesful then
      return;
   end if;

   CORBA.ORB.Initialize ("ORB");

   Root_NC := CosNaming.NamingContextExt.Impl.Create;

   PolyORB.CORBA_P.Server_Tools.Initiate_Well_Known_Service
     (PortableServer.Servant (Root_NC), "NameService", Ref);
   CORBA.ORB.Register_Initial_Reference
     (CORBA.ORB.To_CORBA_String ("NamingService"), Ref);

   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_NAME_SERVICE="
      & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)));
   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_NAME_SERVICE="
      & CORBA.To_Standard_String
          (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref)));

   if Print_To_File then
      declare
         File : File_Type;

      begin
         Create (File, Out_File, Ada.Strings.Unbounded.To_String (Filename));
         Put_Line
           (File,
            CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)));
         Close (File);
      end;
   end if;

   PolyORB.CORBA_P.Server_Tools.Initiate_Server;
end PO_COS_Naming;
