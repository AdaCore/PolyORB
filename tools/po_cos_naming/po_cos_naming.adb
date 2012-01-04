------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O _ C O S _ N A M I N G                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

--  Stand-alone server with a CORBA COS Naming's Root Context

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

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

   procedure Scan_Command_Line;
   --  Scan the command line

   procedure Usage;
   --  Print usage information

   -----------------------
   -- Scan_Command_Line --
   -----------------------

   procedure Scan_Command_Line is
      use Ada.Command_Line;
   begin
      if Argument_Count > 0 then
         for J in 1 .. Argument_Count loop
            if Argument (J) = "-file" then
               Print_To_File := True;
               Filename := Ada.Strings.Unbounded.To_Unbounded_String
                 (Argument (J + 1));

            elsif Argument (J) = "-help" then
               Usage;
            end if;
         end loop;
      end if;
   end Scan_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      New_Line;
      Put_Line (Standard_Error, "Usage: po_cos_naming"
                & ASCII.LF
                & " -file <filename> : output COS Naming IOR to 'filename'"
                & ASCII.LF
                & " -help : print this help"
                & ASCII.LF
                & " [PolyORB command line configuration variables]");
      New_Line;
   end Usage;

   --  Main procedure begins here

   Root_NC  : CosNaming.NamingContextExt.Impl.Object_Ptr;
   Ref      : CORBA.Object.Ref;

begin
   Scan_Command_Line;
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
