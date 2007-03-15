------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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

--  Sample MOMA server

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with MOMA.Configuration.Server;
with MOMA.References;
with MOMA.Runtime;
with MOMA.Types;

procedure Server is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Configuration;
   use MOMA.Configuration.Server;
   use MOMA.Types;

   MOMA_Ref : MOMA.Types.Ref;
   Pool_1   : Message_Pool;

begin

   --  Initialize MOMA

   MOMA.Runtime.Initialize;

   --  Argument check

   if Argument_Count > 1 then
      Put_Line ("usage : server [Naming_Service_IOR]");
      return;
   end if;

   --  Load Configuration File.

   Load_Configuration_File ("destinations.conf");

   --  Get information about destination #1

   Pool_1 := Get_Message_Pool (1);

   --  Create one message pool

   Create_Message_Pool (Pool_1, MOMA_Ref);

   --  Outputs its reference

   Put_Line ("'" & MOMA.References.Reference_To_IOR_String (MOMA_Ref) & "'");

   --  Register reference to naming service

   begin
      if Argument_Count = 1 then
         MOMA.References.Initialize_Naming_Service
           (Ada.Command_Line.Argument (1));

         MOMA.References.Register_Name ("Pool_1", MOMA_Ref);
      end if;
   exception
      when others => Put_Line ("Could not initialise Message Pool");
   end;

   --  Run the server

   MOMA.Runtime.Start;

end Server;
