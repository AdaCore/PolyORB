------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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

--  Testing MOMA server.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.Types;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with MOMA.Configuration.Server;
with MOMA.Types;
with PolyORB.MOMA_P.Tools;

procedure Server is

   use MOMA.Configuration;
   use MOMA.Configuration.Server;
   use MOMA.Types;
   use PolyORB.MOMA_P.Tools;

   MOMA_Ref : PolyORB.References.Ref;
   Pool_1   : Message_Pool;

begin
   --  Load Configuration File.
   Load_Configuration_File ("destinations.conf");

   --  Get information about destination #1.
   Pool_1 := Get_Message_Pool (1);

   --  Create one message pool.
   Create_Message_Pool (Pool_1, MOMA_Ref);

   --  Outputs its reference.
   Put_Line (PolyORB.Types.To_Standard_String
             (PolyORB.References.IOR.Object_To_String (MOMA_Ref)));

   --  Run the server.
   Run_Server;

end Server;
