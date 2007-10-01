------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               R O U T E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Sample MOMA router

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with MOMA.Configuration.Server;
with MOMA.References;
with MOMA.Runtime;
with MOMA.Types;

procedure Router is

   use Ada.Command_Line;
   use Ada.Text_IO;

   use MOMA.Configuration;
   use MOMA.Configuration.Server;
   use MOMA.Types;

   Other_Router   : MOMA.Types.Ref := MOMA.Types.Nil_Ref;
   Router         : MOMA.Types.Ref;

begin

   --  Argument check

   if Argument_Count < 1
     or else Argument_Count > 2
   then
      Put_Line ("usage : router <router_id> [IOR]");
      Put_Line ("where :");
      Put_Line ("-- 'router_id' is a the id of the router");
      Put_Line ("-- 'IOR'       is the IOR of another router");
      return;
   end if;

   --  Initialize MOMA

   MOMA.Runtime.Initialize;

   --  Find reference to other router if needed

   if Argument_Count = 2 then
      MOMA.References.String_To_Reference
        (Ada.Command_Line.Argument (2), Other_Router);
   end if;

   --  Create one router and output its reference

   MOMA.Configuration.Server.Create_Router
      (To_MOMA_String (Ada.Command_Line.Argument (1)),
       Router,
       Other_Router);

   Put_Line ("Router IOR :");
   Put_Line (MOMA.References.Reference_To_IOR_String (Router));

   --  Run the server

   MOMA.Runtime.Start;

end Router;
