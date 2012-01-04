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

with all_functions.Impl;
with all_functions.Helper;

with CORBA;
with CORBA.Object;
with CORBA.ORB;

with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;
with PolyORB.CORBA_P.CORBALOC;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

--  Note : the server must execute two tasks concurrently to pass oneway tests

with Ada.Command_Line;
with Ada.Text_IO;

with Run_Tests;

procedure Server is
   Ref : CORBA.Object.Ref;
begin
   CORBA.ORB.Initialize ("ORB");
   Initiate_Servant (new all_functions.Impl.Object, Ref);

   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String
              (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref)) & "'");

   Initiate_Server (Start_New_Task => True);

   if Ada.Command_Line.Argument_Count = 1
        and then Ada.Command_Line.Argument (1) = "local"
   then
      Run_Tests (all_functions.Helper.To_Ref (Ref));
   end if;
end Server;
