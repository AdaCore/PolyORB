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

--  Random Server.

with Random.Impl;

with CORBA;
with CORBA.Object;
with CORBA.ORB;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.CORBA_P.Server_Tools;

with Ada.Text_IO;

procedure Server is

   use PolyORB.CORBA_P.Server_Tools;

   Ref : CORBA.Object.Ref;

begin
   CORBA.ORB.Initialize ("ORB");
   Initiate_Servant (new Random.Impl.Object, Ref);
   Ada.Text_IO.Put_Line
     ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
      "'");
   Initiate_Server;
end Server;
