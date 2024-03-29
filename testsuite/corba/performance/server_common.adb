------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        S E R V E R _ C O M M O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2023, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

with PolyORB.CORBA_P.Server_Tools;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer;

with benchs.Impl;

package body Server_Common is

   procedure Launch_Server is
      use PolyORB.CORBA_P.Server_Tools;

      Ref : CORBA.Object.Ref;

   begin

      Ada.Text_IO.Put_Line ("Server starting.");
      CORBA.ORB.Initialize ("ORB");

      declare
         Obj : constant CORBA.Impl.Object_Ptr
           := new benchs.Impl.Object;
      begin
         Initiate_Servant (PortableServer.Servant (Obj), Ref);
      end;

      --  Print IOR so that we can give it to a client

      Ada.Text_IO.Put_Line
        ("'" & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)) &
         "'");

      --  Launch the server

      Initiate_Server;

   end Launch_Server;

end Server_Common;
