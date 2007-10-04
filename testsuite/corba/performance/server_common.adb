------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        S E R V E R _ C O M M O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2007, Free Software Foundation, Inc.             --
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

with Ada.Text_IO;

with PolyORB.CORBA_P.Server_Tools;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer;

with Benchs.Impl;

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
