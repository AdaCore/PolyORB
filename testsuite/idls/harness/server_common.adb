------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        S E R V E R _ C O M M O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  $Id: server_common.adb 6558 2004-06-21 10:24:28Z hugues $

with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with PortableServer;

with Harness.Impl;

with PolyORB.CORBA_P.Server_Tools;

package body Server_Common is

   -------------------
   -- Launch_Server --
   -------------------

   procedure Launch_Server is
      use PolyORB.CORBA_P.Server_Tools;
   begin

      Ada.Text_IO.Put_Line ("Server starting.");
      CORBA.ORB.Initialize ("ORB");

      declare
         Obj : constant CORBA.Impl.Object_Ptr := new Harness.Impl.Object;
         Ref : CORBA.Object.Ref;
      begin
         Initiate_Servant (PortableServer.Servant (Obj), Ref);

         --  Print IOR so that we can give it to a client

         Ada.Text_IO.Put_Line
           ("'"
            & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
            & "'");

         --  Launch the server

         Initiate_Server;
      end;
   end Launch_Server;

end Server_Common;
