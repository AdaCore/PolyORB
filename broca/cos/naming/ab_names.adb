------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                             A B _ N A M E S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with PortableServer;

with Broca.Environment;            use Broca.Environment;
with Broca.ORB;                    use Broca.ORB;
with Broca.Inet_Server;
with Broca.Server_Tools;
with CosNaming.NamingContext.Impl; use CosNaming.NamingContext.Impl;

procedure AB_Names is

   subtype NamingContext_Ptr is CosNaming.NamingContext.Impl.Object_Ptr;

   Root_NC  : constant NamingContext_Ptr := new Object;

   Ref      : CORBA.Object.Ref;

   Port_Str : constant String := Get_Conf (Naming_Port, Naming_Port_Default);

begin
   --  Broca.Inet_Server.Start (Natural'Value (Port_Str));
   Broca.Server_Tools.Initiate_Server;
   Broca.Server_Tools.Initiate_Servant
     (PortableServer.Servant (Root_NC), Ref);
   Register_Initial_Reference (Name_Service_ObjectId, Ref);
end AB_Names;
