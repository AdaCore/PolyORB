------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        P O _ C O S _ N A M I N G                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  Stand-alone server with a CORBA COS Naming's Root Context

with Ada.Text_IO;
with CORBA.Object;
with PortableServer;

with CORBA.ORB;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.CORBA_P.CORBALOC;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with CosNaming.NamingContext.Impl; use CosNaming.NamingContext.Impl;

procedure PO_COS_Naming is

   subtype NamingContext_Ptr is CosNaming.NamingContext.Impl.Object_Ptr;

   Root_NC  : NamingContext_Ptr;
   Ref      : CORBA.Object.Ref;

   --  Port_Str : constant String
   --    := Get_Conf (Naming_Port, Naming_Port_Default);

   use PolyORB.CORBA_P.Server_Tools;

begin
   CORBA.ORB.Initialize ("ORB");

   Root_NC := CosNaming.NamingContext.Impl.Create;
   --  Initiate_Servant (PortableServer.Servant (Root_NC), Ref);

   PolyORB.CORBA_P.Server_Tools.Initiate_Well_Known_Service
     (PortableServer.Servant (Root_NC), "NameService", Ref);
   CORBA.ORB.Register_Initial_Reference
     (CORBA.ORB.To_CORBA_String ("NamingService"), Ref);
   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_NAMING_IOR="
      & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)));
   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_NAMING_URI="
      & CORBA.To_Standard_String
          (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref)));

   Initiate_Server;
end PO_COS_Naming;
