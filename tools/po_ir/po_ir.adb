------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                P O _ I R                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2007, Free Software Foundation, Inc.            --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;

with CORBA.Object;
with CORBA.ORB;
with CORBA.Repository_Root.Contained.Impl;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Repository.Impl;

with PolyORB.CORBA_P.CORBALOC;
with PolyORB.CORBA_P.Server_Tools;

with PortableServer;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);

procedure PO_IR is
   Repository_Obj : CORBA.Repository_Root.Repository.Impl.Object_Ptr;
   Ref            : CORBA.Object.Ref;

begin
   CORBA.ORB.Initialize ("ORB");

   Repository_Obj := new CORBA.Repository_Root.Repository.Impl.Object;
   CORBA.Repository_Root.Repository.Impl.Init
     (Repository_Obj,
      CORBA.Repository_Root.IRObject.Impl.Object_Ptr (Repository_Obj),
      CORBA.Repository_Root.dk_Repository,
      CORBA.Repository_Root.Contained.Impl.Contained_Seq.Null_Sequence);
   PolyORB.CORBA_P.Server_Tools.Initiate_Well_Known_Service
     (PortableServer.Servant (Repository_Obj), "InterfaceRepository", Ref);

   CORBA.ORB.Register_Initial_Reference
     (CORBA.ORB.To_CORBA_String ("InterfaceRepository"), Ref);

   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_IR_SERVICE="
      & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref)));

   Ada.Text_IO.Put_Line
     ("POLYORB_CORBA_IR_SERVICE="
      & CORBA.To_Standard_String
          (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref)));

   PolyORB.CORBA_P.Server_Tools.Initiate_Server;
end PO_IR;
