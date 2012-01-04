------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                P O _ I R                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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
