------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with PortableServer.POA.Helper;
with PortableServer.POAManager;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.GIOP_P.Code_Sets.Converters.Test;
pragma Warnings (Off, PolyORB.GIOP_P.Code_Sets.Converters.Test);

with Test_Interface.Impl;

procedure Server is
   Argv     : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   Root_POA : PortableServer.POA.Local_Ref;
   Ref      : CORBA.Object.Ref;
begin
   CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);

   Root_POA := PortableServer.POA.Helper.To_Local_Ref
     (CORBA.ORB.Resolve_Initial_References
      (CORBA.ORB.To_CORBA_String ("RootPOA")));

   PortableServer.POAManager.Activate
     (PortableServer.POA.Get_The_POAManager (Root_POA));

   declare
      Obj : constant CORBA.Impl.Object_Ptr := new Test_Interface.Impl.Object;
   begin
      Ref :=
        PortableServer.POA.Servant_To_Reference
         (Root_POA, PortableServer.Servant (Obj));
   end;

   Ada.Text_IO.Put_Line
     ("'"
        & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
        & "'");
   Ada.Text_IO.New_Line;

   CORBA.ORB.Run;
end Server;
