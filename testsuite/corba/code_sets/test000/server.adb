------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
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

with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with PortableServer.POA.Helper;
with PortableServer.POAManager;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.GIOP_P.Code_Sets.Converters.Test;
pragma Warnings (Off, PolyORB.GIOP_P.Code_Sets.Converters.Test);

with Test_Interface.Impl;

procedure Server is
   Argv     : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   Root_POA : PortableServer.POA.Ref;
   Ref      : CORBA.Object.Ref;
begin
   CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);

   Root_POA := PortableServer.POA.Helper.To_Ref
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
