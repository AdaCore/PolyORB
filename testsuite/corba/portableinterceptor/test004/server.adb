------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;
with PortableInterceptor.ORBInitializer.Initialize_All;
with PortableInterceptor.ORBInitializer.Register;
with PortableServer.POA.Helper;
with PortableServer.POAManager;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with Test.Demo.Impl;
with Test.ServerORBInitializer.Impl;

procedure Server is
begin
   --  Initialize ORB

   declare
      Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;
   begin
      CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);
   end;

   --  Register Interceptors

   declare
      Ptr  : constant Test.ServerORBInitializer.Impl.Object_Ptr
        := new Test.ServerORBInitializer.Impl.Object;
      Ref  : PortableInterceptor.ORBInitializer.Local_Ref;

   begin
      PortableInterceptor.ORBInitializer.Set
        (Ref, CORBA.Impl.Object_Ptr (Ptr));

      PortableInterceptor.ORBInitializer.Register (Ref);

      PortableInterceptor.ORBInitializer.Initialize_All;
   end;

   --  Create POA and object reference

   declare
      Root_POA : PortableServer.POA.Local_Ref;
      My_POA   : PortableServer.POA.Local_Ref;
      Policies : CORBA.Policy.PolicyList;

      Ref : CORBA.Object.Ref;
      Obj : constant CORBA.Impl.Object_Ptr := new Test.Demo.Impl.Object;

   begin

      --  Retrieve Root POA

      Root_POA := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      --  Create My POA

      My_POA :=
         PortableServer.POA.Local_Ref
         (PortableServer.POA.Create_POA
          (Root_POA,
           CORBA.To_CORBA_String ("My_POA"),
           PortableServer.POA.Get_The_POAManager (Root_POA),
           Policies));

      --  Set up new object

      Ref := PortableServer.POA.Servant_To_Reference
        (My_POA, PortableServer.Servant (Obj));

      Ada.Text_IO.Put_Line
        ("'"
         & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
         & "'");

      --  Launch the server

      CORBA.ORB.Run;
   end;

end Server;
