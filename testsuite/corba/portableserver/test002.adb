------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 2                               --
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

with CORBA.ORB;
with CORBA.Object;
with CORBA.Policy;

with PortableServer.Current.Helper;
with PortableServer.POA.Helper;
with PortableServer.POAManager;

with Test.Helper;
with Test.Impl;
with Test_Globals;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.Utils.Report;

procedure Test002 is
   use PolyORB.Utils.Report;
   use PortableServer.Current;
   use Test;
   use Test_Globals;

   procedure Test_Get_POA;

   procedure Test_Get_Reference;

   procedure Test_Get_Object_Id;

   procedure Test_Get_Servant;

   ------------------
   -- Test_Get_POA --
   ------------------

   procedure Test_Get_POA is
      pragma Warnings (On); --  WAG:GCC3.4.4
      Aux : PortableServer.POA.Local_Ref;
      pragma Unreferenced (Aux);
      pragma Warnings (Off); --  WAG:GCC3.4.4

   begin
      Aux := PortableServer.POA.Convert.To_Ref (Get_POA (Test_Current));
      Get_POA_Success := False;

   exception
      when PortableServer.Current.NoContext =>
         null;

      when others =>
         Get_POA_Success := False;
   end Test_Get_POA;

   ------------------------
   -- Test_Get_Reference --
   ------------------------

   procedure Test_Get_Reference is
      pragma Warnings (On); --  WAG:GCC3.4.4
      Aux : CORBA.Object.Ref;
      pragma Unreferenced (Aux);
      pragma Warnings (Off); --  WAG:GCC3.4.4

   begin
      Aux := Get_Reference (Test_Current);
      Get_Reference_Success := False;

   exception
      when NoContext =>
         null;

      when others =>
         Get_Reference_Success := False;
   end Test_Get_Reference;

   ------------------------
   -- Test_Get_Object_Id --
   ------------------------

   procedure Test_Get_Object_Id is
      pragma Warnings (On); --  WAG:GCC3.4.4
      Aux : PortableServer.ObjectId;
      pragma Unreferenced (Aux);
      pragma Warnings (Off); --  WAG:GCC3.4.4

   begin
      Aux := Get_Object_Id (Test_Current);
      Get_Object_Id_Success := False;

   exception
      when NoContext =>
         null;

      when others =>
         Get_Object_Id_Success := False;
   end Test_Get_Object_Id;

   ----------------------
   -- Test_Get_Servant --
   ----------------------

   procedure Test_Get_Servant is
      Aux : PortableServer.Servant;
      pragma Unreferenced (Aux);

   begin
      Aux := Get_Servant (Test_Current);
      Get_Servant_Success := False;

   exception
      when NoContext =>
         null;

      when others =>
         Get_Servant_Success := False;
   end Test_Get_Servant;

begin
   New_Test ("PortableServer::Current operations");

   declare
      Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;

   begin
      CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);
   end;

   declare
      Root_POA : PortableServer.POA.Local_Ref;

      Policies : CORBA.Policy.PolicyList;

   begin
      --  Retrieve Root POA

      Root_POA := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Test_POA :=
        PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("TestPOA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      --  Set up new object

      Test_Servant := new Test.Impl.Object;

      Test_Id := PortableServer.POA.Activate_Object (Test_POA, Test_Servant);

      Test_Reference :=
        Test.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference (Test_POA, Test_Servant));
   end;

   --  Retrieve POA Current

   begin
      Test_Current :=
        PortableServer.Current.Helper.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("POACurrent")));

   exception
      when others =>
         null;
   end;

   Output ("Retrieve PortableServer::Current", not Is_Nil (Test_Current));

   --  Reset test state

   Get_POA_Success       := True;
   Get_Reference_Success := True;
   Get_Object_Id_Success := True;
   Get_Servant_Success   := True;

   Test_Get_POA;
   Test_Get_Reference;
   Test_Get_Object_Id;
   Test_Get_Servant;

   proc (Test_Reference);

   Test_Get_POA;
   Test_Get_Reference;
   Test_Get_Object_Id;
   Test_Get_Servant;

   Output ("PortableServer::Current::get_poa",       Get_POA_Success);
   Output ("PortableServer::Current::get_reference", Get_Reference_Success);
   Output ("PortableServer::Current::get_object_id", Get_Object_Id_Success);
   Output ("PortableServer::Current::get_servant",   Get_Servant_Success);

   End_Report;
end Test002;
