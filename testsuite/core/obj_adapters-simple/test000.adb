------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.Obj_Adapters.Simple;

with PolyORB.Initialization;
with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);

with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.Types;
with PolyORB.References;
with PolyORB.References.IOR;
with PolyORB.ORB.Interface;
with PolyORB.ORB;
with PolyORB.Setup;

with PolyORB.Report;
with Test_Servant;

procedure Test000 is

   use Ada.Text_IO;
   use Ada.Exceptions;
   use PolyORB.Types;
   use Test_Servant;
   use PolyORB.Objects;
   use PolyORB.ORB;
   use PolyORB.Obj_Adapters.Simple;
   use PolyORB.Setup;

   --------------
   -- Test_SOA --
   --------------

   procedure Test_SOA;

   procedure Test_SOA
   is
      Obj_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;
      S1  : My_Servant_Access;
   begin
      --  Create object adapter.
      Obj_Adapter := new Simple_Obj_Adapter;
      PolyORB.Obj_Adapters.Create (Obj_Adapter);

      --  Link object adapter with ORB.
      Set_Object_Adapter (The_ORB, Obj_Adapter);

      PolyORB.Report.Output ("Created Object Adapter", True);

      --  Create Servant.
      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");
      PolyORB.Report.Output ("Servant Created", True);

      --  Servant manipulation tests.

      declare
         My_Id : constant Object_Id_Access
           := new Object_Id'(PolyORB.Obj_Adapters.Export
                             (Obj_Adapter,
                              PolyORB.Servants.Servant_Access (S1)));
         --  Register it with the SOA.

         My_Ref : PolyORB.References.Ref;
      begin
         Set_Interface_Description
           (Simple_Obj_Adapter (Obj_Adapter.all),
            My_Id, Test_Servant.If_Desc);
         --  Set object description.

         Create_Reference (The_ORB, My_Id, "POLYORB:TEST_SERVANT:1.0", My_Ref);
         --  Obtain object reference.

         PolyORB.Report.Output ("Registered object", True);

         declare
            IOR : constant String :=
              PolyORB.Types.To_Standard_String
              (PolyORB.References.IOR.Object_To_String (My_Ref));
         begin
            PolyORB.Report.Output("IOR created", True);
         end;

         PolyORB.Obj_Adapters.Unexport (Obj_Adapter, My_Id);
         PolyORB.Report.Output ("Unregistered object", True);
      end;

      --  Destroy object adapter
      PolyORB.Obj_Adapters.Destroy (Obj_Adapter);
      PolyORB.Report.Output ("Destroyed Object Adapter", True);
   end Test_SOA;


begin
   PolyORB.Initialization.Initialize_World;

   Test_SOA;
   PolyORB.Report.End_Report;

exception
   when E : others =>
      Put_Line ("Got exception "
                & Exception_Name (E)
                & " : "
                & Exception_Message (E));
      PolyORB.Report.Output ("END TESTS", False);

end Test000;
