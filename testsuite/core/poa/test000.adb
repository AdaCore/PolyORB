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

with PolyORB.POA.Basic_POA;
with PolyORB.POA_Types;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.POA_Config.Minimum;

with PolyORB.Initialization;
with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);

with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.Types;

with PolyORB.Report;
with Test_Servant;

procedure Test000 is

   use Ada.Text_IO;
   use Ada.Exceptions;
   use PolyORB.Types;

   Incorrect_Execution : exception;
   Correct_Execution   : exception;

   -------------------
   -- Test_Root_POA --
   -------------------

   procedure Test_Root_POA;
   --  Test the construction/destruction of the ROOT POA.

   procedure Test_Root_POA
   is
      Root_POA : constant PolyORB.POA.Obj_Adapter_Access
        := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

   begin
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Minimum.Minimum_Configuration);

      --  Root POA creation
      PolyORB.POA.Create (Root_POA);
      PolyORB.Report.Output ("RootPOA creation", True);

      --  Root POA destruction
      PolyORB.POA.Destroy (Root_POA);
      PolyORB.Report.Output ("RootPOA destruction", True);

   exception
      when others =>
         PolyORB.Report.Output ("RootPOA creation/destruction", False);
         raise;

   end Test_Root_POA;

   --------------------
   -- Test_Child_POA --
   --------------------

   procedure Test_Child_POA;
   --  Test the construction/destruction of the ROOT POAs.

   procedure Test_Child_POA is

      use PolyORB.POA;
      use PolyORB.POA_Manager;
      use PolyORB.POA_Policies;
      use PolyORB.POA_Policies.Policy_Sequences;

      Root_POA : constant PolyORB.POA.Obj_Adapter_Access
        := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

      OA1, OA2, OA3 : Obj_Adapter_Access;
      Policies      : PolicyList;
      PM1           : POAManager_Access;
      Ok            : Boolean := False;
   begin
      --  Root POA creation.
      PolyORB.POA.Create (Root_POA);

      --  Construct policy list.
      Append (Policies,
              Policy_Access (Root_POA.Thread_Policy));

      Append (Policies,
              Policy_Access (Root_POA.Lifespan_Policy));

      PM1 := POAManager_Access (Entity_Of (Root_POA.POA_Manager));

      --  POA1 Creation.
      OA1 := PolyORB.POA.Create_POA
        (Root_POA, To_PolyORB_String ("POA1"), PM1, Policies);

      --  POA2 Creation.
      OA2 := PolyORB.POA.Create_POA
        (OA1, To_PolyORB_String ("POA2"), null, Policies);

      --  POA3 Creation.
      OA3 := PolyORB.POA.Create_POA
        (OA1, To_PolyORB_String ("POA3"), PM1, Policies);

      PolyORB.Report.Output ("Child POA construction", True);

      Ok := False;
      begin
         OA2 := PolyORB.POA.Create_POA
           (OA1, To_PolyORB_String ("POA3"), PM1, Policies);

      exception
         when Adapter_Already_Exists =>
            Ok := True;
         when others =>
            raise;
      end;
      PolyORB.Report.Output ("Raised Adapter_Already_Exists", Ok);

      Ok := False;
      if OA1.POA_Manager = OA3.POA_Manager then
         Ok := True;
      end if;
      PolyORB.Report.Output ("Same POA Manager", Ok);

      Ok := False;
      if OA1.POA_Manager /= OA2.POA_Manager then
         Ok := True;
      end if;
      PolyORB.Report.Output ("Implicit creation of a POA Manager", Ok);

      --  POA recursive destruction.
      PolyORB.POA.Destroy (Root_POA);
      PolyORB.Report.Output ("POA recursive destruction", True);

   exception
      when others =>
         raise;
   end Test_Child_POA;

   --------------------------
   -- Test_Activate_Object --
   --------------------------

   procedure Test_Activate_Object;
   --  Test simple activation/deactivation of an object.

   procedure Test_Activate_Object
   is
      use PolyORB.POA;
      use Test_Servant;

      S1  : My_Servant_Access;
   begin

      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      declare
         Root_POA : constant PolyORB.POA.Obj_Adapter_Access
           := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

      begin
         PolyORB.POA.Create (Root_POA);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Root_POA,
                               PolyORB.Servants.Servant_Access (S1));

            Ok : Boolean := False;
         begin
            PolyORB.Report.Output ("Servant activation", True);

            Deactivate_Object (Root_POA, Id1);
            PolyORB.Report.Output ("Servant deactivation", True);

            begin
               Deactivate_Object (Root_POA, Id1);

            exception
               when Object_Not_Active =>
                  Ok := True;
            end;
            PolyORB.Report.Output ("Raised Object_Not_Active", Ok);

            PolyORB.POA.Destroy (Root_POA);

         exception
            when others =>
               raise;
         end;
      end;

      declare
         Root_POA : constant PolyORB.POA.Obj_Adapter_Access
           := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

      begin
         PolyORB.POA.Create (Root_POA);

         declare
            pragma Warnings (Off);
            Id1 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Root_POA,
                               PolyORB.Servants.Servant_Access (S1));

            Id2 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Root_POA,
                               PolyORB.Servants.Servant_Access (S1));
            pragma Unreferenced (Id1);
            pragma Unreferenced (Id2);
            pragma Warnings (On);

         begin
            PolyORB.POA.Destroy (Root_POA);
         end;

         PolyORB.Report.Output ("Raised Servant_Already_Active", False);
      exception
         when Servant_Already_Active =>
            PolyORB.Report.Output ("Raised Servant_Already_Active", True);

         when others =>
            raise;
      end;

   end Test_Activate_Object;

   ----------------------------------
   -- Test_Activate_Object_With_Id --
   ----------------------------------
   --  XXX not implemented in PolyORB !!

--    procedure Test_Activate_Object_With_Id;

--    procedure Test_Activate_Object_With_Id
--    is
--       use Test_Servant;
--    begin
--       declare
--          use PolyORB.POA;
--          use PolyORB.POA.Basic_POA;
--          OA1    : Obj_Adapter_Access;
--          S1, S2 : My_Servant_Access;
--       begin
--          S1 := new My_Servant;
--          S1.Nb    := 1;
--          S1.Name  := To_PolyORB_String ("Servant1");

--          S2 := new My_Servant;
--          S2.Nb    := 2;
--          S2.Name  := To_PolyORB_String ("Servant2");

--          declare
--             use PolyORB.POA;
--             OA1 : Obj_Adapter_Access := Create_Root_POA;
--             Id1 : PolyORB.POA_Types.Object_Id
--               := Activate_Object (OA1.all'Access,
--                                   PolyORB.Servants.Servant_Access (S1));
--          begin

--             Deactivate_Object (OA1.all'Access, Id1);
--             Activate_Object_With_Id (OA1.all'Access,
--                                      PolyORB.Servants.Servant_Access (S1),
--                                      Id1);
--             Destroy (OA1, True, True);
--          end;

--          declare
--             use PolyORB.POA;
--             OA1 : Obj_Adapter_Access := Create_Root_POA;
--             Id1 : PolyORB.POA_Types.Object_Id
--               := Activate_Object (OA1.all'Access,
--                                   PolyORB.Servants.Servant_Access (S1));
--          begin
--             Activate_Object_With_Id (OA1.all'Access,
--                                      PolyORB.Servants.Servant_Access (S2),
--                                      Id1);
--             Destroy (OA1, True, True);
--          exception
--             when Object_Already_Active =>
--                null;
--          end;

--          declare
--             use PolyORB.POA;
--             OA1 : Obj_Adapter_Access := Create_Root_POA;
--             pragma Warnings (Off);
--             Id1 : PolyORB.POA_Types.Object_Id
--               := Activate_Object (OA1.all'Access,
--                                   PolyORB.Servants.Servant_Access (S1));
--             pragma Warnings (On);
--             Id2 : PolyORB.POA_Types.Object_Id
--               := Activate_Object (OA1.all'Access,
--                                   PolyORB.Servants.Servant_Access (S2));
--          begin
--             Deactivate_Object (OA1.all'Access, Id2);
--             Activate_Object_With_Id (OA1.all'Access,
--                                      PolyORB.Servants.Servant_Access (S1),
--                                      Id2);
--             Destroy (OA1, True, True);
--       exception
--             when Servant_Already_Active =>
--                null;
--          end;

--       exception
--          when others =>
--             null;
--       end;
--    end Test_Activate_Object_With_Id;

   ------------------------
   -- Test_Servant_To_Id --
   ------------------------

   procedure Test_Servant_To_Id;
   --  Test Servant_To_Id functions.

   procedure Test_Servant_To_Id
   is
      use PolyORB.POA;
      use Test_Servant;
      use PolyORB.Objects;

      S1  : My_Servant_Access;
      Root_POA : Obj_Adapter_Access;

   begin
      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Root_POA,
                               PolyORB.Servants.Servant_Access (S1));

            Id2 : constant PolyORB.POA_Types.Object_Id
              := Servant_To_Id (Root_POA,
                                PolyORB.Servants.Servant_Access (S1));
         begin
            if Id1 /= Id2 then
               raise Incorrect_Execution;
            end if;
            PolyORB.POA.Destroy (Root_POA);
            PolyORB.Report.Output ("Servant_To_Id", True);
         end;
      end;

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         declare
            pragma Warnings (Off);
            Id2 : constant PolyORB.POA_Types.Object_Id
              := Servant_To_Id (Root_POA,
                                PolyORB.Servants.Servant_Access (S1));

            pragma Unreferenced (Id2);
            pragma Warnings (On);
         begin
            PolyORB.POA.Destroy (Root_POA);
         end;
      end;
      PolyORB.Report.Output ("Raised Servant_Not_Active", False);

   exception
      when Servant_Not_Active =>
         PolyORB.Report.Output ("Raised Servant_Not_Active", True);

      when others =>
         PolyORB.Report.Output ("Raised Servant_Not_Active", False);
         raise;

   end Test_Servant_To_Id;

   ------------------------
   -- Test_Id_To_Servant --
   ------------------------

   procedure Test_Id_To_Servant;

   procedure Test_Id_To_Servant
   is
      use PolyORB.POA;
      use Test_Servant;

      S1  : My_Servant_Access;
      Root_POA : Obj_Adapter_Access;
      Ok : Boolean := False;
   begin

      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id
              := Activate_Object (Root_POA,
                                  PolyORB.Servants.Servant_Access (S1));
            S2 : constant My_Servant_Access
              := My_Servant_Access (Id_To_Servant (Root_POA, Id1));

         begin
            if S1 /= S2 then
               PolyORB.Report.Output ("Id_to_Servant", False);
            end if;

            PolyORB.POA.Destroy (Root_POA);
            PolyORB.Report.Output ("Id_to_Servant", True);
         end;
      end;

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id
              := Activate_Object (Root_POA,
                                  PolyORB.Servants.Servant_Access (S1));
            S2 : My_Servant_Access;

         begin
            Deactivate_Object (Root_POA, Id1);
            S2 := My_Servant_Access (Id_To_Servant (Root_POA, Id1));

            PolyORB.POA.Destroy (Root_POA);
         exception
            when Object_Not_Active =>
               Ok := True;

            when others =>
               raise;
         end;

         PolyORB.POA.Destroy (Root_POA);
         PolyORB.Report.Output ("Raised Object_Not_Active", Ok);
      end;

   exception
      when others =>
         raise;

   end Test_Id_To_Servant;

begin
   PolyORB.Initialization.Initialize_World;

   Test_Root_POA;
   Test_Child_POA;
   Test_Activate_Object;
   --  Test_Activate_Object_With_Id;
   Test_Servant_To_Id;
   Test_Id_To_Servant;

   PolyORB.Report.End_Report;

exception
   when E : others =>
      Put_Line ("Got exception "
                & Exception_Name (E)
                & " : "
                & Exception_Message (E));
      PolyORB.Report.Output ("END TESTS", False);

end Test000;
