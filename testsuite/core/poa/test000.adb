------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with Ada.Text_IO;

with PolyORB.POA_Types;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.POA_Config.Minimum;

with PolyORB.Errors;
with PolyORB.Initialization;
with PolyORB.Servants;
with PolyORB.Types;
with PolyORB.Utils.Report;

with PolyORB.Setup.Base;
pragma Warnings (Off, PolyORB.Setup.Base);

with PolyORB.ORB.No_Tasking;
pragma Warnings (Off, PolyORB.ORB.No_Tasking);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);

with PolyORB.Setup.Tasking.No_Tasking;
pragma Warnings (Off, PolyORB.Setup.Tasking.No_Tasking);

with PolyORB.POA.Basic_POA;
--  POA to be tested

with Test_Servant;

procedure Test000 is

   use Ada.Text_IO;
   use Ada.Exceptions;

   use PolyORB.Errors;
   use PolyORB.Types;
   use PolyORB.Utils.Report;

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
      Output ("RootPOA creation", True);

      --  Root POA destruction
      PolyORB.POA.Destroy (Root_POA);
      Output ("RootPOA destruction", True);

   exception
      when others =>
         Output ("RootPOA creation/destruction", False);
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
      use PolyORB.POA_Policies.Policy_Lists;

      Root_POA : constant PolyORB.POA.Obj_Adapter_Access
        := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

      OA1, OA2, OA3 : Obj_Adapter_Access;
      Policies      : PolicyList;
      PM1           : POAManager_Access;
      Ok            : Boolean := False;

      Error : Error_Container;
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
      PolyORB.POA.Create_POA (Root_POA, "POA1", PM1, Policies, OA1, Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      --  POA2 Creation.
      PolyORB.POA.Create_POA (OA1, "POA2", null, Policies, OA2, Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      --  POA3 Creation.
      PolyORB.POA.Create_POA (OA1, "POA3", PM1, Policies, OA3, Error);

      if Found (Error) then
         raise Program_Error;
      end if;

      Output ("Child POA construction", True);

      PolyORB.POA.Create_POA (OA1, "POA3", PM1, Policies, OA2, Error);

      if Found (Error) then
         Ok := True;
      else
         Ok := False;
      end if;

      Output ("Raised Adapter_Already_Exists", Ok);

      Ok := False;
      if OA1.POA_Manager = OA3.POA_Manager then
         Ok := True;
      end if;
      Output ("Same POA Manager", Ok);

      Ok := False;
      if OA1.POA_Manager /= OA2.POA_Manager then
         Ok := True;
      end if;
      Output ("Implicit creation of a POA Manager", Ok);

      --  POA recursive destruction.
      PolyORB.POA.Destroy (Root_POA);
      Output ("POA recursive destruction", True);

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

      Ok : Boolean := False;

      Error : Error_Container;
   begin

      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      declare
         Root_POA : constant PolyORB.POA.Obj_Adapter_Access
           := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

         Id1 : PolyORB.POA_Types.Unmarshalled_Oid;

      begin
         PolyORB.POA.Create (Root_POA);

         Activate_Object (Root_POA,
                          PolyORB.Servants.Servant_Access (S1),
                          null,
                          Id1,
                          Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Output ("Servant activation", True);

         Deactivate_Object (Root_POA,
                            PolyORB.POA_Types.U_Oid_To_Oid (Id1),
                            Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Output ("Servant deactivation", True);

         Deactivate_Object (Root_POA,
                            PolyORB.POA_Types.U_Oid_To_Oid (Id1),
                            Error);

         if Found (Error) then
            Ok := True;
            Catch (Error);
         else
            Ok := False;
         end if;

         Output ("Raised Object_Not_Active", Ok);

         PolyORB.POA.Destroy (Root_POA);
      end;

      declare
         Root_POA : constant PolyORB.POA.Obj_Adapter_Access
           := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;

         Id1 : PolyORB.POA_Types.Unmarshalled_Oid;
         Id2 : PolyORB.POA_Types.Unmarshalled_Oid;

      begin
         PolyORB.POA.Create (Root_POA);

         Activate_Object (Root_POA,
                          PolyORB.Servants.Servant_Access (S1),
                          null,
                          Id1,
                          Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Activate_Object (Root_POA,
                          PolyORB.Servants.Servant_Access (S1),
                          null,
                          Id2,
                          Error);

         if Found (Error) then
            Output ("Raised Servant_Already_Active", True);
            Catch (Error);
         else
            Output ("Raised Servant_Already_Active", False);
         end if;

         PolyORB.POA.Destroy (Root_POA);
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
      use type PolyORB.POA_Types.Unmarshalled_Oid;

      use PolyORB.POA;

      use Test_Servant;

      S1  : My_Servant_Access;
      Root_POA : Obj_Adapter_Access;

      Error : Error_Container;

      Id1 : PolyORB.POA_Types.Unmarshalled_Oid;
      Id2 : PolyORB.POA_Types.Object_Id_Access;

   begin
      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         Activate_Object (Root_POA,
                          PolyORB.Servants.Servant_Access (S1),
                          null,
                          Id1,
                          Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Servant_To_Id (Root_POA,
                        PolyORB.Servants.Servant_Access (S1),
                        Id2,
                        Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         PolyORB.POA.Destroy (Root_POA);
         Output ("Servant_To_Id", True);
      end;

   end Test_Servant_To_Id;

   ------------------------
   -- Test_Id_To_Servant --
   ------------------------

   procedure Test_Id_To_Servant;

   procedure Test_Id_To_Servant
   is
      use type PolyORB.POA_Types.Unmarshalled_Oid;

      use PolyORB.POA;

      use Test_Servant;

      S1 : My_Servant_Access;
      S2 : My_Servant_Access;

      Error : Error_Container;

   begin

      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      declare
         Root_POA : Obj_Adapter_Access;

         Id1 : PolyORB.POA_Types.Unmarshalled_Oid;
      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         Activate_Object (Root_POA,
                          PolyORB.Servants.Servant_Access (S1),
                          null,
                          Id1,
                          Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Id_To_Servant (Root_POA,
                        PolyORB.POA_Types.U_Oid_To_Oid (Id1),
                        PolyORB.Servants.Servant_Access (S2),
                        Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         if S1 /= S2 then
            Output ("Id_to_Servant", False);
         else
            Output ("Id_to_Servant", True);
         end if;

         PolyORB.POA.Destroy (Root_POA);
      end;

      declare
         Root_POA : Obj_Adapter_Access;

         Id1 : PolyORB.POA_Types.Unmarshalled_Oid;
      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Create (Root_POA);

         Activate_Object (Root_POA,
                          PolyORB.Servants.Servant_Access (S1),
                          null,
                          Id1,
                          Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Deactivate_Object (Root_POA,
                            PolyORB.POA_Types.U_Oid_To_Oid (Id1),
                            Error);

         if Found (Error) then
            raise Program_Error;
         end if;

         Id_To_Servant (Root_POA,
                        PolyORB.POA_Types.U_Oid_To_Oid (Id1),
                        PolyORB.Servants.Servant_Access (S2),
                        Error);

         if Found (Error) then
            Output ("Got error", True);
            Catch (Error);
         else
            Output ("Got error", False);
         end if;

         PolyORB.POA.Destroy (Root_POA);
      end;
   end Test_Id_To_Servant;

begin
   PolyORB.Initialization.Initialize_World;

   Test_Root_POA;
   Test_Child_POA;
   Test_Activate_Object;
   --  Test_Activate_Object_With_Id;
   Test_Servant_To_Id;
   Test_Id_To_Servant;

   End_Report;

exception
   when E : others =>
      Put_Line ("Got exception "
                & Exception_Name (E)
                & " : "
                & Exception_Message (E));
      Output ("END TESTS", False);

end Test000;
