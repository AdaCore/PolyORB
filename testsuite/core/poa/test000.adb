with PolyORB.POA.Basic_POA;
with PolyORB.POA_Types;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.POA_Config;
with PolyORB.POA_Config.Minimum;

with PolyORB.Initialization;
with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);

with PolyORB.Objects;
with PolyORB.Servants;
with PolyORB.Types;

with Report;
with Test_Servant;

procedure Test000 is

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
      Root_POA_Object : PolyORB.POA.Obj_Adapter_Access;

   begin
      PolyORB.POA_Config.Set_Configuration
        (new PolyORB.POA_Config.Minimum.Minimum_Configuration);

      --  Root POA creation
      Root_POA_Object := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
      PolyORB.POA.Basic_POA.Create
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA_Object.all)'Access);

      Report.Output ("RootPOA creation", True);

      --  Root POA destruction
      PolyORB.POA.Basic_POA.Destroy
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA_Object.all)'Access);

      Report.Output ("RootPOA destruction", True);

   exception
      when others =>
         Report.Output ("RootPOA creation/destruction", False);
         raise;

   end Test_Root_POA;

   --------------------
   -- Test_Child_POA --
   --------------------

   procedure Test_Child_POA;
   --  Test the construction/destruction of the ROOT POAs.

   procedure Test_Child_POA is

      use PolyORB.POA;
      use PolyORB.POA.Basic_POA;
      use PolyORB.POA_Manager;
      use PolyORB.POA_Policies;
      use PolyORB.POA_Policies.Policy_Sequences;

      Root_POA : PolyORB.POA.Obj_Adapter_Access;

      OA1, OA2, OA3 : Obj_Adapter_Access;
      Policies      : PolicyList;
      PM1           : POAManager_Access;
      Ok            : Boolean := False;
   begin
      --  Root POA creation.
      Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
      PolyORB.POA.Basic_POA.Create
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);
      Report.Output ("RootPOA creation", True);

      --  Construct policy list.
      Append (Policies,
              Policy_Access (Root_POA.Thread_Policy));

      Append (Policies,
              Policy_Access (Root_POA.Lifespan_Policy));

      PM1 := POAManager_Access (Entity_Of (Root_POA.POA_Manager));

      --  POA1 Creation.
      OA1 := PolyORB.POA.Basic_POA.Create_POA
        (Basic_Obj_Adapter (Root_POA.all)'Access,
         To_PolyORB_String ("POA1"), PM1, Policies);
      Report.Output ("POA1 creation", True);

      --  POA2 Creation.
      OA2 := PolyORB.POA.Basic_POA.Create_POA
        (Basic_Obj_Adapter (Root_POA.all)'Access,
         To_PolyORB_String ("POA2"), PM1, Policies);
      Report.Output ("POA2 creation", True);

      --  POA3 Creation.
      OA3 := PolyORB.POA.Basic_POA.Create_POA
        (Basic_Obj_Adapter (OA1.all)'Access,
         To_PolyORB_String ("POA3"), PM1, Policies);
      Report.Output ("POA3 creation", True);

      begin
         OA2 := PolyORB.POA.Basic_POA.Create_POA
           (Basic_Obj_Adapter (OA1.all)'Access,
            To_PolyORB_String ("PO3"), PM1, Policies);

      exception
         when Adapter_Already_Exists =>
            Ok := True;
         when others =>
            raise;
      end;
      Report.Output ("Raised Adapter_Already_Exists", Ok);

      if OA2.POA_Manager /= OA1.POA_Manager then
         raise Incorrect_Execution;
      end if;
      Report.Output ("Same POA Manager", True);

      --  POA recursive destruction.
      PolyORB.POA.Basic_POA.Destroy
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

      Report.Output ("POA recursive destruction", True);

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
      use PolyORB.POA.Basic_POA;
      use Test_Servant;

      S1  : My_Servant_Access;
   begin

      S1 := new My_Servant;
      S1.Nb    := 1;
      S1.Name  := To_PolyORB_String ("Servant1");

      declare
         Root_POA : Obj_Adapter_Access;

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Basic_POA.Create
            (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                               PolyORB.Servants.Servant_Access (S1));

            Ok : Boolean := False;
         begin
            Report.Output ("Servant activation", True);

            Deactivate_Object (Basic_Obj_Adapter (Root_POA.all)'Access, Id1);
            Report.Output ("Servant deactivation", True);

            begin
               Deactivate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                                  Id1);
            exception
               when Object_Not_Active =>
                  Ok := True;
            end;
            Report.Output ("Raised Object_Not_Active", Ok);

            Destroy (Root_POA, True, True);

         end;
      end;

      declare
         Root_POA : Obj_Adapter_Access;

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Basic_POA.Create
            (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

         declare
            pragma Warnings (Off);
            Id1 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                               PolyORB.Servants.Servant_Access (S1));

            Id2 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                               PolyORB.Servants.Servant_Access (S1));
            pragma Unreferenced (Id1);
            pragma Unreferenced (Id2);
            pragma Warnings (On);

         begin
            Destroy (Root_POA, True, True);
         end;

      exception
         when Servant_Already_Active =>
            Report.Output ("Raised Servant_Already_Active", True);

         when others =>
            Report.Output ("Raised Servant_Already_Active", False);
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
      use PolyORB.POA.Basic_POA;
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
         PolyORB.POA.Basic_POA.Create
           (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id :=
              Activate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                               PolyORB.Servants.Servant_Access (S1));

            Id2 : constant PolyORB.POA_Types.Object_Id
              := Servant_To_Id (Basic_Obj_Adapter (Root_POA.all)'Access,
                                PolyORB.Servants.Servant_Access (S1));
         begin
            if Id1 /= Id2 then
               raise Incorrect_Execution;
            end if;
            Destroy (Root_POA, True, True);
            Report.Output ("Servant_To_Id", True);
         end;
      end;

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Basic_POA.Create
           (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

         declare
            pragma Warnings (Off);
            Id2 : constant PolyORB.POA_Types.Object_Id
              := Servant_To_Id (Basic_Obj_Adapter (Root_POA.all)'Access,
                                PolyORB.Servants.Servant_Access (S1));

            pragma Unreferenced (Id2);
            pragma Warnings (On);
         begin
            Destroy (Root_POA, True, True);
         end;
      end;
      Report.Output ("Raised Servant_Not_Active", False);

   exception
      when Servant_Not_Active =>
         Report.Output ("Raised Servant_Not_Active", True);

      when others =>
         Report.Output ("Raised Servant_Not_Active", False);
         raise;

   end Test_Servant_To_Id;

   ------------------------
   -- Test_Id_To_Servant --
   ------------------------

   procedure Test_Id_To_Servant;

   procedure Test_Id_To_Servant
   is
      use PolyORB.POA;
      use PolyORB.POA.Basic_POA;
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
         PolyORB.POA.Basic_POA.Create
           (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id
              := Activate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                                  PolyORB.Servants.Servant_Access (S1));
            S2 : constant My_Servant_Access
              := My_Servant_Access (Id_To_Servant
                                    (Basic_Obj_Adapter (Root_POA.all)'Access,
                                     Id1));
         begin
            if S1 /= S2 then
               Report.Output ("Id_to_Servant", False);
               raise Incorrect_Execution;
            end if;

            Destroy (Root_POA, True, True);
            Report.Output ("Id_to_Servant", True);
         end;
      end;

      begin
         Root_POA := new PolyORB.POA.Basic_POA.Basic_Obj_Adapter;
         PolyORB.POA.Basic_POA.Create
           (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Root_POA.all)'Access);

         declare
            Id1 : constant PolyORB.POA_Types.Object_Id
              := Activate_Object (Basic_Obj_Adapter (Root_POA.all)'Access,
                                  PolyORB.Servants.Servant_Access (S1));
            S2 : My_Servant_Access;
         begin
            Deactivate_Object (Basic_Obj_Adapter (Root_POA.all)'Access, Id1);
            S2 := My_Servant_Access
              (Id_To_Servant (Basic_Obj_Adapter (Root_POA.all)'Access, Id1));
            Destroy (Root_POA, True, True);
         exception
            when Object_Not_Active =>
               Destroy (Root_POA, True, True);
               Ok := False;
         end;

         Report.Output ("Raised Object_Not_Active", Ok);
      end;

   exception
      when others =>
         null;

   end Test_Id_To_Servant;

begin
   PolyORB.Initialization.Initialize_World;

   Test_Root_POA;
   Test_Child_POA;
   Test_Activate_Object;
--   Test_Activate_Object_With_Id;
   Test_Servant_To_Id;
   Test_Id_To_Servant;
   Report.End_Report;

end Test000;
