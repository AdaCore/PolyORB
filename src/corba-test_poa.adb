------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C O R B A . T E S T _ P O A                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.POA.Basic_POA;
with PolyORB.POA;
with CORBA.Policy;
with PolyORB.POA_Types;
with PolyORB.POA_Manager;

with PolyORB.Log;
with PolyORB.No_Tasking;
with PolyORB.Smart_Pointers;
with PolyORB.Objects;

package body CORBA.Test_POA is

   ---------------------------------------------------
   --  Declare additional procedures and functions  --
   ---------------------------------------------------

   procedure Print_Title
     (Title : Standard.String);

   procedure Print_Test_Title
     (Test : Standard.String);

   procedure Print_Test_Text
     (Test   : Standard.String);

   procedure Print_Test_Result
     (Result : Standard.Boolean);

   -------------------------------
   --  Declare test procedures  --
   -------------------------------

   procedure Test_POA_Creation;
   procedure Test_POA_Destruction;
   procedure Test_Activate_Object;
   procedure Test_Activate_Object_With_Id;
   procedure Test_Deactivate_Object;
   procedure Test_Servant_To_Id;
   procedure Test_Id_To_Servant;

   ------------------
   -- Test_The_POA --
   ------------------

   procedure Test_The_POA
   is
   begin
      -------------------------------
      -- Initialize all subsystems --
      -------------------------------

      Print_Title ("Initialize subsystems");
      PolyORB.Log.Initialize;
      Put_Line ("Initialize logging");
      --  Logging subsystem. Start this one first so we can debug
      --  problems in others.

      PolyORB.No_Tasking.Initialize;
      Put_Line ("Use No-tasking");
      --  Setup soft links.

      PolyORB.Smart_Pointers.Initialize;
      Put_Line ("Initialize smart-pointers");
      --  Depends on Soft_Links.

      -----------------------
      --  Start the tests  --
      -----------------------

      Print_Title ("Test the POA");
      Test_POA_Creation;
      Test_POA_Destruction;
      Test_Activate_Object;
      Test_Activate_Object_With_Id;
      Test_Deactivate_Object;
      Test_Servant_To_Id;
      Test_Id_To_Servant;

   end Test_The_POA;

   --------------------------------------------
   --  Additionnal procedures and functions  --
   --------------------------------------------

   -----------------
   -- Print_Title --
   -----------------

   procedure Print_Title
     (Title : Standard.String)
   is
   begin
      New_Line;
      Put_Line
        ("------------------------------------------------------------------");
      Put      ("--  ");
      Put      (Title);
      Set_Col  (65);
      Put_Line ("--");
      Put_Line
        ("------------------------------------------------------------------");
      New_Line;
   end Print_Title;

   ----------------------
   -- Print_Test_Title --
   ----------------------

   procedure Print_Test_Title
     (Test : Standard.String)
   is
   begin
      Put_Line (Test);
      for I in Test'Range loop
         Put ("=");
      end loop;
      New_Line;
   end Print_Test_Title;

   ---------------------
   -- Print_Test_Text --
   ---------------------

   procedure Print_Test_Text
     (Test   : Standard.String)
   is
   begin
      Put ("  ");
      Put (Test);
   end Print_Test_Text;

   -----------------------
   -- Print_Test_Result --
   -----------------------

   procedure Print_Test_Result
     (Result : Standard.Boolean)
   is
   begin
      Set_Col (60);
      if Result = True then
         Put_Line ("Ok");
      else
         Put_Line ("Failed");
      end if;
   end Print_Test_Result;

   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Test_POA_Creation
   is
   begin
      Print_Test_Title ("Test POA creation");

      --  Root POA creation
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         OA1 : Obj_Adapter_Access;
         OA2 : aliased Basic_Obj_Adapter;
      begin
         Print_Test_Text ("RootPOA creation");
         OA1 := Create_Root_POA;
         Create (OA2'Access);
         Print_Test_Result (True);
      exception
         when others =>
            Print_Test_Result (False);
      end;

      --  POA tree
      declare
         use PolyORB.POA;
         use CORBA.Policy;
         use CORBA.Policy.Policy_Sequences;
         use PolyORB.POA.Basic_POA;
         use PolyORB.POA_Manager;
         OA1, OA2, OA3, OA4 : Obj_Adapter_Access;
         Policies           : PolicyList
           := new PolicyList;
         PM1                : POAManager_Access;
      begin
         Print_Test_Text ("POA tree");
         OA1 := Create_Root_POA;
         Append (Sequence (Policies.all), Policy_Access (OA1.Thread_Policy));
         Append (Sequence (Policies.all), Policy_Access (OA1.Lifespan_Policy));
         PM1 := OA1.POA_Manager;
         OA2 := Create_POA (OA1, To_CORBA_String ("FirstPOA"), PM1, Policies);
         OA3 := Create_POA (OA1, To_CORBA_String ("SecondPOA"), PM1, null);
         OA4 := Create_POA (OA3, To_CORBA_String ("ThirdPOA"), null, Policies);
         declare
         begin
            OA2 := Create_POA (OA3, To_CORBA_String ("ThirdPOA"),
                               null,
                               Policies);
         exception
            when Adapter_Already_Exists =>
               null;
            when others =>
               raise;
         end;
         if OA2.POA_Manager /= OA1.POA_Manager then
            raise Incorrect_Execution;
         end if;
         Print_Test_Result (True);
      exception
         when others =>
            Print_Test_Result (False);
      end;
   end Test_POA_Creation;

   --------------------------
   -- Test_POA_Destruction --
   --------------------------

   procedure Test_POA_Destruction
   is
   begin
      Print_Test_Title ("Test POA destruction");

      --  Single POA destruction
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         OA1 : Obj_Adapter_Access;
      begin
         Print_Test_Text ("Single POA destruction");
         begin
            OA1 := Create_Root_POA;
            Destroy (OA1, True, True);
         exception
            when others =>
               raise Incorrect_Execution;
         end;
         if OA1.Name /= "" then
            raise Incorrect_Execution;
         end if;
         Print_Test_Result (True);
      exception
         when Constraint_Error =>
            Print_Test_Result (True);
         when others =>
            Print_Test_Result (False);
      end;

      --  POA tree destruction
      declare
         use PolyORB.POA;
         use CORBA.Policy;
         use CORBA.Policy.Policy_Sequences;
         use PolyORB.POA.Basic_POA;
         use PolyORB.POA_Manager;
         OA1, OA2, OA3, OA4 : Obj_Adapter_Access;
         Policies           : PolicyList
           := new PolicyList;
         PM1                : POAManager_Access;
      begin
         Print_Test_Text ("POA tree destruction");
         OA1 := Create_Root_POA;
         Append (Sequence (Policies.all), Policy_Access (OA1.Thread_Policy));
         Append (Sequence (Policies.all), Policy_Access (OA1.Lifespan_Policy));
         PM1 := OA1.POA_Manager;
         OA2 := Create_POA (OA1, To_CORBA_String ("FirstPOA"), PM1, Policies);
         OA3 := Create_POA (OA1, To_CORBA_String ("SecondPOA"), PM1, null);
         OA4 := Create_POA (OA3, To_CORBA_String ("ThirdPOA"), null, Policies);
         Destroy (OA1, True, True);
         if OA1.Name /= "" then
            raise Incorrect_Execution;
         end if;
         if OA2.Name /= "" then
            raise Incorrect_Execution;
         end if;
         if OA3.Name /= "" then
            raise Incorrect_Execution;
         end if;
         if OA4.Name /= "" then
            raise Incorrect_Execution;
         end if;
         --  Note: this doesn't check that the components have been entirely
         --  freed...
         Print_Test_Result (True);
      exception
         when others =>
            Print_Test_Result (False);
      end;
   end Test_POA_Destruction;

   --------------------------
   -- Test_Activate_Object --
   --------------------------

   procedure Test_Activate_Object
   is
   begin
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         S1  : My_Servant_Access;
      begin
         Print_Test_Text ("Test Activate_Object");

         S1 := new My_Servant;
         S1.Nb    := 1;
         S1.Name  := To_CORBA_String ("Servant1");

         declare
            use PolyORB.POA;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            pragma Warnings (Off);
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            pragma Warnings (On);
         begin
            Destroy (OA1, True, True);
         end;

         declare
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            pragma Warnings (Off);
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            Id2 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            pragma Warnings (On);
         begin
            Destroy (OA1, True, True);
         end;
         Print_Test_Result (False);
      exception
         when Servant_Already_Active =>
            Print_Test_Result (True);
         when others =>
            Print_Test_Result (False);
      end;
   end Test_Activate_Object;

   ----------------------------------
   -- Test_Activate_Object_With_Id --
   ----------------------------------

   procedure Test_Activate_Object_With_Id
   is
   begin
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         OA1    : Obj_Adapter_Access;
         S1, S2 : My_Servant_Access;
      begin
         Print_Test_Text ("Test Activate_Object_With_Id");
         OA1 := Create_Root_POA;

         S1 := new My_Servant;
         S1.Nb    := 1;
         S1.Name  := To_CORBA_String ("Servant1");

         S2 := new My_Servant;
         S2.Nb    := 2;
         S2.Name  := To_CORBA_String ("Servant2");

         declare
            use PolyORB.POA;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
         begin
            Deactivate_Object (OA1.all'Access, Id1);
            Activate_Object_With_Id (OA1.all'Access,
                                     PolyORB.POA_Types.Servant_Access (S1),
                                     Id1);
            Destroy (OA1, True, True);
         end;

         declare
            use PolyORB.POA;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
         begin
            Activate_Object_With_Id (OA1.all'Access,
                                     PolyORB.POA_Types.Servant_Access (S2),
                                     Id1);
            Destroy (OA1, True, True);
         exception
            when Object_Already_Active =>
               null;
         end;

         declare
            use PolyORB.POA;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            pragma Warnings (Off);
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            pragma Warnings (On);
            Id2 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S2));
         begin
            Deactivate_Object (OA1.all'Access, Id2);
            Activate_Object_With_Id (OA1.all'Access,
                                     PolyORB.POA_Types.Servant_Access (S1),
                                     Id2);
            Destroy (OA1, True, True);
      exception
            when Servant_Already_Active =>
               null;
         end;

         Print_Test_Result (True);
      exception
         when others =>
            Print_Test_Result (False);
      end;
   end Test_Activate_Object_With_Id;

   ----------------------------
   -- Test_Deactivate_Object --
   ----------------------------

   procedure Test_Deactivate_Object
   is
   begin
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         S1  : My_Servant_Access;
      begin
         Print_Test_Text ("Test Deactivate_Object");

         S1 := new My_Servant;
         S1.Nb    := 1;
         S1.Name  := To_CORBA_String ("Servant1");

         declare
            use PolyORB.POA;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            pragma Warnings (Off);
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            pragma Warnings (On);
         begin
            Deactivate_Object (OA1.all'Access, Id1);
            Destroy (OA1, True, True);
         end;

         declare
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            pragma Warnings (Off);
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            pragma Warnings (On);
         begin
            Deactivate_Object (OA1.all'Access, Id1);
            Deactivate_Object (OA1.all'Access, Id1);
            Destroy (OA1, True, True);
         exception
            when Object_Not_Active =>
               null;
         end;
         Print_Test_Result (True);
      exception
         when others =>
            Print_Test_Result (False);
      end;
   end Test_Deactivate_Object;

   ------------------------
   -- Test_Servant_To_Id --
   ------------------------

   procedure Test_Servant_To_Id
   is
   begin
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         S1  : My_Servant_Access;
      begin
         Print_Test_Text ("Test Servant_To_Id");

         S1 := new My_Servant;
         S1.Nb    := 1;
         S1.Name  := To_CORBA_String ("Servant1");

         declare
            use PolyORB.POA;
            use PolyORB.Objects;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            Id2 : PolyORB.POA_Types.Object_Id
              := Servant_To_Id (OA1.all'Access,
                                PolyORB.POA_Types.Servant_Access (S1));
         begin
            if Id1 /= Id2 then
               raise Incorrect_Execution;
            end if;
            Destroy (OA1, True, True);
         end;

         declare
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            pragma Warnings (Off);
            Id2 : PolyORB.POA_Types.Object_Id
              := Servant_To_Id (OA1.all'Access,
                                PolyORB.POA_Types.Servant_Access (S1));
            pragma Warnings (On);
         begin
            Destroy (OA1, True, True);
         end;
         Print_Test_Result (False);
      exception
         when Servant_Not_Active =>
            Print_Test_Result (True);
         when others =>
            Print_Test_Result (False);
      end;
   end Test_Servant_To_Id;

   ------------------------
   -- Test_Id_To_Servant --
   ------------------------

   procedure Test_Id_To_Servant
   is
   begin
      declare
         use PolyORB.POA;
         use PolyORB.POA.Basic_POA;
         S1  : My_Servant_Access;
      begin
         Print_Test_Text ("Test Id_To_Servant");

         S1 := new My_Servant;
         S1.Nb    := 1;
         S1.Name  := To_CORBA_String ("Servant1");

         declare
            use PolyORB.POA;
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            S2 : My_Servant_Access
              := My_Servant_Access
              (Id_To_Servant (OA1.all'Access, Id1));
         begin
            if S1 /= S2 then
               raise Incorrect_Execution;
            end if;
            Destroy (OA1, True, True);
         end;

         declare
            OA1 : Obj_Adapter_Access := Create_Root_POA;
            Id1 : PolyORB.POA_Types.Object_Id
              := Activate_Object (OA1.all'Access,
                                  PolyORB.POA_Types.Servant_Access (S1));
            S2 : My_Servant_Access;
         begin
            Deactivate_Object (OA1.all'Access, Id1);
            S2 := My_Servant_Access
              (Id_To_Servant (OA1.all'Access, Id1));
            Destroy (OA1, True, True);
         exception
            when Object_Not_Active =>
               Destroy (OA1, True, True);
         end;
         Print_Test_Result (True);
      exception
         when others =>
            Print_Test_Result (False);
      end;
   end Test_Id_To_Servant;

   --------------------------------------------------------
   --  Procedures and functions required by the Servant  --
   --------------------------------------------------------

   --------------------
   -- Handle_Message --
   --------------------

   pragma Warnings (Off);
   function Handle_Message
     (S   : access My_Servant;
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
   begin
      return Handle_Message (S, Msg);
   end Handle_Message;
   pragma Warnings (On);

   ----------
   -- Left --
   ----------

   function "=" (Left, Right : My_Servant)
                return Standard.Boolean
   is
   begin
      if Left.Nb = Right.Nb
        and then Left.Name = Right.Name
      then
         return True;
      end if;
      return False;
   end "=";

end CORBA.Test_POA;
