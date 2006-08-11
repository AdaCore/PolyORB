------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TERMINATION_MANAGER.BOOTSTRAP                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with Ada.Unchecked_Conversion;
with PolyORB.Annotations;
with PolyORB.Binding_Data.Neighbour;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.DSA_P.Exceptions;
with PolyORB.Errors;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.POA;
with PolyORB.POA_Config;
with PolyORB.POA_Config.RACWs;
with PolyORB.POA_Manager;
with PolyORB.QoS.Term_Manager_Info;
with PolyORB.References.Binding;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Threads;
with System.Partition_Interface;

package body PolyORB.Termination_Manager.Bootstrap is

   use PolyORB.Binding_Data;
   use PolyORB.Binding_Objects;
   use PolyORB.Log;
   use PolyORB.ORB;
   use PolyORB.Servants;
   use PolyORB.Setup;
   use System.Partition_Interface;

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.termination_manager.bootstrap");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -------------------------
   -- Stub Types managing --
   -------------------------

   type RACW_Tick_Stub_Type_Access is
     access all Term_Manager_Access'Stub_Type;

   --  We have to consider three views of the same type:
   --    * RACW_Tick_Stub_Type_Access: the type returned by RACW'Stub_Type
   --    * RACW_Stub_Type_Access: a general stub type used by S-PolInt
   --    * Term_Manager_Access : the type we use in the termination manager
   --  We define some Unchecked_Conversions between them:

   pragma Warnings (Off);

   --  To disable "possible aliasing problem" warnings which do not apply in
   --  this case.

   function RACW_Tick_To_RACW_Access is
     new Ada.Unchecked_Conversion
       (Source => RACW_Tick_Stub_Type_Access,
        Target => RACW_Stub_Type_Access);

   function RACW_Access_To_TM_Access is
     new Ada.Unchecked_Conversion
       (Source => RACW_Stub_Type_Access,
        Target => Term_Manager_Access);

   pragma Warnings (On);

   function Term_Manager_To_Address is
     new Ada.Unchecked_Conversion (Term_Manager_Access, System.Address);

   ----------------------------------
   -- Extract_TM_Reference_From_BO --
   ----------------------------------

   procedure Extract_TM_Reference_From_BO
      (BO  :     Binding_Object_Access;
       Ref : out References.Ref;
       NK  : out Node_Kind)
   is
      use Annotations;
      use Binding_Data.Neighbour;
      use Errors;
      use QoS.Term_Manager_Info;
      use References;

      BO_Ref  : Smart_Pointers.Ref;
      P       : Neighbour_Profile_Type;
      Note    : BO_Note;

   begin

      pragma Assert (BO /= null);

      if Get_Profile (BO) = null then
         pragma Debug (O ("Extracting TM ref from Server BO"));

         --  BO is a server side BO

         Enter_BO_Note_Lock;
         begin
            Get_Note (Notepad_Of (BO).all, Note);
         exception
            when others =>
               Leave_BO_Note_Lock;
               NK := Non_DSA_Node;
               return;
         end;

         Leave_BO_Note_Lock;

         if References.Is_Nil (Note.TM_Ref) then
            NK := DSA_Node_Without_TM;
            return;
         end if;

         NK := DSA_Node;
         Ref := Note.TM_Ref;
      else
         pragma Debug (O ("Extracting TM ref from Client BO"));

         --  BO is a client side BO

         Smart_Pointers.Set (BO_Ref, Entity_Ptr (BO));

         --  We create a new Neighbour profile, which always bind to the given
         --  Binding Object

         Create_Neighbour_Profile (BO  => BO_Ref,
                                   Oid => The_TM_Oid.all,
                                   P   => P);

         --  Then we construct a reference using the Neighbour Profile

         declare
            P_Array : constant Profile_Array := (1 => Duplicate_Profile (P));
         begin
            Create_Reference (Profiles => P_Array,
                              Type_Id  => RACW_Type_Name,
                              R        => Ref);
         end;

         NK := Unknown;

         --  In the client BO case, we cannot determine the kind of the target
         --  node.

      end if;

      pragma Debug (O ("Extracted Ref is:" & Image (Ref)));
   end Extract_TM_Reference_From_BO;

   ---------------
   --  Shutdown --
   ---------------

   procedure Shutdown (Wait_For_Completion : Boolean);

   procedure Shutdown (Wait_For_Completion : Boolean) is
      pragma Unreferenced (Wait_For_Completion);
   begin
      The_TM.Terminated := True;
   end Shutdown;

   ------------------------------------
   -- Initialize_Termination_Manager --
   ------------------------------------

   procedure Initialize_Termination_Manager
   is
      use PolyORB.Errors;
      use PolyORB.Objects;
      use PolyORB.Parameters;
      use PolyORB.References.Binding;

      TM           : constant Term_Manager_Ptr := new Term_Manager;
      S            : Components.Component_Access;
      Pro          : Binding_Data.Profile_Access;
      Error        : Error_Container;

      --  Retrieve the termination configuration parameters

      Is_Initiator : constant Boolean :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "termination_initiator",
           Default => False);

      Term_Policy : constant String :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "termination_policy",
           Default => "global_termination");

      Time_Between_Waves : constant Duration :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "tm_time_between_waves",
           Default => 1.0);

      Time_Before_Start : constant Duration :=
        Parameters.Get_Conf
          (Section => "dsa",
           Key     => "tm_time_before_start",
           Default => 5.0);

      function Term_Policy_Value (S : String) return Termination_Type;

      function Term_Policy_Value (S : String) return Termination_Type is
      begin
         return Termination_Type'Value (S);
      exception
         when others =>
            return Global_Termination;
      end Term_Policy_Value;

   begin
      pragma Debug (O ("Initialize enter"));

      if not Tasking_Available then
         if Term_Policy_Value (Term_Policy) = Local_Termination then

            --  If our profile is a no_tasking node with local_termination
            --  then there is nothing more to do!

            pragma Debug (O ("Detected No-Tasking local termination node."));
            return;
         else

            --  Except local_termination, all the others termination policies
            --  require tasking.

            O ("Only Local_Termination policy can be used in a "
               & "No_Tasking partition", Log.Error);

            raise Program_Error;
         end if;
      end if;

      --  Register a new Termination Manager for this partition

      The_TM := TM;
      The_TM_Ref := Term_Manager_Access_To_Ref (Term_Manager_Access (TM));
      The_TM_Oid := new Object_Id'(String_To_Oid (TM_Name_Space));

      --  We need the servant of TM so we can initiate a well known service
      --  pointing to it. We bind the reference and get the servant of TM.

      Bind (R          => The_TM_Ref,
            Local_ORB  => The_ORB,
            Servant    => S,
            Qos        => (others => null),
            Pro        => Pro,
            Local_Only => True,
            Error      => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  Start the Well Known Service

      pragma Debug (O ("Initiating Well Known Service"));
      Initiate_Well_Known_Service
        (S    => Servants.Servant_Access (S),
         Name => TM_Name_Space);

      --  Start the termination manager

      Start (Term_Manager_Access (TM),
             Term_Policy_Value (Term_Policy),
             Is_Initiator,
             Time_Between_Waves,
             Time_Before_Start);

      Register_Termination_Manager
        (The_TM_Ref,
         The_TM_Oid,
         Term_Manager_To_Address (Term_Manager_Access (TM)),
         Shutdown'Access);

      pragma Debug (O ("Initialize leave"));
   end Initialize_Termination_Manager;

   ---------------------------------
   -- Initiate_Well_Known_Service --
   ---------------------------------

   procedure Initiate_Well_Known_Service
     (S    : Servants.Servant_Access;
      Name : String)
   is
      use PolyORB.Errors;
      use PolyORB.POA;
      use PolyORB.POA_Config;
      use PolyORB.POA_Manager;
      use PolyORB.POA_Config.RACWs;

      POA : Obj_Adapter_Access;
      Error : Error_Container;
   begin
      Create_POA
        (Self         => Obj_Adapter_Access (Object_Adapter (The_ORB)),
         Adapter_Name => Name,
         A_POAManager => null,
         Policies     => Default_Policies (RACW_POA_Config.all),
         POA          => POA,
         Error        => Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;

      POA.Default_Servant := S;

      Activate (POAManager_Access (Entity_Of (POA.POA_Manager)), Error);

      if Found (Error) then
         PolyORB.DSA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Initiate_Well_Known_Service;

   --------------------------------
   -- Ref_To_Term_Manager_Access --
   --------------------------------

   function Ref_To_Term_Manager_Access (R : References.Ref)
     return Term_Manager_Access
   is
      The_Stub : constant RACW_Tick_Stub_Type_Access
        := new Term_Manager_Access'Stub_Type;
      --  A Stub of Term Manager type

      The_Same_Stub : RACW_Stub_Type_Access;
   begin

      --  We convert it to the general S-Pol_Int stub type so we can access the
      --  target Field.

      The_Same_Stub := RACW_Tick_To_RACW_Access (The_Stub);

      --  We manually increment R reference counter since we don't want the
      --  reference finalized while we got an RACW using it.

      Smart_Pointers.Inc_Usage
        (References.Entity_Of (R));

      --  Finally, we assign the reference to the Stub target field

      The_Same_Stub.Target := References.Entity_Of (R);

      return RACW_Access_To_TM_Access (The_Same_Stub);
   end Ref_To_Term_Manager_Access;

   -----------------------
   -- Tasking_Available --
   -----------------------

   function Tasking_Available return Boolean
   is
      use PolyORB.Tasking.Threads;
   begin
      return Current_Task /= Null_Thread_Id;
   end Tasking_Available;

   --------------------------------
   -- Term_Manager_Access_To_Ref --
   --------------------------------

   function Term_Manager_Access_To_Ref (TM : Term_Manager_Access)
     return References.Ref
   is
      Receiver     : System.Partition_Interface.Servant_Access;
      Result       : References.Ref;
   begin

      --  We retrieve the receiver stub of Term_Manager racw for this partition

      Receiver := Retrieve_Receiving_Stub (RACW_Type_Name, Obj_Stub);
      pragma Assert (Receiver /= null);

      --  Then use it to get a reference to TM

      Get_Reference (Addr     => Term_Manager_To_Address (TM),
                     Typ      => RACW_Type_Name,
                     Receiver => Receiver,
                     Ref      => Result);

      return Result;
   end Term_Manager_Access_To_Ref;

end PolyORB.Termination_Manager.Bootstrap;
