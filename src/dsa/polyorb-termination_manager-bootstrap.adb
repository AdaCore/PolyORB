------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TERMINATION_MANAGER.BOOTSTRAP                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2013, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Conversion;

with System.Partition_Interface;

with PolyORB.Annotations;
with PolyORB.Binding_Data.Neighbour;
with PolyORB.Binding_Data;
with PolyORB.DSA_P.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.QoS.Term_Manager_Info;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;

package body PolyORB.Termination_Manager.Bootstrap is

   use System.Partition_Interface;

   use PolyORB.Binding_Data;
   use PolyORB.Binding_Objects;
   use PolyORB.Log;
   use PolyORB.Servants;

   -------------
   -- Logging --
   -------------

   package L is new Log.Facility_Log ("polyorb.termination_manager.bootstrap");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
                renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
               renames L.Enabled;

   -------------------------
   -- Stub Types managing --
   -------------------------

   type Term_Manager_Stub_Access is
     access all Term_Manager_Access'Stub_Type;

   --  We have to consider three views of the same type:
   --    * Term_Manager_Stub_Access: the type returned by RACW'Stub_Type
   --    * RACW_Stub_Type_Access: a general stub type used by S-PolInt
   --    * Term_Manager_Access : the type we use in the termination manager
   --  We define some Unchecked_Conversions between them:

   pragma Warnings (Off);

   --  To disable "possible aliasing problem" warnings which do not apply in
   --  this case.

   function To_RACW_Stub_Access is
     new Ada.Unchecked_Conversion
       (Source => Term_Manager_Stub_Access,
        Target => RACW_Stub_Type_Access);

   function To_TM_Access is
     new Ada.Unchecked_Conversion
       (Source => System.Address,
        Target => Term_Manager_Access);

   pragma Warnings (On);

   function Term_Manager_To_Address is
     new Ada.Unchecked_Conversion (Term_Manager_Access, System.Address);

   ----------------------------------
   -- Extract_TM_Reference_From_BO --
   ----------------------------------

   procedure Extract_TM_Reference_From_BO
      (BO  : Binding_Object_Access;
       Ref : out References.Ref;
       NK  : out Node_Kind)
   is
      use Annotations;
      use Binding_Data.Neighbour;
      use QoS.Term_Manager_Info;
      use References;

      BO_Prof : constant Binding_Data.Profile_Access := Get_Profile (BO);
      BO_Ref  : Smart_Pointers.Ref;
      P       : Neighbour_Profile_Type;
      Note    : BO_Note;

   begin
      if BO_Prof = null then
         pragma Debug (C, O ("Extracting TM ref from Server BO"));

         --  BO is a server side BO

         Enter_BO_Note_Lock;
         begin
            Get_Note (Notepad_Of (BO).all, Note);
         exception
            when Constraint_Error =>
               Leave_BO_Note_Lock;
               NK := Non_DSA_Node;
               pragma Debug (C, O ("-> " & NK'Img));
               return;
         end;
         Leave_BO_Note_Lock;

         if References.Is_Nil (Note.TM_Ref) then
            NK := DSA_Node_Without_TM;
            pragma Debug (C, O ("-> " & NK'Img));
            return;
         end if;

         NK := DSA_Node;
         Ref := Note.TM_Ref;

      elsif not Is_Multicast_Profile (BO_Prof.all) then
         pragma Debug (C, O ("Extracting TM ref from Client BO"));

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

      else
         --  Client multicast binding object: do not attempt to contact a
         --  termination manager.

         NK := Non_DSA_Node;
      end if;

      pragma Debug (C, O ("-> " & NK'Img & " TM: " & Image (Ref)));
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

   procedure Initialize_Termination_Manager is
      use PolyORB.Objects;
      use PolyORB.Parameters;

      TM           : constant Term_Manager_Ptr := new Term_Manager;
      S            : System.Partition_Interface.Servant_Access;

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
      pragma Debug (C, O ("Initialize_Termination_Manager: enter"));

      if not Get_Conf ("dsa", "tasking_available") then
         if Term_Policy_Value (Term_Policy) = Local_Termination then

            --  If our profile is a no_tasking node with local_termination
            --  then there is nothing more to do!

            pragma Debug (C, O ("No-tasking, Local_Termination node"));
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
      --  Note, we can't bind The_TM_Ref to obtain the servant because the
      --  corresponding POA has not been activated yet, and so we would get
      --  the Hold_Servant instead.

      S := Find_Receiving_Stub (RACW_Type_Name, Obj_Stub);
      pragma Assert (S /= null);

      --  Start the Well Known Service

      pragma Debug (C, O ("Initiating Well Known Service"));
      DSA_P.Initialization.Initiate_Well_Known_Service
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

      pragma Debug (C, O ("Initialize_Termination_Manager: leave"));
   end Initialize_Termination_Manager;

   --------------------------------
   -- Ref_To_Term_Manager_Access --
   --------------------------------

   function Ref_To_Term_Manager_Access
     (R : References.Ref) return Term_Manager_Access
   is
   begin
      return To_TM_Access
               (System.Partition_Interface.Get_RACW
                  (Ref          => R,
                   Stub_Tag     => Term_Manager_Access'Stub_Type'Tag,
                   Is_RAS       => False,
                   Asynchronous => False));
   end Ref_To_Term_Manager_Access;

   --------------------------------
   -- Term_Manager_Access_To_Ref --
   --------------------------------

   function Term_Manager_Access_To_Ref
     (TM : Term_Manager_Access) return References.Ref
   is
      Receiver : System.Partition_Interface.Servant_Access;
      Result   : References.Ref;
   begin

      --  We retrieve the receiver stub of Term_Manager RACW for this partition

      Receiver := Find_Receiving_Stub (RACW_Type_Name, Obj_Stub);
      pragma Assert (Receiver /= null);

      --  Then use it to get a reference to TM

      Build_Local_Reference
        (Addr     => Term_Manager_To_Address (TM),
         Typ      => RACW_Type_Name,
         Receiver => Receiver,
         Ref      => Result);

      return Result;
   end Term_Manager_Access_To_Ref;

end PolyORB.Termination_Manager.Bootstrap;
