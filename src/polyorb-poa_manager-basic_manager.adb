------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . P O A _ M A N A G E R . B A S I C _ M A N A G E R     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Components;
with PolyORB.Log;
with PolyORB.ORB.Interface;
with PolyORB.Setup;

package body PolyORB.POA_Manager.Basic_Manager is

   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Tasking.Rw_Locks;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.poa_manager.basic_manager");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Do_Wait_For_Completion
     (Self : access Basic_POA_Manager);
   --  Wait for completion

   procedure Do_Etherealize_Objects
     (Self : access Basic_POA_Manager);
   --  Etherealize the objects of the associated POAs
   --  (in case a Servant Manager is used with a RETAIN policy)

   procedure Inc_Usage_Counter
     (Self : access Basic_POA_Manager);
   --  Increment POA Manager Usage Counter.

   procedure Dec_Usage_Counter
     (Self : access Basic_POA_Manager);
   --  Decrement POA Manager Usage Counter.

   procedure Destroy_If_Unused
     (Self : in out Basic_POA_Manager);
   --  Destroy the POAManager if it is no longer used by any POA,
   --  and the POAManager has been created only for

   procedure Reemit_Requests
     (Self : access Basic_POA_Manager);
   --  Reemit requests stored by the Hold Servant attached to 'Self'.

   --------------
   -- Activate --
   --------------

   procedure Activate
     (Self  : access Basic_POA_Manager;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      use Requests_Queue_P;

   begin
      pragma Debug (O ("Activate POAManager: enter"));

      Lock_W (Self.State_Lock);

      --  Test invocation validity.

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is 'inactive', raise an exception.

         Throw (Error,
                AdapterInactive_E,
                Null_Members'(Null_Member));
         Unlock_W (Self.State_Lock);
      else

         --  else set the POAManager state to 'active'

         Self.Current_State := ACTIVE;
         Unlock_W (Self.State_Lock);
      end if;

      --  If we were holding requests, reemit them.

      if Self.PM_Hold_Servant /= null
        and then Length (Self.Held_Requests) > 0 then
         Reemit_Requests (Self);
      end if;

      pragma Debug (O ("Activate POAManager: leave"));
   end Activate;

   -------------------
   -- Hold_Requests --
   -------------------

   procedure Hold_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Debug (O ("Hold requests, Wait_For_Completion is "
                       & Boolean'Image (Wait_For_Completion)));

      Lock_W (Self.State_Lock);

      --  Test invocation validity.

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is 'inactive', raise an exception.

         Unlock_W (Self.State_Lock);
         Throw (Error,
                AdapterInactive_E,
                Null_Members'(Null_Member));

      else

         --  else set the POAManager state to 'holding'

         Self.Current_State := HOLDING;
         Unlock_W (Self.State_Lock);
      end if;

      --  Wait for the completion of the different request invocation.
      --  XXX To be implemented
      --  Illogical M. Kirk, this should be _at the beginning_ of the function.

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Hold_Requests;

   ----------------------
   -- Discard_Requests --
   ----------------------

   procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Debug (O ("Discard requests, Wait_For_Completion is "
                       & Boolean'Image (Wait_For_Completion)));

      Lock_W (Self.State_Lock);

      --  Test invocation validity.

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is 'inactive', raise an exception.

         Unlock_W (Self.State_Lock);
         Throw (Error,
                AdapterInactive_E,
                Null_Members'(Null_Member));
      else

         --  else set the POAManager state to 'discarding'

         Self.Current_State := DISCARDING;
         Unlock_W (Self.State_Lock);
      end if;

      --  Wait for the completion of the different request invocation.
      --  XXX To be implemented

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Discard_Requests;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean) is
   begin
      pragma Debug (O ("Deactivate: Wait_For_Completion is "
                       & Boolean'Image (Wait_For_Completion)
                       & ", Etherealize_Objects is "
                       & Boolean'Image (Etherealize_Objects)));

      Lock_W (Self.State_Lock);

      --  Test invocation validity.

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is 'inactive', nothing to do.

         Unlock_W (Self.State_Lock);
      else

         --  else set the POAManager state to 'inactive'

         Self.Current_State := INACTIVE;
         Unlock_W (Self.State_Lock);
      end if;

      --  XXX to be implemented

      if Etherealize_Objects then
         Do_Etherealize_Objects (Self);
      end if;

      --  XXX to be implemented

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Deactivate;

   ---------------
   -- Get_State --
   ---------------

   function Get_State
     (Self : Basic_POA_Manager)
     return State is
   begin
      return Self.Current_State;
   end Get_State;

   ------------
   -- Create --
   ------------

   procedure Create
     (M : access Basic_POA_Manager)
   is
      use PolyORB.POA_Types.POA_Sequences;

   begin
      pragma Debug (O ("Create a new Basic_POA_Manager"));

      Create (M.State_Lock);
      Create (M.Count_Lock);
      Create (M.POAs_Lock);
      Create (M.Queue_Lock);

      M.Managed_POAs := new POAList;

      M.Current_State := HOLDING;

   end Create;

   ------------------
   -- Register_POA --
   ------------------

   procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access)
   is
      use PolyORB.POA_Types.POA_Sequences;
   begin
      pragma Debug (O ("Register a new POA"));

      Lock_W (Self.POAs_Lock);

      for J in 1 .. Length (Sequence (Self.Managed_POAs.all)) loop
         if Element_Of (Sequence (Self.Managed_POAs.all), J) = null then
            Replace_Element (Sequence (Self.Managed_POAs.all), J, OA);
            Unlock_W (Self.POAs_Lock);
            Inc_Usage_Counter (Self);
            return;
         end if;
      end loop;

      Append (Sequence (Self.Managed_POAs.all), OA);
      Unlock_W (Self.POAs_Lock);

      Inc_Usage_Counter (Self);
   end Register_POA;

   ----------------
   -- Remove_POA --
   ----------------

   procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access)
   is
      use PolyORB.POA_Types.POA_Sequences;
      A_Child : Obj_Adapter_Access;
   begin
      pragma Debug (O ("Remove a POA: enter"));

      --  XXX to be reimplemented using a hash table.

      Lock_W (Self.POAs_Lock);

      for J in 1 .. Length (Sequence (Self.Managed_POAs.all)) loop
         A_Child := Element_Of (Sequence (Self.Managed_POAs.all), J);

         if A_Child = OA then
            Replace_Element (Sequence (Self.Managed_POAs.all), J, null);
            Unlock_W (Self.POAs_Lock);
            Dec_Usage_Counter (Self);
            Destroy_If_Unused (Self.all);
            pragma Debug (O ("Remove a POA: end"));
            return;
         end if;
      end loop;

      Unlock_W (Self.POAs_Lock);

      raise Program_Error;
   end Remove_POA;

   ----------------------
   -- Get_Hold_Servant --
   ----------------------

   function Get_Hold_Servant
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
     return Servants.Servant_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Get a Hold_Servant"));

      Lock_W (Self.State_Lock);

      if Self.PM_Hold_Servant = null then
         Self.PM_Hold_Servant := new Hold_Servant;
         Self.PM_Hold_Servant.PM := Basic_POA_Manager_Access (Self);
      end if;

      Unlock_W (Self.State_Lock);

      return Servants.Servant_Access (Self.PM_Hold_Servant);
   end Get_Hold_Servant;

   -----------------------
   -- Inc_Usage_Counter --
   -----------------------

   procedure Inc_Usage_Counter
     (Self : access Basic_POA_Manager) is
   begin
      Lock_W (Self.Count_Lock);
      Self.Usage_Count := Self.Usage_Count + 1;
      Unlock_W (Self.Count_Lock);

      pragma Debug (O ("Increase usage to "
                       & Integer'Image (Self.Usage_Count)));
   end Inc_Usage_Counter;

   -----------------------
   -- Dec_Usage_Counter --
   -----------------------

   procedure Dec_Usage_Counter
     (Self : access Basic_POA_Manager) is
   begin
      Lock_W (Self.Count_Lock);
      Self.Usage_Count := Self.Usage_Count - 1;
      Unlock_W (Self.Count_Lock);

      pragma Debug (O ("Decrease usage to "
                       & Integer'Image (Self.Usage_Count)));
   end Dec_Usage_Counter;

   ----------------------------
   -- Do_Wait_For_Completion --
   ----------------------------

   procedure Do_Wait_For_Completion
     (Self : access Basic_POA_Manager)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      --  XXX What's this thing about the threads ? Ignored for now.
      --  XXX Iterates through the POAs to wait for completion
      null;
   end Do_Wait_For_Completion;

   ----------------------------
   -- Do_Etherealize_Objects --
   ----------------------------

   procedure Do_Etherealize_Objects
     (Self : access Basic_POA_Manager)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      null;
      --  XXX To be implemented
   end Do_Etherealize_Objects;

   -----------------------
   -- Destroy_If_Unused --
   -----------------------

   procedure Destroy_If_Unused
     (Self : in out Basic_POA_Manager)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Hold_Servant, Hold_Servant_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (POAList, POAList_Access);

   begin
      Lock_R (Self.Count_Lock);

      if Self.Usage_Count = 0 then
         pragma Debug (O ("POAManager is no longer used, destroying it"));
         Unlock_R (Self.Count_Lock);

         Destroy (Self.State_Lock);
         Destroy (Self.Count_Lock);
         Destroy (Self.POAs_Lock);
         Destroy (Self.Queue_Lock);

         if Self.PM_Hold_Servant /= null then
            Free (Self.PM_Hold_Servant);
         end if;

         Free (Self.Managed_POAs);

         Finalize (Self);
         pragma Debug (O ("POAManager destroyed."));
      else
         Unlock_R (Self.Count_Lock);
      end if;

   end Destroy_If_Unused;

   ----------------------------------
   -- Holding state implementation --
   ----------------------------------

   ---------------------
   -- Reemit_Requests --
   ---------------------

   procedure Reemit_Requests
     (Self : access Basic_POA_Manager)
   is
      use PolyORB.Setup;
      use PolyORB.Components;
      use PolyORB.ORB.Interface;
      use Requests_Queue_P;

      R : Execute_Request;
      N : constant Natural := Length (Self.Held_Requests);

      All_Requests : Element_Array (1 .. N);
   begin
      pragma Debug (O ("Number of requests to reemit"
                       & Integer'Image (Length (Self.Held_Requests))));

      Lock_W (Self.Queue_Lock);
      All_Requests := To_Element_Array (Self.Held_Requests);
      Delete (Self.Held_Requests, 1, N);
      Unlock_W (Self.Queue_Lock);

      for J in 1 .. N loop
         R := All_Requests (J);
         Emit_No_Reply (Component_Access (PolyORB.Setup.The_ORB),
                        Queue_Request'
                        (Request   => R.Req,
                         Requestor => R.Req.Requesting_Component));
      end loop;
   end Reemit_Requests;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (Obj : access Hold_Servant;
      Msg :        PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use Requests_Queue_P;

      S            : Hold_Servant_Access := Hold_Servant_Access (Obj);
      Null_Message : PolyORB.Components.Null_Message;

   begin
      if Msg in Execute_Request then
         Lock_W (S.PM.Queue_Lock);

         pragma Debug (O ("Hold Servant queues message"));
         Append (S.PM.Held_Requests, Execute_Request (Msg));

         Unlock_W (S.PM.Queue_Lock);
      else
         pragma Debug (O ("Message not in Execute_Request"));
         raise PolyORB.Components.Unhandled_Message;
      end if;

      return Null_Message;
   end Execute_Servant;

end PolyORB.POA_Manager.Basic_Manager;
