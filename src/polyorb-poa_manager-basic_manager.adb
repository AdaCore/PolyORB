------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . P O A _ M A N A G E R . B A S I C _ M A N A G E R     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Components;
with PolyORB.Log;
with PolyORB.ORB.Iface;
with PolyORB.Setup;

package body PolyORB.POA_Manager.Basic_Manager is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.poa_manager.basic_manager");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Do_Wait_For_Completion  (Self : access Basic_POA_Manager);
   --  Wait for completion

   procedure Do_Etherealize_Objects (Self : access Basic_POA_Manager);
   --  Etherealize the objects of the associated POAs
   --  (in case a Servant Manager is used with a RETAIN policy)

   procedure Reemit_Requests (Self : access Basic_POA_Manager);
   --  Reemit requests stored by the Hold Servant attached to
   --  Self. Note: this function assumes Self.Lock is held.

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Self  : access Basic_POA_Manager;
      Error : in out PolyORB.Errors.Error_Container)
   is
      use Requests_Queues;

   begin
      pragma Debug (C, O ("Activate POAManager: enter"));
      Enter (Self.Lock);
      pragma Debug (C, O ("Activate POAManager: locked, state is "
        & Self.Current_State'Img));

      --  Test invocation validity

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is INACTIVE, raise an exception

         Throw (Error,
                AdapterInactive_E,
                Null_Members'(Null_Member));
         Leave (Self.Lock);
         return;
      end if;

      --  else set the POAManager state to ACTIVE

      Self.Current_State := ACTIVE;

      --  If we were holding requests, reemit them

      pragma Debug (C, O ("Activate POAManager: checking for held requests"));
      if Self.PM_Hold_Servant /= null
        and then not Is_Empty (Self.Held_Requests)
      then
         Reemit_Requests (Self);
      end if;
      Leave (Self.Lock);
      pragma Debug (C, O ("Activate POAManager: leave"));
   end Activate;

   -------------------
   -- Hold_Requests --
   -------------------

   overriding procedure Hold_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion : Boolean;
      Error               : in out PolyORB.Errors.Error_Container)
   is
   begin
      pragma Debug (C, O ("Hold requests, Wait_For_Completion is "
                       & Boolean'Image (Wait_For_Completion)));

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;

      Enter (Self.Lock);

      --  Test invocation validity

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is INACTIVE, raise an exception

         Throw (Error,
                AdapterInactive_E,
                Null_Members'(Null_Member));
      else
         --  else set the POAManager state to HOLDING

         Self.Current_State := HOLDING;
      end if;

      Leave (Self.Lock);
   end Hold_Requests;

   ----------------------
   -- Discard_Requests --
   ----------------------

   overriding procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Errors.Error_Container)
   is
   begin
      pragma Debug (C, O ("Discard requests, Wait_For_Completion is "
                       & Boolean'Image (Wait_For_Completion)));

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;

      Enter (Self.Lock);

      --  Test invocation validity

      if Self.Current_State = INACTIVE then

         --  If the POAManager state is INACTIVE, raise an exception

         Throw (Error,
                AdapterInactive_E,
                Null_Members'(Null_Member));
      else
         --  else set the POAManager state to DISCARDING

         Self.Current_State := DISCARDING;
      end if;

      Leave (Self.Lock);
   end Discard_Requests;

   ----------------
   -- Deactivate --
   ----------------

   overriding procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean)
   is
   begin
      pragma Debug (C, O ("Deactivate: Wait_For_Completion is "
                       & Boolean'Image (Wait_For_Completion)
                       & ", Etherealize_Objects is "
                       & Boolean'Image (Etherealize_Objects)));

      Enter (Self.Lock);

      if Self.Current_State /= INACTIVE then
         Self.Current_State := INACTIVE;
      end if;

      Leave (Self.Lock);

      if Etherealize_Objects then
         Do_Etherealize_Objects (Self);
      end if;

      if Wait_For_Completion then
         Do_Wait_For_Completion (Self);
      end if;
   end Deactivate;

   ---------------
   -- Get_State --
   ---------------

   overriding function Get_State (Self : Basic_POA_Manager) return State is
      Result : State;

   begin
      Enter (Self.Lock);
      Result := Self.Current_State;
      Leave (Self.Lock);

      return Result;
   end Get_State;

   ------------
   -- Create --
   ------------

   overriding procedure Create (M : access Basic_POA_Manager) is
      use Requests_Queues;
   begin
      pragma Debug (C, O ("Create a new Basic_POA_Manager"));

      Create (M.Lock);
      pragma Assert (Is_Empty (M.Held_Requests));
      M.Current_State := HOLDING;
   end Create;

   ------------------
   -- Register_POA --
   ------------------

   overriding procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access)
   is
      use POA_Lists;

   begin
      pragma Debug (C, O ("Register a new POA"));

      Enter (Self.Lock);
      Append (Self.Managed_POAs, OA);
      Leave (Self.Lock);
   end Register_POA;

   ----------------
   -- Remove_POA --
   ----------------

   overriding procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
   is
      use POA_Lists;

      A_Child : Obj_Adapter_Access;

      It : Iterator := First (Self.Managed_POAs);

   begin
      pragma Debug (C, O ("Remove a POA: enter"));

      Enter (Self.Lock);

      while not Last (It) loop
         A_Child := Value (It).all;

         if A_Child = OA then
            Remove (Self.Managed_POAs, It);
            Leave (Self.Lock);
            pragma Debug (C, O ("Remove a POA: end"));
            return;
         end if;

         Next (It);
      end loop;

      Leave (Self.Lock);

      raise Program_Error;
   end Remove_POA;

   ----------------------
   -- Get_Hold_Servant --
   ----------------------

   overriding function Get_Hold_Servant
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
     return Servants.Servant_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

   begin
      pragma Debug (C, O ("Get a Hold_Servant"));

      Enter (Self.Lock);

      if Self.PM_Hold_Servant = null then
         Self.PM_Hold_Servant := new Hold_Servant;
         Self.PM_Hold_Servant.PM := Basic_POA_Manager_Access (Self);
      end if;

      Leave (Self.Lock);

      return Servants.Servant_Access (Self.PM_Hold_Servant);
   end Get_Hold_Servant;

   ----------------------------
   -- Do_Wait_For_Completion --
   ----------------------------

   procedure Do_Wait_For_Completion (Self : access Basic_POA_Manager) is
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

   procedure Do_Etherealize_Objects (Self : access Basic_POA_Manager) is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      null;
      --  XXX To be implemented
   end Do_Etherealize_Objects;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Basic_POA_Manager) is
      use Requests_Queues;
      use POA_Lists;

      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Hold_Servant,
         Name => Hold_Servant_Access);

      R : Request_Access;

   begin
      pragma Debug (C, O ("POAManager is no longer used, destroying it"));

      Destroy (Self.Lock);

      if Self.PM_Hold_Servant /= null then
         Free (Self.PM_Hold_Servant);
      end if;

      Deallocate (Self.Managed_POAs);

      while not Is_Empty (Self.Held_Requests) loop
         Extract_First (Self.Held_Requests, R);
         Destroy_Request (R);
      end loop;

      Deallocate (Self.Held_Requests);

      pragma Debug (C, O ("POAManager destroyed."));
   end Finalize;

   ----------------------------------
   -- Holding state implementation --
   ----------------------------------

   ---------------------
   -- Reemit_Requests --
   ---------------------

   procedure Reemit_Requests (Self : access Basic_POA_Manager) is
      use PolyORB.Components;
      use PolyORB.ORB.Iface;
      use Requests_Queues;

      R : Request_Access;

   begin
      pragma Debug (C, O ("Number of requests to reemit:"
                       & Integer'Image (Length (Self.Held_Requests))));

      while not Is_Empty (Self.Held_Requests) loop
         Extract_First (Self.Held_Requests, R);
         Emit_No_Reply (Component_Access (PolyORB.Setup.The_ORB),
                        Queue_Request'
                        (Request   => R,
                         Requestor => R.Requesting_Component));
      end loop;
   end Reemit_Requests;

   ---------------------
   -- Execute_Servant --
   ---------------------

   overriding function Execute_Servant
     (Obj : not null access Hold_Servant;
      Req : Requests.Request_Access) return Boolean
   is
      use Requests_Queues;
      S : constant Hold_Servant_Access := Hold_Servant_Access (Obj);
   begin
      pragma Debug (C, O ("Hold_Servant: Queuing request"));
      Enter (S.PM.Lock);
      Append (S.PM.Held_Requests, Req);
      Leave (S.PM.Lock);
      return False;
   end Execute_Servant;

end PolyORB.POA_Manager.Basic_Manager;
