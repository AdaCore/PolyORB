------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . P O A _ M A N A G E R . B A S I C _ M A N A G E R     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
with PolyORB.Requests;
with PolyORB.Setup;

package body PolyORB.POA_Manager.Basic_Manager is

   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.poa_manager.basic_manager");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

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

   procedure Activate
     (Self  : access Basic_POA_Manager;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      use Requests_Queue_P;

   begin
      pragma Debug (O ("Activate POAManager: enter"));

      Enter (Self.Lock);

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

      if Self.PM_Hold_Servant /= null
        and then Length (Self.Held_Requests) > 0
      then
         Reemit_Requests (Self);
      end if;

      Leave (Self.Lock);
      pragma Debug (O ("Activate POAManager: leave"));
   end Activate;

   -------------------
   -- Hold_Requests --
   -------------------

   procedure Hold_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      pragma Debug (O ("Hold requests, Wait_For_Completion is "
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

   procedure Discard_Requests
     (Self                : access Basic_POA_Manager;
      Wait_For_Completion :        Boolean;
      Error               : in out PolyORB.Exceptions.Error_Container)
   is
   begin
      pragma Debug (O ("Discard requests, Wait_For_Completion is "
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

   procedure Deactivate
     (Self                : access Basic_POA_Manager;
      Etherealize_Objects :        Boolean;
      Wait_For_Completion :        Boolean)
   is
   begin
      pragma Debug (O ("Deactivate: Wait_For_Completion is "
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

   function Get_State (Self : Basic_POA_Manager) return State is
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

   procedure Create (M : access Basic_POA_Manager) is
   begin
      pragma Debug (O ("Create a new Basic_POA_Manager"));

      Create (M.Lock);

      M.Current_State := HOLDING;
   end Create;

   ------------------
   -- Register_POA --
   ------------------

   procedure Register_POA
     (Self : access Basic_POA_Manager;
      OA   : Obj_Adapter_Access)
   is
      use POA_Lists;

   begin
      pragma Debug (O ("Register a new POA"));

      Enter (Self.Lock);
      Append (Self.Managed_POAs, OA);
      Leave (Self.Lock);
   end Register_POA;

   ----------------
   -- Remove_POA --
   ----------------

   procedure Remove_POA
     (Self : access Basic_POA_Manager;
      OA   :        Obj_Adapter_Access)
   is
      use POA_Lists;

      A_Child : Obj_Adapter_Access;

      It : Iterator := First (Self.Managed_POAs);

   begin
      pragma Debug (O ("Remove a POA: enter"));

      Enter (Self.Lock);

      while not Last (It) loop
         A_Child := Value (It).all;

         if A_Child = OA then
            Remove (Self.Managed_POAs, It);
            Leave (Self.Lock);
            pragma Debug (O ("Remove a POA: end"));
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

   procedure Finalize (Self : in out Basic_POA_Manager) is
      use PolyORB.Requests;
      use Requests_Queue_P;
      use POA_Lists;

      procedure Free is new Ada.Unchecked_Deallocation
        (Hold_Servant, Hold_Servant_Access);

      R : Execute_Request;

   begin
      pragma Debug (O ("POAManager is no longer used, destroying it"));

      Destroy (Self.Lock);

      if Self.PM_Hold_Servant /= null then
         Free (Self.PM_Hold_Servant);
      end if;

      Deallocate (Self.Managed_POAs);

      while Self.Held_Requests /= Requests_Queue_P.Empty loop
         Extract_First (Self.Held_Requests, R);
         Destroy_Request (R.Req);
      end loop;

      Deallocate (Self.Held_Requests);

      pragma Debug (O ("POAManager destroyed."));
   end Finalize;

   ----------------------------------
   -- Holding state implementation --
   ----------------------------------

   ---------------------
   -- Reemit_Requests --
   ---------------------

   procedure Reemit_Requests (Self : access Basic_POA_Manager) is
      use PolyORB.Components;
      use PolyORB.ORB.Interface;
      use Requests_Queue_P;

      R : Execute_Request;

   begin
      pragma Debug (O ("Number of requests to reemit"
                       & Integer'Image (Length (Self.Held_Requests))));

      while Self.Held_Requests /= Empty loop
         Extract_First (Self.Held_Requests, R);
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

      S            : constant Hold_Servant_Access := Hold_Servant_Access (Obj);
      Null_Message : PolyORB.Components.Null_Message;

   begin
      if Msg in Execute_Request then
         Enter (S.PM.Lock);

         pragma Debug (O ("Hold Servant queues message"));
         Append (S.PM.Held_Requests, Execute_Request (Msg));

         Leave (S.PM.Lock);
      else
         pragma Debug (O ("Message not in Execute_Request"));
         raise PolyORB.Components.Unhandled_Message;
      end if;

      return Null_Message;
   end Execute_Servant;

end PolyORB.POA_Manager.Basic_Manager;
