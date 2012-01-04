------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        R T C O R B A . R T O R B                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with CORBA.ORB;

with PolyORB.CORBA_P.Initial_References;

with PolyORB.RTCORBA_P.Mutex;
with PolyORB.RTCORBA_P.PriorityModelPolicy;
with PolyORB.RTCORBA_P.ThreadPoolManager;
with PolyORB.RTCORBA_P.To_ORB_Priority;

with PolyORB.Exceptions;
with PolyORB.Initialization;

with PolyORB.Lanes;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Priorities;
with PolyORB.Types;
with PolyORB.Utils.Strings.Lists;

with RTCORBA.ThreadpoolPolicy.Helper;

package body RTCORBA.RTORB is

   use CORBA;
   use PolyORB.Tasking.Priorities;

   function Create return CORBA.Object.Ref;
   --  Create a RTCORBA.RTORB.Ref

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref is
      Result : Local_Ref;

      RTORB_Obj : constant PolyORB.Smart_Pointers.Entity_Ptr
        := new RTORB_Object;

   begin
      Set (Result, RTORB_Obj);

      return CORBA.Object.Ref (Result);
   end Create;

   ------------------
   -- Create_Mutex --
   ------------------

   function Create_Mutex
     (Self : Local_Ref)
     return RTCORBA.Mutex.Local_Ref
   is
      pragma Unreferenced (Self);

      use PolyORB.Smart_Pointers;

      Result : RTCORBA.Mutex.Local_Ref;
      Mutex_E : constant Entity_Ptr
        := new PolyORB.RTCORBA_P.Mutex.Mutex_Entity;

   begin
      PolyORB.Tasking.Mutexes.Create
        (PolyORB.RTCORBA_P.Mutex.Mutex_Entity (Mutex_E.all).Mutex);
      RTCORBA.Mutex.Set (Result, Mutex_E);

      return Result;
   end Create_Mutex;

   -------------------
   -- Destroy_Mutex --
   -------------------

   procedure Destroy_Mutex
     (Self      : Local_Ref;
      The_Mutex : RTCORBA.Mutex.Local_Ref)
   is
      pragma Unreferenced (Self);

      Mutex : PolyORB.Tasking.Mutexes.Mutex_Access
        := PolyORB.RTCORBA_P.Mutex.Mutex_Entity
        (RTCORBA.Mutex.Entity_Of (The_Mutex).all).Mutex;

   begin
      PolyORB.Tasking.Mutexes.Destroy (Mutex);

      PolyORB.RTCORBA_P.Mutex.Mutex_Entity
        (RTCORBA.Mutex.Entity_Of (The_Mutex).all).Mutex := null;
   end Destroy_Mutex;

   -----------------------
   -- Create_Threadpool --
   -----------------------

   function Create_Threadpool
     (Self                    : Local_Ref;
      Stacksize               : CORBA.Unsigned_Long;
      Static_Threads          : CORBA.Unsigned_Long;
      Dynamic_Threads         : CORBA.Unsigned_Long;
      Default_Priority        : RTCORBA.Priority;
      Allow_Request_Buffering : CORBA.Boolean;
      Max_Buffered_Requests   : CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId
   is
      pragma Unreferenced (Self);

      use PolyORB.Lanes;

      New_Lane : Lane_Root_Access;
      Lane_Index : RTCORBA.ThreadpoolId;

   begin
      if Max_Request_Buffer_Size /= 0 then
         --  See note in package specification

         Raise_Bad_Param (Default_Sys_Member);
      end if;

      New_Lane := Lane_Root_Access (Create
        (PolyORB.RTCORBA_P.To_ORB_Priority (Default_Priority),
         External_Priority (Default_Priority),
         Natural (Static_Threads),
         Natural (Dynamic_Threads),
         Natural (Stacksize),
         Allow_Request_Buffering,
         PolyORB.Types.Unsigned_Long (Max_Buffered_Requests),
         PolyORB.Types.Unsigned_Long (Max_Request_Buffer_Size)));

      PolyORB.RTCORBA_P.ThreadPoolManager.Register_Lane
        (New_Lane,
         Lane_Index);

      return Lane_Index;
   end Create_Threadpool;

   ----------------------------------
   -- Create_Threadpool_With_Lanes --
   ----------------------------------

   function Create_Threadpool_With_Lanes
     (Self                    : Local_Ref;
      Stacksize               : CORBA.Unsigned_Long;
      Lanes                   : RTCORBA.ThreadpoolLanes;
      Allow_Borrowing         : CORBA.Boolean;
      Allow_Request_Buffering : CORBA.Boolean;
      Max_Buffered_Requests   : CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId
   is
      pragma Unreferenced (Self);

      use PolyORB.Lanes;

      New_Lane : Lane_Root_Access;
      Lane_Index : RTCORBA.ThreadpoolId;

   begin
      if Max_Request_Buffer_Size /= 0
        or else Allow_Borrowing
      then
         --  See note in package specification

         Raise_Bad_Param (Default_Sys_Member);
      end if;

      New_Lane := new Lanes_Set (Length (Lanes));

      for J in 1 .. Length (Lanes) loop
         Add_Lane
           (Lanes_Set (New_Lane.all),
            Create
            (PolyORB.RTCORBA_P.To_ORB_Priority
             (Get_Element (Lanes, J).Lane_Priority),
             External_Priority (Get_Element (Lanes, J).Lane_Priority),
             Positive (Get_Element (Lanes, J).Static_Threads),
             Natural (Get_Element (Lanes, J).Dynamic_Threads),
             Natural (Stacksize),
             Allow_Request_Buffering,
             PolyORB.Types.Unsigned_Long (Max_Buffered_Requests),
             PolyORB.Types.Unsigned_Long (Max_Request_Buffer_Size)),
            J);
      end loop;

      PolyORB.RTCORBA_P.ThreadPoolManager.Register_Lane
        (New_Lane,
         Lane_Index);

      return Lane_Index;
   end Create_Threadpool_With_Lanes;

   ------------------------
   -- Destroy_Threadpool --
   ------------------------

   procedure Destroy_Threadpool
     (Self       : Local_Ref;
      Threadpool : RTCORBA.ThreadpoolId)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.RTCORBA_P.ThreadPoolManager;

   begin
      if not Lane_Registered (Threadpool) then
         raise InvalidThreadpool;
      end if;

      PolyORB.RTCORBA_P.ThreadPoolManager.Unregister_Lane (Threadpool);
   end Destroy_Threadpool;

   ----------------------------------
   -- Create_Priority_Model_Policy --
   ----------------------------------

   function Create_Priority_Model_Policy
     (Self            : Local_Ref;
      Priority_Model  : RTCORBA.PriorityModel;
      Server_Priority : RTCORBA.Priority)
     return RTCORBA.PriorityModelPolicy.Local_Ref
   is
      pragma Unreferenced (Self);

      use PolyORB.RTCORBA_P.PriorityModelPolicy;

      Result : RTCORBA.PriorityModelPolicy.Local_Ref;

      Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
        := Create (Priority_Model, Server_Priority);

   begin
      RTCORBA.PriorityModelPolicy.Set (Result, Entity);

      return Result;
   end Create_Priority_Model_Policy;

   ------------------------------
   -- Create_Threadpool_Policy --
   ------------------------------

   function Create_Threadpool_Policy
     (Self       : Local_Ref;
      Threadpool : RTCORBA.ThreadpoolId)
     return RTCORBA.ThreadpoolPolicy.Local_Ref
   is
      pragma Unreferenced (Self);

      use PolyORB.RTCORBA_P.ThreadPoolManager;

   begin
      if not Lane_Registered (Threadpool) then
         raise InvalidThreadpool;
      end if;

      return RTCORBA.ThreadpoolPolicy.Helper.To_Local_Ref
        (CORBA.ORB.Create_Policy
         (THREADPOOL_POLICY_TYPE, To_Any (Threadpool)));
   end Create_Threadpool_Policy;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidThreadpool_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= InvalidThreadpool'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := InvalidThreadpool_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------------------
   -- Raise_InvalidThreadpool --
   -----------------------------

   procedure Raise_InvalidThreadpool
     (Excp_Memb : InvalidThreadpool_Members) is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidThreadpool'Identity, Excp_Memb);
   end Raise_InvalidThreadpool;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      use PolyORB.CORBA_P.Initial_References;

   begin
      Register_Initial_Reference ("RTORB", Create'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"rtcorba.rtorb",
       Conflicts => Empty,
       Depends   => +"corba.initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end RTCORBA.RTORB;
