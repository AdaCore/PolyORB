------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        R T C O R B A . R T O R B                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with CORBA.ORB;

with PolyORB.RTCORBA_P.PriorityModelPolicy;
with PolyORB.CORBA_P.Initial_References;

with PolyORB.Exceptions;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings.Lists;

package body RTCORBA.RTORB is

   type RTORB_Object is new PolyORB.Smart_Pointers.Entity with null record;

   function Create return CORBA.Object.Ref;
   --  Create a RTCORBA.RTORB.Ref

   ------------
   -- Create --
   ------------

   function Create return CORBA.Object.Ref
   is
      Result : Ref;

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
     (Self : in Ref)
     return RTCORBA.Mutex.Ref is
   begin
      pragma Warnings (Off);
      return Create_Mutex (Self);
      pragma Warnings (On);
   end Create_Mutex;

   -------------------
   -- Destroy_Mutex --
   -------------------

   procedure Destroy_Mutex
     (Self      : in Ref;
      The_Mutex : in RTCORBA.Mutex.Ref)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (The_Mutex);
      pragma Warnings (On);
   begin
      null;
   end Destroy_Mutex;

   -----------------------
   -- Create_Threadpool --
   -----------------------

   function Create_Threadpool
     (Self                    : in Ref;
      Stacksize               : in CORBA.Unsigned_Long;
      Static_Threads          : in CORBA.Unsigned_Long;
      Dynamic_Threads         : in CORBA.Unsigned_Long;
      Default_Priority        : in RTCORBA.Priority;
      Allow_Request_Buffering : in CORBA.Boolean;
      Max_Buffered_Requests   : in CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : in CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId is
   begin
      pragma Warnings (Off);
      return Create_Threadpool
        (Self,
         Stacksize,
         Static_Threads,
         Dynamic_Threads,
         Default_Priority,
         Allow_Request_Buffering,
         Max_Buffered_Requests,
         Max_Request_Buffer_Size);
      pragma Warnings (On);
   end Create_Threadpool;

   ----------------------------------
   -- Create_Threadpool_With_Lanes --
   ----------------------------------

   function Create_Threadpool_With_Lanes
     (Self                    : in Ref;
      Stacksize               : in CORBA.Unsigned_Long;
      Lanes                   : in RTCORBA.ThreadpoolLanes;
      Allow_Borrowing         : in CORBA.Boolean;
      Allow_Request_Buffering : in CORBA.Boolean;
      Max_Buffered_Requests   : in CORBA.Unsigned_Long;
      Max_Request_Buffer_Size : in CORBA.Unsigned_Long)
     return RTCORBA.ThreadpoolId is
   begin
      pragma Warnings (Off);
      return Create_Threadpool_With_Lanes
        (Self,
         Stacksize,
         Lanes,
         Allow_Borrowing,
         Allow_Request_Buffering,
         Max_Buffered_Requests,
         Max_Request_Buffer_Size);
      pragma Warnings (On);
   end Create_Threadpool_With_Lanes;

   ------------------------
   -- Destroy_Threadpool --
   ------------------------

   procedure Destroy_Threadpool
     (Self       : in Ref;
      Threadpool : in RTCORBA.ThreadpoolId)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Threadpool);
      pragma Warnings (On);

   begin
      null;
   end Destroy_Threadpool;

   ----------------------------------
   -- Create_Priority_Model_Policy --
   ----------------------------------

   function Create_Priority_Model_Policy
     (Self            : in Ref;
      Priority_Model  : in RTCORBA.PriorityModel;
      Server_Priority : in RTCORBA.Priority)
     return RTCORBA.PriorityModelPolicy.Ref
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); --  WAG:3.15

      use PolyORB.RTCORBA_P.PriorityModelPolicy;

      Result : RTCORBA.PriorityModelPolicy.Ref;

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
     (Self       : in Ref;
      Threadpool : in RTCORBA.ThreadpoolId)
     return RTCORBA.ThreadpoolPolicy.Ref
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On); --  WAG:3.15
   begin
      return RTCORBA.ThreadpoolPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (THREADPOOL_POLICY_TYPE, To_Any (Threadpool)));
   end Create_Threadpool_Policy;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
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
     (Excp_Memb : in InvalidThreadpool_Members) is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidThreadpool'Identity, Excp_Memb);
   end Raise_InvalidThreadpool;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
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
      (Name      => +"rtcorba.current",
       Conflicts => Empty,
       Depends   => +"corba.initial_references",
       Provides  => Empty,
       Init      => Initialize'Access));

end RTCORBA.RTORB;
