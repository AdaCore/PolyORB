------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C O S N O T I F Y F I L T E R . F I L T E R . I M P L           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosNotifyFilter.Filter.Helper;
pragma Elaborate (CosNotifyFilter.Filter.Helper);
pragma Warnings (Off, CosNotifyFilter.Filter.Helper);

--  with CosNotifyFilter.Filter.Skel;
--  pragma Elaborate (CosNotifyFilter.Filter.Skel);
--  pragma Warnings (Off, CosNotifyFilter.Filter.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
--  with PolyORB.Tasking.Semaphores;
with PolyORB.Log;

package body CosNotifyFilter.Filter.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   --  use PolyORB.Tasking.Semaphores;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("filter");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Filter_Record is record
      This    : Object_Ptr;
   end record;

   ---------------------------
   -- Ensure_Initialization --
   ---------------------------

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   T_Initialized : Boolean := False;
   Self_Mutex : Mutex_Access;

   procedure Ensure_Initialization is
   begin
      if not T_Initialized then
         Create (Self_Mutex);
         T_Initialized := True;
      end if;
   end Ensure_Initialization;

   ----------------------------
   -- Get_Constraint_Grammar --
   ----------------------------

   function Get_Constraint_Grammar
     (Self : access Object)
     return CORBA.String
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyGrammar : CORBA.String;
   begin
      pragma Debug (O ("get_constraint_grammar in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyGrammar;
   end Get_Constraint_Grammar;

   ---------------------
   -- Add_Constraints --
   ---------------------

   function Add_Constraints
     (Self            : access Object;
      Constraint_List : in CosNotifyFilter.ConstraintExpSeq)
     return CosNotifyFilter.ConstraintInfoSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Constraint_List);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotifyFilter.ConstraintInfoSeq;
   begin
      pragma Debug (O ("add_constraints in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Add_Constraints;

   ------------------------
   -- Modify_Constraints --
   ------------------------

   procedure Modify_Constraints
     (Self        : access Object;
      Del_List    : in CosNotifyFilter.ConstraintIDSeq;
      Modify_List : in CosNotifyFilter.ConstraintInfoSeq)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Del_List, Modify_List);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("modify_constraints in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Modify_Constraints;

   ---------------------
   -- Get_Constraints --
   ---------------------

   function Get_Constraints
     (Self    : access Object;
      Id_List : in CosNotifyFilter.ConstraintIDSeq)
     return CosNotifyFilter.ConstraintInfoSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Id_List);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotifyFilter.ConstraintInfoSeq;
   begin
      pragma Debug (O ("get_constraints in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Get_Constraints;

   -------------------------
   -- Get_All_Constraints --
   -------------------------

   function Get_All_Constraints
     (Self : access Object)
     return CosNotifyFilter.ConstraintInfoSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotifyFilter.ConstraintInfoSeq;
   begin
      pragma Debug (O ("get_all_constraints in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Get_All_Constraints;

   ----------------------------
   -- Remove_All_Constraints --
   ----------------------------

   procedure Remove_All_Constraints
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("remove_all_constraints in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Remove_All_Constraints;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("destroy in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Destroy;

   -----------
   -- Match --
   -----------

   function Match
     (Self            : access Object;
      Filterable_Data : in CORBA.Any)
     return CORBA.Boolean
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filterable_Data);
      pragma Warnings (On);  --  WAG:3.14
      Res : constant CORBA.Boolean := True;
   begin
      pragma Debug (O ("match in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      raise Program_Error;

      return Res;
   end Match;

   ----------------------
   -- Match_Structured --
   ----------------------

   function Match_Structured
     (Self            : access Object;
      Filterable_Data : in CosNotification.StructuredEvent)
     return CORBA.Boolean
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filterable_Data);
      pragma Warnings (On);  --  WAG:3.14
      Res : constant CORBA.Boolean := True;
   begin
      pragma Debug (O ("match_structured in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      raise Program_Error;

      return Res;
   end Match_Structured;

   -----------------
   -- Match_Typed --
   -----------------

   function Match_Typed
     (Self            : access Object;
      Filterable_Data : in CosNotification.PropertySeq)
     return CORBA.Boolean
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filterable_Data);
      pragma Warnings (On);  --  WAG:3.14
      Res : constant CORBA.Boolean := True;
   begin
      pragma Debug (O ("match_typed in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      raise Program_Error;

      return Res;
   end Match_Typed;

   ---------------------
   -- Attach_Callback --
   ---------------------

   function Attach_Callback
     (Self     : access Object;
      Callback : in CosNotifyComm.NotifySubscribe.Ref)
     return CosNotifyFilter.CallbackID
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Callback);
      pragma Warnings (On);  --  WAG:3.14
      MyID : constant CosNotifyFilter.CallbackID
        := CosNotifyFilter.CallbackID'First;
   begin
      pragma Debug (O ("attach_callback in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      raise Program_Error;

      return MyID;
   end Attach_Callback;

   ---------------------
   -- Detach_Callback --
   ---------------------

   procedure Detach_Callback
     (Self     : access Object;
      Callback : in CosNotifyFilter.CallbackID)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Callback);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("detach_callback in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Detach_Callback;

   -------------------
   -- Get_Callbacks --
   -------------------

   function Get_Callbacks
     (Self : access Object)
     return CosNotifyFilter.CallbackIDSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MySeq : CosNotifyFilter.CallbackIDSeq;
   begin
      pragma Debug (O ("get_callbacks in filter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Get_Callbacks;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Filter : Object_Ptr;
      My_Ref : CosNotifyFilter.Filter.Ref;
   begin
      pragma Debug (O ("create filter"));

      Filter         := new Object;
      Filter.X       := new Filter_Record;
      Filter.X.This  := Filter;
      Initiate_Servant (Servant (Filter), My_Ref);

      return Filter;
   end Create;

end CosNotifyFilter.Filter.Impl;
