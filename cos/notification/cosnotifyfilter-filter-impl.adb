------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          C O S N O T I F Y F I L T E R . F I L T E R . I M P L           --
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

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Log;
with PolyORB.Tasking.Mutexes;

with CosNotifyFilter.Filter.Skel;
pragma Warnings (Off, CosNotifyFilter.Filter.Skel);

package body CosNotifyFilter.Filter.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("filter");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
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
      Constraint_List : CosNotifyFilter.ConstraintExpSeq)
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
      Del_List    : CosNotifyFilter.ConstraintIDSeq;
      Modify_List : CosNotifyFilter.ConstraintInfoSeq)
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
      Id_List : CosNotifyFilter.ConstraintIDSeq)
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
      Filterable_Data : CORBA.Any)
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
      Filterable_Data : CosNotification.StructuredEvent)
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
      Filterable_Data : CosNotification.PropertySeq)
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
      Callback : CosNotifyComm.NotifySubscribe.Ref)
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
      Callback : CosNotifyFilter.CallbackID)
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
      Initiate_Servant (PortableServer.Servant (Filter), My_Ref);

      return Filter;
   end Create;

end CosNotifyFilter.Filter.Impl;
