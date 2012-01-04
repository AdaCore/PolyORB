------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   C O S N O T I F Y F I L T E R . M A P P I N G F I L T E R . I M P L    --
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

with CosNotifyFilter.MappingFilter.Skel;
pragma Warnings (Off, CosNotifyFilter.MappingFilter.Skel);

package body CosNotifyFilter.MappingFilter.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("mappingfilter");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Mapping_Filter_Record is record
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
      pragma Unreferenced (Self);
      MyGrammar : CORBA.String;
   begin
      pragma Debug (O ("get_constraint_grammar in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyGrammar;
   end Get_Constraint_Grammar;

   --------------------
   -- Get_Value_Type --
   --------------------
   --  NK Should get it checked because of TypeCode Problems

   function Get_Value_Type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      pragma Unreferenced (Self);
      MyObj : CORBA.TypeCode.Object;
   begin
      pragma Debug (O ("get_value_type in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyObj;
   end Get_Value_Type;

   -----------------------
   -- Get_Default_Value --
   -----------------------

   function Get_Default_Value
     (Self : access Object)
     return CORBA.Any
   is
      pragma Unreferenced (Self);
      MyValue : CORBA.Any;
   begin
      pragma Debug (O ("get_default_value in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyValue;
   end Get_Default_Value;

   -----------------------------
   -- Add_Mapping_Constraints --
   -----------------------------

   function Add_Mapping_Constraints
     (Self      : access Object;
      Pair_List : CosNotifyFilter.MappingConstraintPairSeq)
     return CosNotifyFilter.MappingConstraintInfoSeq
   is
      pragma Unreferenced (Self, Pair_List);
      MySeq : CosNotifyFilter.MappingConstraintInfoSeq;
   begin
      pragma Debug (O ("add_mapping_constraints in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Add_Mapping_Constraints;

   --------------------------------
   -- Modify_Mapping_Constraints --
   --------------------------------

   procedure Modify_Mapping_Constraints
     (Self        : access Object;
      Del_List    : CosNotifyFilter.ConstraintIDSeq;
      Modify_List : CosNotifyFilter.MappingConstraintInfoSeq)
   is
      pragma Unreferenced (Self, Del_List, Modify_List);
   begin
      pragma Debug (O ("modify_mapping_constraints in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Modify_Mapping_Constraints;

   -----------------------------
   -- Get_Mapping_Constraints --
   -----------------------------

   function Get_Mapping_Constraints
     (Self    : access Object;
      Id_List : CosNotifyFilter.ConstraintIDSeq)
     return CosNotifyFilter.MappingConstraintInfoSeq
   is
      pragma Unreferenced (Self, Id_List);
      MySeq : CosNotifyFilter.MappingConstraintInfoSeq;
   begin
      pragma Debug (O ("get_mapping_constraints in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Get_Mapping_Constraints;

   ---------------------------------
   -- Get_All_Mapping_Constraints --
   ---------------------------------

   function Get_All_Mapping_Constraints
     (Self : access Object)
     return CosNotifyFilter.MappingConstraintInfoSeq
   is
      pragma Unreferenced (Self);
      MySeq : CosNotifyFilter.MappingConstraintInfoSeq;
   begin
      pragma Debug (O ("get_all_mapping_constraints in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MySeq;
   end Get_All_Mapping_Constraints;

   ------------------------------------
   -- Remove_All_Mapping_Constraints --
   ------------------------------------

   procedure Remove_All_Mapping_Constraints
     (Self : access Object)
   is
      pragma Unreferenced (Self);
   begin
      pragma Debug (O ("remove_all_mapping_constraints in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Remove_All_Mapping_Constraints;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : access Object)
   is
      pragma Unreferenced (Self);
   begin
      pragma Debug (O ("destroy in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Destroy;

   -----------
   -- Match --
   -----------

   procedure Match
     (Self            : access Object;
      Filterable_Data : CORBA.Any;
      Result_To_Set   : out CORBA.Any;
      Returns         : out CORBA.Boolean)
   is
      pragma Unreferenced (Self, Filterable_Data);
      ResSet : CORBA.Any;
   begin
      pragma Debug (O ("match in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      Result_To_Set := ResSet;
      Returns := True;
   end Match;

   ----------------------
   -- Match_Structured --
   ----------------------

   procedure Match_Structured
     (Self            : access Object;
      Filterable_Data : CosNotification.StructuredEvent;
      Result_To_Set   : out CORBA.Any;
      Returns         : out CORBA.Boolean)
   is
      pragma Unreferenced (Self, Filterable_Data);
      ResSet : CORBA.Any;
   begin
      pragma Debug (O ("match_structured in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      Result_To_Set := ResSet;
      Returns := True;
   end Match_Structured;

   -----------------
   -- Match_Typed --
   -----------------

   procedure Match_Typed
     (Self            : access Object;
      Filterable_Data : CosNotification.PropertySeq;
      Result_To_Set   : out CORBA.Any;
      Returns         : out CORBA.Boolean)
   is
      pragma Unreferenced (Self, Filterable_Data);
      ResSet : CORBA.Any;
   begin
      pragma Debug (O ("match_typed in mappingfilter"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      Result_To_Set := ResSet;
      Returns := True;
   end Match_Typed;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      Filter : Object_Ptr;
      My_Ref : CosNotifyFilter.MappingFilter.Ref;
   begin
      pragma Debug (O ("create mappingfilter"));

      Filter         := new Object;
      Filter.X       := new Mapping_Filter_Record;
      Filter.X.This  := Filter;
      Initiate_Servant (PortableServer.Servant (Filter), My_Ref);

      return Filter;
   end Create;

end CosNotifyFilter.MappingFilter.Impl;
