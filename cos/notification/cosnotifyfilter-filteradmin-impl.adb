------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     C O S N O T I F Y F I L T E R . F I L T E R A D M I N . I M P L      --
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

with CosNotifyFilter.FilterAdmin.Skel;
pragma Warnings (Off, CosNotifyFilter.FilterAdmin.Skel);

package body CosNotifyFilter.FilterAdmin.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("filteradmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Filter_Admin_Record is record
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

   ----------------
   -- Add_Filter --
   ----------------

   function Add_Filter
     (Self       : access Object;
      New_Filter : CosNotifyFilter.Filter.Ref)
     return CosNotifyFilter.FilterID
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, New_Filter);
      pragma Warnings (On);  --  WAG:3.14
      MyFilterID : CosNotifyFilter.FilterID;
      MyID       : CORBA.Long;
   begin
      pragma Debug (O ("add_filter in filteradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
      MyID := 0;
      MyFilterID := CosNotifyFilter.FilterID (MyID);
      return MyFilterID;
   end Add_Filter;

   -------------------
   -- Remove_Filter --
   -------------------

   procedure Remove_Filter
     (Self   : access Object;
      Filter : CosNotifyFilter.FilterID)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filter);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("remove_filter in filteradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_Filter;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (Self   : access Object;
      Filter : CosNotifyFilter.FilterID)
     return CosNotifyFilter.Filter.Ref
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Filter);
      pragma Warnings (On);  --  WAG:3.14
      MyFilter : CosNotifyFilter.Filter.Ref;
   begin
      pragma Debug (O ("get_filter in filteradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilter;
   end Get_Filter;

   ---------------------
   -- Get_All_Filters --
   ---------------------

   function Get_All_Filters
     (Self : access Object)
     return CosNotifyFilter.FilterIDSeq
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyFilterSeq : CosNotifyFilter.FilterIDSeq;
   begin
      pragma Debug (O ("get_all_filters in filteradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyFilterSeq;
   end Get_All_Filters;

   ------------------------
   -- Remove_All_Filters --
   ------------------------

   procedure Remove_All_Filters
     (Self : access Object)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("remove_all_filters in filteradmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);
   end Remove_All_Filters;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      FilterAdmin : Object_Ptr;
      My_Ref      : CosNotifyFilter.FilterAdmin.Ref;
   begin
      pragma Debug (O ("create filteradmin"));

      FilterAdmin         := new Object;
      FilterAdmin.X       := new Filter_Admin_Record;
      FilterAdmin.X.This  := FilterAdmin;
      Initiate_Servant (PortableServer.Servant (FilterAdmin), My_Ref);

      return FilterAdmin;
   end Create;

end CosNotifyFilter.FilterAdmin.Impl;
