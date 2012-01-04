------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFICATION.ADMINPROPERTIESADMIN.IMPL                 --
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

with CosNotification.AdminPropertiesAdmin.Skel;
pragma Warnings (Off, CosNotification.AdminPropertiesAdmin.Skel);

package body CosNotification.AdminPropertiesAdmin.Impl is

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("adminpropertiesadmin");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type AdminProperties_Admin_Record is record
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

   ---------------
   -- Get_Admin --
   ---------------

   function Get_Admin
     (Self : access Object)
     return CosNotification.AdminProperties
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.14
      MyProp : CosNotification.AdminProperties;
   begin
      pragma Debug (O ("get_admin in adminpropertiesadmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

      return MyProp;
   end Get_Admin;

   ---------------
   -- Set_Admin --
   ---------------

   procedure Set_Admin
     (Self   : access Object;
      Admin  : CosNotification.AdminProperties)
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Self, Admin);
      pragma Warnings (On);  --  WAG:3.14
   begin
      pragma Debug (O ("set_admin in adminpropertiesadmin"));

      Ensure_Initialization;

      Enter (Self_Mutex);
      Leave (Self_Mutex);

   end Set_Admin;

   ------------
   -- Create --
   ------------

   function Create
      return Object_Ptr
   is
      AdminPropertiesAdmin : Object_Ptr;
      My_Ref               : CosNotification.AdminPropertiesAdmin.Ref;
   begin
      pragma Debug (O ("create adminpropertiesadmin"));

      AdminPropertiesAdmin         := new Object;
      AdminPropertiesAdmin.X       := new AdminProperties_Admin_Record;
      AdminPropertiesAdmin.X.This  := AdminPropertiesAdmin;
      Initiate_Servant (PortableServer.Servant (AdminPropertiesAdmin), My_Ref);

      return AdminPropertiesAdmin;
   end Create;

end CosNotification.AdminPropertiesAdmin.Impl;
