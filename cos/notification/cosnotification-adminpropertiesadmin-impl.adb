------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                COSNOTIFICATION.ADMINPROPERTIESADMIN.IMPL                 --
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

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);

with CosNotification.AdminPropertiesAdmin.Helper;
pragma Elaborate (CosNotification.AdminPropertiesAdmin.Helper);
pragma Warnings (Off, CosNotification.AdminPropertiesAdmin.Helper);

--  with CosNotification.AdminPropertiesAdmin.Skel;
--  pragma Elaborate (CosNotification.AdminPropertiesAdmin.Skel);
--  pragma Warnings (Off, CosNotification.AdminPropertiesAdmin.Skel);

with PortableServer;

with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Tasking.Mutexes;
--  with PolyORB.Tasking.Semaphores;
with PolyORB.Log;

package body CosNotification.AdminPropertiesAdmin.Impl is

   use PortableServer;

   use PolyORB.CORBA_P.Server_Tools;
   use PolyORB.Tasking.Mutexes;
   --  use PolyORB.Tasking.Semaphores;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("adminpropertiesadmin");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

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
      Admin  : in CosNotification.AdminProperties)
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
      Initiate_Servant (Servant (AdminPropertiesAdmin), My_Ref);

      return AdminPropertiesAdmin;
   end Create;

end CosNotification.AdminPropertiesAdmin.Impl;
