------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--             S Y S T E M . G A R L I C . S O F T _ L I N K S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Types;

package System.Garlic.Soft_Links is

   pragma Elaborate_Body;

   --  This package allows soft links to be defined and later called if they
   --  have been installed. The purpose of this is to be able not to register
   --  certain services that require tasking or other high-level features
   --  in order to provide the user with a light run time system.

   -------------------
   -- General types --
   -------------------

   type Parameterless_Procedure is access procedure;

   --------------------------
   -- Termination services --
   --------------------------

   --  Comments relative to these subprograms are located in s-garter.ads

   procedure Register_Add_Non_Terminating_Task
     (P : in Parameterless_Procedure);
   procedure Add_Non_Terminating_Task;

   procedure Register_Sub_Non_Terminating_Task
     (P : in Parameterless_Procedure);
   procedure Sub_Non_Terminating_Task;

   procedure Register_Activity_Detected
     (P : in Parameterless_Procedure);
   procedure Activity_Detected;

   procedure Register_Local_Termination
     (P : in Parameterless_Procedure);
   procedure Local_Termination;

   procedure Register_Global_Termination
     (P : in Parameterless_Procedure);
   procedure Global_Termination;

   -------------------------------
   -- Critical section handling --
   -------------------------------

   procedure Register_Enter_Critical_Section
     (P : in Parameterless_Procedure);
   procedure Enter_Critical_Section;

   procedure Register_Leave_Critical_Section
     (P : in Parameterless_Procedure);
   procedure Leave_Critical_Section;

   -----------
   -- Mutex --
   -----------

   type Mutex_Type is abstract tagged null record;

   procedure Enter (M : in Mutex_Type) is abstract;

   procedure Destroy (M : in out Mutex_Type) is abstract;

   procedure Leave (M : in Mutex_Type) is abstract;

   type Mutex_Access is access all Mutex_Type'Class;

   type Mutex_Creation_Function is
     access function return Mutex_Access;

   procedure Register_Mutex_Creation_Function
     (F : in Mutex_Creation_Function);

   procedure Create (M : out Mutex_Access);
   pragma Inline (Create);

   procedure Enter (M : in Mutex_Access);
   pragma Inline (Enter);

   procedure Destroy (M : in out Mutex_Access);
   pragma Inline (Destroy);

   procedure Leave (M : in Mutex_Access);
   pragma Inline (Leave);

   -------------
   -- Watcher --
   -------------

   type Watcher_Type is abstract tagged limited null record;

   procedure Destroy (W : in out Watcher_Type) is abstract;

   procedure Differ
     (W : in out Watcher_Type;
      V : in Types.Version_Id) is abstract;
   --  Await until W version differs from V

   procedure Lookup
     (W : in Watcher_Type;
      V : out Types.Version_Id) is abstract;
   --  Fetch W version

   procedure Update (W : in out Watcher_Type) is abstract;
   --  Increment W version

   type Watcher_Access is access all Watcher_Type'Class;

   type Watcher_Creation_Function is
     access function (V : in Types.Version_Id) return Watcher_Access;

   procedure Register_Watcher_Creation_Function
     (F : in Watcher_Creation_Function);

   procedure Create
     (W : out Watcher_Access;
      V : in  Types.Version_Id := Types.No_Version);
   pragma Inline (Create);

   procedure Destroy (W : in out Watcher_Access);
   pragma Inline (Destroy);

   procedure Differ (W : in Watcher_Access; V : in Types.Version_Id);
   pragma Inline (Differ);
   --  Await until W version differs from V

   procedure Lookup (W : in Watcher_Access; V : out Types.Version_Id);
   pragma Inline (Lookup);
   --  Fetch W version

   procedure Update (W : in Watcher_Access);
   pragma Inline (Update);
   --  Increment W version

   --------------------
   -- Advanced Mutex --
   --------------------

   --  This is a classical mutual exclusion object except that when a
   --  task try to Enter a mutex several times without leaving it
   --  first it is not blocked and can continue. Leave keeps track of
   --  the number of times Enter has been successful.

   type Adv_Mutex_Type is abstract tagged limited null record;

   procedure Enter (M : in out Adv_Mutex_Type) is abstract;

   procedure Destroy (M : in out Adv_Mutex_Type) is abstract;

   procedure Leave (M : in out Adv_Mutex_Type) is abstract;

   type Adv_Mutex_Access is access all Adv_Mutex_Type'Class;

   type Adv_Mutex_Creation_Function is
     access function return Adv_Mutex_Access;

   procedure Register_Adv_Mutex_Creation_Function
     (F : in Adv_Mutex_Creation_Function);

   procedure Create (M : out Adv_Mutex_Access);
   pragma Inline (Create);

   procedure Enter (M : in Adv_Mutex_Access);
   pragma Inline (Enter);

   procedure Destroy (M : in out Adv_Mutex_Access);
   pragma Inline (Destroy);

   procedure Leave (M : in Adv_Mutex_Access);
   pragma Inline (Leave);

   -------------------------
   -- Shutdown mechanisms --
   -------------------------

   procedure Register_RPC_Shutdown
     (P : in Parameterless_Procedure);
   procedure RPC_Shutdown;

   -----------------------
   -- Tasking Utilities --
   -----------------------

   type Return_Boolean_Function is access function return Boolean;

   procedure Register_Is_Environment_Task
     (F : in Return_Boolean_Function);
   function Is_Environment_Task return Boolean;

   type Return_Natural_Function is access function return Natural;

   procedure Register_Env_Task_Awake_Count
     (F : in Return_Natural_Function);
   function Env_Task_Awake_Count return Natural;

   procedure Register_Independent_Task_Count
     (F : in Return_Natural_Function);
   function Independent_Task_Count return Natural;

   procedure Register_List_Tasks
     (P : in Parameterless_Procedure);
   procedure List_Tasks;

   procedure Register_Get_Priority
     (F : in Return_Natural_Function);
   function Get_Priority return Natural;

   type Natural_Parameter_Procedure is access procedure (N : in Natural);
   procedure Register_Set_Priority
     (P : in Natural_Parameter_Procedure);
   procedure Set_Priority (P : in Natural);

   type Abort_Handler_Type is tagged
      record
         PID  : Types.Partition_ID;
         Wait : Boolean;
         Key  : Natural;
      end record;

   procedure Adjust (Self : in out Abort_Handler_Type);

   type Abort_Handler_Access is access all Abort_Handler_Type'Class;

   procedure Register_Abort_Handler
     (Abort_Handler : in Abort_Handler_Access);

   function Abort_Handler return Abort_Handler_Type'Class;

end System.Garlic.Soft_Links;
