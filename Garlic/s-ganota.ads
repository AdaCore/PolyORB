------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . N O _ T A S K I N G            --
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

with System.Garlic.Soft_Links;
with System.Garlic.Types;

package System.Garlic.No_Tasking is

   procedure Initialize;

   ----------------------------------------------
   -- Critical Section for PCS without Tasking --
   ----------------------------------------------

   procedure Enter_Critical_Section;

   procedure Leave_Critical_Section;

   --------------------------------
   -- Mutex for PCS with Tasking --
   --------------------------------

   type Unprotected_Mutex_Type is
     new Soft_Links.Mutex_Type with private;

   function Create return Soft_Links.Mutex_Access;

   procedure Enter (M : in Unprotected_Mutex_Type);

   procedure Destroy (M : in out Unprotected_Mutex_Type);

   procedure Leave (M : in Unprotected_Mutex_Type);

   -------------------------------------
   -- Watcher for PCS without Tasking --
   -------------------------------------

   type Unprotected_Watcher_Type is
     new Soft_Links.Watcher_Type with private;

   function Create (V : in Types.Version_Id) return Soft_Links.Watcher_Access;

   procedure Destroy (W : in out Unprotected_Watcher_Type);

   procedure Differ
     (W : in out Unprotected_Watcher_Type;
      V : in Types.Version_Id);

   procedure Lookup
     (W : in Unprotected_Watcher_Type;
      V : out Types.Version_Id);

   procedure Update (W : in out Unprotected_Watcher_Type);

   --------------------------------------------
   -- Advanced Mutex for PCS without Tasking --
   --------------------------------------------

   type Unprotected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with private;

   function Create return Soft_Links.Adv_Mutex_Access;

   procedure Enter (M : in out Unprotected_Adv_Mutex_Type);

   procedure Destroy (M : in out Unprotected_Adv_Mutex_Type);

   procedure Leave (M : in out Unprotected_Adv_Mutex_Type);

   function Is_Environment_Task return Boolean;

   function Env_Task_Awake_Count return Natural;

   function Independent_Task_Count return Natural;

   function Get_Priority return Natural;

   procedure Set_Priority (P : in Natural);

private

   type Unprotected_Mutex_Type is
     new Soft_Links.Mutex_Type with null record;

   type Unprotected_Watcher_Type is
     new Soft_Links.Watcher_Type with
      record
         Version : Types.Version_Id;
      end record;

   type Unprotected_Adv_Mutex_Type is
     new Soft_Links.Adv_Mutex_Type with null record;

end System.Garlic.No_Tasking;
