------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . T A B L E                   --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Deallocation;
with System.Garlic.Types;        use System.Garlic.Types;
with System.Garlic.Name_Table;   use System.Garlic.Name_Table;
with System.Garlic.Soft_Links;   use System.Garlic.Soft_Links;
with System.Garlic.Debug;        use System.Garlic.Debug;

package body System.Garlic.Table is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARTAB", "(s-gartab): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   -------------
   -- Complex --
   -------------

   package body Complex is

      type Component_Table_Type is
         array (Index_Type range <>) of Component_Type;

      type Component_Table_Access is access Component_Table_Type;

      Table : Component_Table_Access;

      Min_Pos  : constant Integer := Integer (First_Index);
      Max_Pos  :          Integer := Min_Pos + Initial_Size - 1;
      Last_Pos :          Integer := Min_Pos - 1;

      Min     : constant Index_Type := Index_Type (Min_Pos);
      Max     :          Index_Type := Index_Type (Max_Pos);

      type Usage_Type is record
         Name : Name_Id;
         Free : Boolean;
      end record;
      Null_Usage : constant Usage_Type := (Null_Name, True);

      type Usage_Table_Type   is array (Index_Type range <>) of Usage_Type;
      type Usage_Table_Access is access Usage_Table_Type;

      Usage   : Usage_Table_Access;

      function Allocate return Index_Type;
      --  Allocate a new component.

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Component_Table_Type, Component_Table_Access);

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Usage_Table_Type, Usage_Table_Access);

      procedure Validate (N : Index_Type);

      Mutex   : Mutex_Access;
      Watcher : Watcher_Access;

      --  This lock is used to block tasks until the table is
      --  modified. This uses special behaviour of Types.Mutex_Record.
      --  Basically, Local_Mutex.Leave (Postponed) lets the run-time know
      --  that the mutex has been postponed and that it should be resumed
      --  when a Local_Mutex.Leave (Modified) occurs.

      --  Most of these subprograms are abort deferred. At the beginning of
      --  them, the code enter a critical section. At the end, it leaves
      --  the critical section. To avoid a premature abortion in the middle
      --  of the critical section, the code is protected against abortion.

      --------------
      -- Allocate --
      --------------

      function Allocate return Index_Type
      is
         Old_Max   : Index_Type;
         Old_Table : Component_Table_Access;
         Old_Usage : Usage_Table_Access;
      begin
         --  Try to allocate a free slot

         for Index in Min .. Max loop
            if Usage (Index).Free then
               Usage (Index).Free := False;
               return Index;
            end if;
         end loop;

         --  Allocate new table

         Old_Max   := Max;
         Old_Table := Table;
         Old_Usage := Usage;

         Max_Pos   := Max_Pos + Increment_Size;
         Max       := Index_Type (Max_Pos);
         Table     := new Component_Table_Type (Min .. Max);
         Usage     := new Usage_Table_Type     (Min .. Max);

         --  Copy old table in new table

         Table (Min .. Old_Max) := Old_Table (Min .. Old_Max);
         Usage (Min .. Old_Max) := Old_Usage (Min .. Old_Max);

         --  Intialize incremented part of new table

         Table (Old_Max + 1 .. Max) := (others => Null_Component);
         Usage (Old_Max + 1 .. Max) := (others => Null_Usage);

         --  Release unused memory

         Free (Old_Table);
         Free (Old_Usage);

         Usage (Old_Max + 1).Free := False;
         return Old_Max + 1;
      end Allocate;

      ------------
      -- Differ --
      ------------

      procedure Differ (Version : in Types.Version_Id) is
      begin
         Differ (Watcher, Version);
      end Differ;

      -----------
      -- Enter --
      -----------

      procedure Enter is
      begin
         Enter (Mutex);
      end Enter;

      -------------------
      -- Get_Component --
      -------------------

      function Get_Component (N : Index_Type) return Component_Type
      is
         Component : Component_Type;
      begin
         Enter_Critical_Section;
         Validate (N);
         Component := Table (N);
         Leave_Critical_Section;

         return Component;
      end Get_Component;

      ---------------
      -- Get_Index --
      ---------------

      function Get_Index (S : String) return Index_Type
      is
         Index : Index_Type;
         Name  : Name_Id;
         Info  : Integer;
      begin
         Enter_Critical_Section;
         Name  := Get (S);
         Info  := Get_Info (Name);
         if Info = 0 then

            --  Info is a null index. Create new component and set its
            --  index as name info.

            Index := Allocate;
            Table (Index) := Null_Component;
            Usage (Index).Name := Name;
            Set_Info (Name, Integer (Integer (Index)));
         else
            Index := Index_Type (Info);
         end if;
         Leave_Critical_Section;

         return Index;
      end Get_Index;

      --------------
      -- Get_Name --
      --------------

      function  Get_Name  (N : Index_Type) return String
      is
         Name : Name_Id;
      begin
         Enter_Critical_Section;
         if Max < N or else Usage (N).Free then
            Name := Null_Name;
         else
            Name := Usage (N).Name;
         end if;
         Leave_Critical_Section;

         return Get (Name);
      end Get_Name;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Create (Mutex);
         Create (Watcher);
         Table := new Component_Table_Type'(Min .. Max => Null_Component);
         Usage := new Usage_Table_Type    '(Min .. Max => Null_Usage);
      end Initialize;

      ----------
      -- Last --
      ----------

      function Last return Index_Type is
      begin
         return Index_Type (Last_Pos);
      end Last;

      -----------
      -- Leave --
      -----------

      procedure Leave (Version : out Version_Id) is
      begin
         Lookup (Watcher, Version);
         Leave (Mutex);
      end Leave;

      -----------
      -- Leave --
      -----------

      procedure Leave is
      begin
         Leave (Mutex);
      end Leave;

      -------------------
      -- Set_Component --
      -------------------

      procedure Set_Component
        (N : in Index_Type;
         C : in Component_Type) is
      begin
         Enter_Critical_Section;
         Validate (N);
         Table (N) := C;
         Update (Watcher);
         Leave_Critical_Section;
      end Set_Component;

      --------------
      -- Set_Name --
      --------------

      procedure Set_Name (N : Index_Type; S : String)
      is
      begin
         Enter_Critical_Section;
         Validate (N);
         Usage (N).Name := Get (S);
         Set_Info (Usage (N).Name, Integer (N));
         Leave_Critical_Section;
      end Set_Name;

      ------------
      -- Update --
      ------------

      procedure Update is
      begin
         Update (Watcher);
      end Update;

      --------------
      -- Validate --
      --------------

      procedure Validate (N : Index_Type) is
         Dummy : Index_Type;
      begin
         pragma Assert (Min <= N);
         while N > Max loop
            Dummy := Allocate;
         end loop;
         if Last_Pos < Integer (N) then
            Last_Pos := Integer (N);
         end if;
      end Validate;

   end Complex;

   ------------
   -- Medium --
   ------------

   package body Medium is

      type Component_Table_Type is
         array (Index_Type range <>) of Component_Type;

      type Component_Table_Access is access Component_Table_Type;

      Table : Component_Table_Access;

      Min_Pos  : constant Integer := Integer (First_Index);
      Max_Pos  :          Integer := Min_Pos + Initial_Size - 1;
      Last_Pos :          Integer := Min_Pos - 1;

      Min     : constant Index_Type := Index_Type (Min_Pos);
      Max     :          Index_Type := Index_Type (Max_Pos);

      function Allocate return Index_Type;
      --  Allocate a new component.

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Component_Table_Type, Component_Table_Access);

      procedure Validate (N : Index_Type);

      Mutex   : Mutex_Access;
      Watcher : Watcher_Access;

      --  This lock is used to block tasks until the table is
      --  modified. This uses special behaviour of Types.Mutex_Record.
      --  Basically, Local_Mutex.Leave (Postponed) lets the run-time know
      --  that the mutex has been postponed and that it should be resumed
      --  when a Local_Mutex.Leave (Modified) occurs.

      --  Most of these subprograms are abort deferred. At the beginning of
      --  them, the code enter a critical section. At the end, it leaves
      --  the critical section. To avoid a premature abortion in the middle
      --  of the critical section, the code is protected against abortion.

      --------------
      -- Allocate --
      --------------

      function Allocate return Index_Type
      is
         Old_Max   : Index_Type;
         Old_Table : Component_Table_Access;
      begin

         --  Allocate new table

         Old_Max   := Max;
         Old_Table := Table;

         Max_Pos   := Max_Pos + Increment_Size;
         Max       := Index_Type (Max_Pos);
         Table     := new Component_Table_Type (Min .. Max);

         --  Copy old table in new table

         Table (Min .. Old_Max) := Old_Table (Min .. Old_Max);

         --  Intialize incremented part of new table

         Table (Old_Max + 1 .. Max) := (others => Null_Component);

         --  Release unused memory

         Free (Old_Table);
         return Old_Max + 1;
      end Allocate;

      ------------
      -- Differ --
      ------------

      procedure Differ (Version : in Types.Version_Id) is
      begin
         Differ (Watcher, Version);
      end Differ;

      -----------
      -- Enter --
      -----------

      procedure Enter is
      begin
         Enter (Mutex);
      end Enter;

      -------------------
      -- Get_Component --
      -------------------

      function Get_Component (N : Index_Type) return Component_Type
      is
         Component : Component_Type;
      begin
         Enter_Critical_Section;
         Validate (N);
         Component := Table (N);
         Leave_Critical_Section;

         return Component;
      end Get_Component;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Create (Mutex);
         Create (Watcher);
         Table := new Component_Table_Type'(Min .. Max => Null_Component);
      end Initialize;

      ----------
      -- Last --
      ----------

      function Last return Index_Type is
      begin
         return Index_Type (Last_Pos);
      end Last;

      -----------
      -- Leave --
      -----------

      procedure Leave (Version : out Version_Id) is
      begin
         Lookup (Watcher, Version);
         Leave (Mutex);
      end Leave;

      -----------
      -- Leave --
      -----------

      procedure Leave is
      begin
         Leave (Mutex);
      end Leave;

      -------------------
      -- Set_Component --
      -------------------

      procedure Set_Component
        (N : in Index_Type;
         C : in Component_Type) is
      begin
         Enter_Critical_Section;
         Validate (N);
         Table (N) := C;
         Update (Watcher);
         Leave_Critical_Section;
      end Set_Component;

      ------------
      -- Update --
      ------------

      procedure Update is
      begin
         Update (Watcher);
      end Update;

      --------------
      -- Validate --
      --------------

      procedure Validate (N : Index_Type) is
         Dummy : Index_Type;
      begin
         pragma Assert (Min <= N);
         while N > Max loop
            Dummy := Allocate;
         end loop;
         if Last_Pos < Integer (N) then
            Last_Pos := Integer (N);
         end if;
      end Validate;

   end Medium;

   ------------
   -- Simple --
   ------------

   package body Simple is

      Min_Pos  : constant Integer := Integer (First_Index);
      Max_Pos  :          Integer := Min_Pos + Initial_Size - 1;

      Min     : constant Index_Type := Index_Type (Min_Pos);
      Max     :          Index_Type := Index_Type (Max_Pos);
      Last    :          Index_Type := Index_Type (Min_Pos - 1);

      procedure Free is
        new Ada.Unchecked_Deallocation
        (Component_Table_Type, Component_Table_Access);

      --------------
      -- Allocate --
      --------------

      function Allocate return Index_Type
      is
         Old : Component_Table_Access;
      begin
         if Last = Max then
            Max_Pos := Max_Pos + Increment_Size;
            Max     := Index_Type (Max_Pos);
            Old     := Table;
            Table   := new Component_Table_Type (Min .. Max);

            Table (Min .. Last) := Old (Min .. Last);
            Table (Last + 1 .. Max) := (others => Null_Component);

            Free (Old);
         end if;

         Last := Last + 1;
         return Last;
      end Allocate;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         Table := new Component_Table_Type'(Min .. Max => Null_Component);
      end Initialize;

   end Simple;

end System.Garlic.Table;
