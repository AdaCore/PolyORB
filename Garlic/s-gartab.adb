with Ada.Unchecked_Deallocation;
with System.Garlic.Utils;      use System.Garlic.Utils;
with System.Garlic.Table;
with System.Garlic.Name_Table; use System.Garlic.Name_Table;

package body System.Garlic.Table is

   package body Concurrent is

      --  This table is more complex than the sequential one. Each entry
      --  includes a name and a component. This allows to retrieve the
      --  name of a component with its index.

      type Fat_Component_Type is record
         Name : Name_Id;
         Data : Component_Type;
      end record;
      Null_Fat_Component : Fat_Component_Type := (Null_Name, Null_Component);

      package Sequential is new Table.Sequential
        (Index_Type, Initial_Size, Increment_Size,
         Fat_Component_Type, Null_Fat_Component);

      Sema : Semaphore_Type;
      --  This lock is used to block tasks until the table is
      --  modified. This uses special behaviour of
      --  Utils.Semaphore_Type. Basically, Sema.Unlock (Postponed) lets the
      --  run-time know that the lock has been postponed and that it should
      --  be resume when a Sema.Unlock (Modified) occurs.

      --  Most of these subprograms are abort deferred. At the beginning of
      --  them, the code enter a critical section. At the end, it leaves
      --  the critical section. To avoid a premature abortion in the middle
      --  of the critical section, the code is protected against abortion.

      --------------
      -- Allocate --
      --------------

      function Allocate (N : Positive := 1) return Index_Type is
         Index : Index_Type;

      begin
         pragma Abort_Defer;

         Enter;
         Index := Sequential.Allocate (N);
         Leave;
         return Index;
      end Allocate;

      -----------
      -- Apply --
      -----------

      procedure Apply
        (N         : in Index_Type;
         Parameter : in Parameter_Type;
         Process   : in Process_Type) is
         Status    : Status_Type;
         Component : Fat_Component_Type;

      begin
         pragma Abort_Defer;

         loop
            Sema.Lock;
            Enter;

            Component := Sequential.Get (N);
            Process (N, Parameter, Component.Data, Status);

            --  If the component has not been modified, then don't update
            --  it in the table.

            if Status /= Unmodified then
               Sequential.Set (N, Component);
            end if;

            Leave;
            Sema.Unlock (Status);

            --  Loop when the subprogram execution has been postponed.

            exit when Status /= Postponed;
         end loop;
      end Apply;

      ---------
      -- Get --
      ---------

      function Get (S : String) return Index_Type is
         Index : Index_Type;
         Name  : Name_Id;
         Info  : Integer;

      begin
         pragma Abort_Defer;

         Enter;
         Name  := Get (S);
         Info  := Get_Info (Name);
         if Info = 0 then

            --  Info has a null value. Create a new component and store its
            --  index as the name info.

            Index := Allocate;
            Sequential.Set (Index, (Name, Null_Component));
            Set_Info (Name, Integer (Index_Type'Pos (Index)));
         else
            Index := Index_Type'Val (Info);
         end if;
         Leave;
         return Index;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (N : Index_Type) return String is
      begin
         return Get (Sequential.Get (N).Name);
      end Get;

      ---------
      -- Get --
      ---------

      function Get (N : Index_Type) return Component_Type is
         Component : Fat_Component_Type;

      begin
         pragma Abort_Defer;

         Enter;
         Component := Sequential.Get (N);
         Leave;
         return Component.Data;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (N : Index_Type; C : Component_Type) is
      begin
         pragma Abort_Defer;

         Enter;
         Sequential.Set (N, (Sequential.Get (N).Name, C));
         Leave;
      end Set;

   end Concurrent;

   package body Sequential is

      Min_Pos : constant Integer := Index_Type'Pos (Null_Index) + 1;
      Max_Pos :          Integer := Min_Pos + Initial_Size;

      Min     : constant Index_Type := Index_Type'Val (Min_Pos);
      Max     :          Index_Type := Index_Type'Val (Max_Pos);

      type Table_Type is array (Index_Type range <>) of Component_Type;
      type Table_Ptr  is access Table_Type;

      procedure Free is new Ada.Unchecked_Deallocation (Table_Type, Table_Ptr);

      Last  : Index_Type := Null_Index;
      Table : Table_Ptr   := new Table_Type'(Min .. Max => Null_Component);

      procedure Reallocate;

      --------------
      -- Allocate --
      --------------

      function Allocate (N : Positive := 1) return Index_Type is
         Pos   : Integer;
         Index : Index_Type;

      begin
         Pos   := Index_Type'Pos (Last);
         Index := Index_Type'Val (Pos + 1);
         Pos   := Pos + N;
         Last  := Index_Type'Val (Pos);

         if Pos > Max_Pos then
            Reallocate;
         end if;

         return Index;
      end Allocate;

      ---------
      -- Get --
      ---------

      function Get (N : Index_Type) return Component_Type is
      begin
         return Table (N);
      end Get;

      ----------------
      -- Reallocate --
      ----------------

      procedure Reallocate is
         New_Pos   : Integer    := Max_Pos + Increment_Size;
         New_Max   : Index_Type := Index_Type'Val (New_Pos);
         New_Table : Table_Ptr   := new Table_Type (Min .. New_Max);

      begin
         for Index in Min .. Last loop
            New_Table (Index) := Table (Index);
         end loop;
         for Index in Last + 1 .. Max loop
            New_Table (Index) := Null_Component;
         end loop;
         Free (Table);
         Table := New_Table;
      end Reallocate;

      ---------
      -- Set --
      ---------

      procedure Set (N : Index_Type; C : Component_Type) is
      begin
         Table (N) := C;
      end Set;

   end Sequential;

end System.Garlic.Table;
