------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . I N I T I A L I Z A T I O N                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2010, Free Software Foundation, Inc.          --
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

--  Automatic initialization of PolyORB subsystems

with PolyORB.Log;
with PolyORB.Platform;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Initialization is

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("polyorb.initialization");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   use String_Lists;

   -----------------------------
   -- Private data structures --
   -----------------------------

   type Module;
   type Module_Access is access all Module;

   package Module_Lists is new PolyORB.Utils.Chained_Lists (Module_Access);
   use Module_Lists;

   type Dependency is record
      Target   : Module_Access;
      --  The module being depended upon

      Optional : Boolean;
      --  If True, failure to initialize the target is not a fatal error
   end record;

   package Dep_Lists is new PolyORB.Utils.Chained_Lists (Dependency);
   use Dep_Lists;

   type Module (Virtual : Boolean) is record
      Deps             : Dep_Lists.List;
      --  Dependencies

      Visited          : Boolean := False;
      --  Has this module been traversed?

      Initialized      : Boolean := False;
      --  Has this module been actually initialized (differs from Visited if
      --  module is disabled).

      Deps_In_Progress : Boolean := False;
      --  Are the dependencies of this module being traversed?

      case Virtual is
         when False =>
            Info : Module_Info;

         when True =>
            Name : Utils.Strings.String_Ptr;
      end case;

   end record;

   Initialized : Boolean := False;

   type Init_Info_T is record
      World          : Module_Lists.List;
      --  The list of all modules

      Shutdown_Order : Module_Lists.List;
      --  List of finalization procedures for all initialized modules, in
      --  reverse initialization order.

      Implicit_Deps  : Dep_Lists.List;
      --  The list of modules marked as implicit dependencies
   end record;
   type Init_Info_A is access Init_Info_T;

   Init_Info : Init_Info_A;
   --  For Initialization to be preelaborable, it may not contain object
   --  declarations of a private type or a non-static initializer, so
   --  instead the data structures used here are allocated upon the first
   --  call to Register_Module.

   --------------------------
   -- Internal subprograms --
   --------------------------

   procedure Raise_Program_Error (Msg : String);
   pragma No_Return (Raise_Program_Error);
   --  Raise Program_Error with the given exception message

   procedure Check_Conflicts;
   --  For each module, check that it does not conflict with any other
   --  registered module. If no conflicts are detected, the name of the modules
   --  and its aliases (the names of the subsystems it implements) are entered
   --  into the global World list.

   procedure Resolve_Dependencies;
   --  For each registered module, construct the list of its
   --  direct dependencies.

   procedure Run_Initializers;
   --  Perform a topological sort on the dependency graph and
   --  initialize each module.

   function Module_Name
     (Module : Module_Access)
      return Utils.Strings.String_Ptr;
   --  Return the name of Module.

   function Lookup_Module (Name : String) return Module_Access;
   --  Look up module 'Name' in modules list, return null if not found.

   procedure Check_Duplicate (Name : String);
   --  Check that no module with the given name already exists, otherwise
   --  raise an exception.

   procedure Visit
     (M                            :     Module_Access;
      Circular_Dependency_Detected : out Boolean);
   --  Visit 'M' dependencies and run the corresponding initializers;
   --  Circular_Dependency_Detected reports circularity between modules.

   procedure Raise_Unresolved_Dependency (From, Upon : String);
   pragma No_Return (Raise_Unresolved_Dependency);
   --  Output a diagnostic message for an unresolved dependency, and
   --  raise the appropriate exception.

   -------------------
   -- Lookup_Module --
   -------------------

   function Lookup_Module (Name : String) return Module_Access is
      It : Module_Lists.Iterator := First (Init_Info.World);
   begin
      while not Last (It) loop
         if Module_Name (Value (It).all).all = Name then
            return Value (It).all;
         end if;

         Next (It);
      end loop;

      return null;
   end Lookup_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Info : Module_Info) is
      M : Module (Virtual => False);
   begin
      if Initialized then

         --  If we call Register_Module after Initialization is done,
         --  then there is a deep problem.

         Raise_Program_Error
           ("Initialization already done, cannot register " & Info.Name.all);
      end if;

      if Init_Info = null then
         Init_Info := new Init_Info_T;
      end if;

      M.Info := Info;
      declare
         New_M : constant Module_Access := new Module'(M);
      begin
         Check_Duplicate (Info.Name.all);
         Append (Init_Info.World, New_M);
         if Info.Implicit then
            Append (Init_Info.Implicit_Deps, Dependency'(
              Target   => New_M,
              Optional => True));
            --  We know for sure that the target module is present, but
            --  we need to make the dependency optional for the case where
            --  the module is disabled.
         end if;
      end;
   end Register_Module;

   ---------------------
   -- Check_Conflicts --
   ---------------------

   procedure Check_Conflicts is
      MI : Module_Lists.Iterator := First (Init_Info.World);
      SI : String_Lists.Iterator;

      Current     : Module_Access;
      Conflicting : Module_Access;
   begin

      --  Register all modules and aliases

      while not Last (MI) loop
         Current := Value (MI).all;

         exit when Current.Virtual;

         SI := First (Current.Info.Provides);
         while not Last (SI) loop

            declare
               Name    : String renames Value (SI).all;
               Last    : Integer := Name'Last;
               Virtual : Module_Access;
            begin
               if Name (Last) = '!' then
                  Last := Last - 1;
                  String_Lists.Append
                    (Current.Info.Conflicts, Name (Name'First .. Last));
               end if;

               Virtual := Lookup_Module (Name (Name'First .. Last));
               if Virtual = null then
                  Virtual := new Module (Virtual => True);
                  Virtual.Name := new String'(Name (Name'First .. Last));
                  Check_Duplicate (Virtual.Name.all);
                  Append (Init_Info.World, Virtual);
               end if;

               Prepend (Virtual.Deps, Dependency'(
                 Target   => Current,
                 Optional => False));
            end;

            Next (SI);
         end loop;
         Next (MI);
      end loop;

      --  Walk each conflict list, looking for a conflicting registered module

      MI := First (Init_Info.World);
      loop

         --  Skip over virtual modules (they do not have a conflicts list)

         while not Last (MI) loop
            Current := Value (MI).all;
            exit when not Current.Virtual;
            Next (MI);
         end loop;
         exit when Last (MI);

         --  Check for conflicting modules

         SI := First (Current.Info.Conflicts);

         while not Last (SI) loop
            Conflicting := Lookup_Module (Value (SI).all);

            if Conflicting /= null then
               if Conflicting.Virtual then
                  declare
                     First_Provider : constant Module_Access :=
                                        Value
                                          (First (Conflicting.Deps)).Target;
                  begin
                     --  For a conflict against a virtual module, do not fail
                     --  if Current is the only provider: the conflict entry
                     --  means in this case "conflict with any other provider".

                     if First_Provider = Current
                          and then Length (Conflicting.Deps) = 1
                     then
                        null;
                     else
                        Raise_Program_Error
                          ("Conflict between "
                           & Module_Name (Current).all
                           & " and "
                           & Module_Name (Conflicting).all
                           & " provided by "
                           & Module_Name (First_Provider).all);
                     end if;
                  end;
               else
                  Raise_Program_Error
                    ("Conflict between " & Module_Name (Current).all
                     & " and " & Module_Name (Conflicting).all);
               end if;
            end if;

            Next (SI);
         end loop;

         Next (MI);
      end loop;
   end Check_Conflicts;

   ---------------------
   -- Check_Duplicate --
   ---------------------

   procedure Check_Duplicate (Name : String) is
      Duplicate : constant Module_Access := Lookup_Module (Name);
   begin
      pragma Debug (C, O ("Registering " & Name));

      if Duplicate /= null then
         Raise_Program_Error
           ("Conflict: " & Name & " already registered.");
      end if;
   end Check_Duplicate;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error (Msg : String) is
   begin
      raise Program_Error with Msg;
   end Raise_Program_Error;

   --------------------------
   -- Resolve_Dependencies --
   --------------------------

   procedure Resolve_Dependencies is
      MI  : Module_Lists.Iterator := First (Init_Info.World);
      IDI : Dep_Lists.Iterator;
      SI  : String_Lists.Iterator;
      Current : Module_Access;
   begin
      while not Last (MI) loop

         Current := Value (MI).all;

         if not Current.Virtual then
            SI := First (Current.Info.Depends);

            while not Last (SI) loop
               declare
                  Dep_Name   : String renames Value (SI).all;
                  Last       : Integer := Dep_Name'Last;
                  Dep_Module : Module_Access;
                  Optional   : Boolean := False;
               begin
                  if Last in Dep_Name'Range
                    and then Dep_Name (Last) = '?'
                  then
                     Optional := True;
                     Last := Last - 1;
                  end if;

                  Dep_Module := Lookup_Module
                    (Dep_Name (Dep_Name'First .. Last));

                  if Dep_Module /= null then
                     Prepend (Current.Deps, Dependency'(
                       Target   => Dep_Module,
                       Optional => Optional));

                  elsif not Optional then
                     Raise_Unresolved_Dependency
                       (From => Module_Name (Current).all,
                        Upon => Dep_Name);
                  end if;
               end;

               if not Current.Info.Implicit then
                  IDI := First (Init_Info.Implicit_Deps);
                  while not Last (IDI) loop
                     Prepend (Current.Deps, Value (IDI).all);
                     Next (IDI);
                  end loop;
               end if;

               Next (SI);
            end loop;
         end if;

         Next (MI);
      end loop;
   end Resolve_Dependencies;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (M                            :     Module_Access;
      Circular_Dependency_Detected : out Boolean)
   is
      MI  : Dep_Lists.Iterator;
      Dep : Dependency;
      One_Dep_Initialized : Boolean := False;
   begin
      if M.Deps_In_Progress then
         O (M.Info.Name.all & " is part of a cycle:", Critical);
         Circular_Dependency_Detected := True;
         return;
      end if;

      Circular_Dependency_Detected := False;

      --  Note that we visit the dependencies of a module even if the module
      --  is disabled. This is necessary to ensure that it is possible to
      --  disable any module that depends on "parameters".

      M.Deps_In_Progress := True;
      MI := First (M.Deps);

      while not Last (MI) loop
         Dep := Value (MI).all;

         pragma Debug (C, O ("DEP: """
                          & Module_Name (M).all & """ -> """
                          & Module_Name (Dep.Target).all & """;"));

         if not Dep.Target.Visited then
            begin
               Visit (Dep.Target, Circular_Dependency_Detected);
               if Circular_Dependency_Detected then
                  O ("... depended upon by " & Module_Name (Dep.Target).all,
                     Critical);
                  return;
               end if;

               if not (False
                 or else Dep.Optional
                 or else M.Virtual
                 or else Dep.Target.Initialized)
               then

                  --  Non-optional dependency of a non-virtual module upon a
                  --  module that is disabled.

                  Raise_Unresolved_Dependency
                    (From => Module_Name (M).all,
                     Upon => Module_Name (Dep.Target).all);
                  return;
               end if;
            end;
         end if;

         if Dep.Target.Initialized then
            One_Dep_Initialized := True;
         end if;

         Next (MI);
      end loop;

      M.Deps_In_Progress := False;
      pragma Debug (C, O ("Processed dependencies of " & Module_Name (M).all));

      if Get_Conf_Hook /= null and then
        not Utils.Strings.To_Boolean
          (Get_Conf_Hook ("modules", Module_Name (M).all, "enable"))
      then

         --  This module is not enabled.

         return;
      end if;

      if M.Virtual then
         --  A virtual module is considered as initialized if at least
         --  one of its providers had been initialized.

         M.Initialized := One_Dep_Initialized;
      else
         begin
            M.Info.Init.all;
            M.Initialized := True;
         exception
            when others =>

               --  XXX When all supported compilers honor the pragma
               --  Preelaborate_05 in Ada.Exceptions, we can add exception
               --  information to this message.

               O ("Initialization of " & Module_Name (M).all
                  & " raised an exception");
               raise;
         end;

         --  If module needs to be shut down, we add it to the shutdown list

         if M.Info.Shutdown /= null then
            Prepend (Init_Info.Shutdown_Order, M);
         end if;
      end if;
      O ("Initialization of " & Module_Name (M).all
         & " was successful.");
      M.Visited := True;
   end Visit;

   ----------------------
   -- Run_Initializers --
   ----------------------

   procedure Run_Initializers is
      MI : Module_Lists.Iterator := First (Init_Info.World);
      M  : Module_Access;

      Circular_Dependency_Detected : Boolean;
   begin
      while not Last (MI) loop
         M := Value (MI).all;

         if not M.Visited then
            Visit (M, Circular_Dependency_Detected);

            if Circular_Dependency_Detected then
               Raise_Program_Error ("Circular dependency detected");
            end if;
         end if;

         Next (MI);
      end loop;
   end Run_Initializers;

   ----------------------
   -- Initialize_World --
   ----------------------

   procedure Initialize_World is
   begin
      if Initialized then
         Raise_Program_Error ("Already initialized");
      end if;

      pragma Debug (C, O ("Initializing PolyORB " & Platform.Version));

      if Init_Info /= null then

         --  Initialize registered modules:
         --  Recursive traversal of the dependency graph then initialize
         --  each module in reverse topological order.

         Check_Conflicts;
         Resolve_Dependencies;
         Run_Initializers;
      end if;

      Initialized := True;
   end Initialize_World;

   --------------------
   -- Is_Initialized --
   --------------------

   function Is_Initialized return Boolean is
   begin
      return Initialized;
   end Is_Initialized;

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name
     (Module : Module_Access)
      return Utils.Strings.String_Ptr
   is
   begin
      if Module.Virtual then
         return Module.Name;
      else
         return Module.Info.Name;
      end if;
   end Module_Name;

   ---------------------------------
   -- Raise_Unresolved_Dependency --
   ---------------------------------

   procedure Raise_Unresolved_Dependency (From, Upon : String) is
   begin
      Raise_Program_Error ("Unresolved dependency: " & From & " -> " & Upon);
   end Raise_Unresolved_Dependency;

   --------------------
   -- Shutdown_World --
   --------------------

   procedure Shutdown_World (Wait_For_Completion : Boolean := True)
   is
      L : Module_Lists.List renames Init_Info.Shutdown_Order;
      M : Module_Access;
   begin
      pragma Debug (C, O ("Shutting down PolyORB"));

      while not Is_Empty (L) loop
         Extract_First (L, M);
         pragma Debug (C, O ("Shutting down module " & Module_Name (M).all));
         M.Info.Shutdown (Wait_For_Completion);
      end loop;

      pragma Debug (C, O ("Shutdown of PolyORB completed"));
   end Shutdown_World;

end PolyORB.Initialization;
