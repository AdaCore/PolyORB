------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . I N I T I A L I Z A T I O N                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  Automatic initialization of PolyORB subsystems.

--  $Id$

with Ada.Exceptions;

with PolyORB.Dynamic_Dict;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Initialization is

   use String_Lists;

   use PolyORB.Log;
   package L is new PolyORB.Log.Facility_Log ("polyorb.initialization");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------------
   -- Private data structures --
   -----------------------------

   type Module;
   type Module_Access is access all Module;

   package Dep_Lists is new PolyORB.Utils.Chained_Lists (Module_Access);
   use Dep_Lists;

   type Module (Virtual : Boolean) is record

      Deps        : Dep_Lists.List;
      Visited     : Boolean := False;
      In_Progress : Boolean := False;

      case Virtual is
         when False =>
            Info : Module_Info;

         when True =>
            Name : Utils.Strings.String_Ptr;
      end case;

   end record;

   Initialized : Boolean := False;
   World : Dep_Lists.List;
   package World_Dict is new PolyORB.Dynamic_Dict (Value => Module_Access);

   Implicit_Dependencies : Dep_Lists.List;

   --------------------------
   -- Internal subprograms --
   --------------------------

   procedure Check_Conflicts;
   --  For each module, check that it does not conflict
   --  with any other registered module. If no conflicts
   --  are detected, the name of the modules and its aliases
   --  (the names of the subsystems it implements) are entered
   --  into the global World_Dict.

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
   --  Look up module 'Name' in module table, return null if not found.

   procedure Enter_Module_Name (Name : String; Module : Module_Access);
   --  Associate Name with Module in module table.

   procedure Visit
     (M                            :     Module_Access;
      Circular_Dependency_Detected : out Boolean);
   --  Visit 'M' dependencies and run the corresponding initializers;
   --  Circular_Dependency_Detected reports circularity between modules.

   procedure Raise_Unresolved_Dependency
     (From, Upon : String);
   pragma No_Return (Raise_Unresolved_Dependency);
   --  Output a diagnostic message for an unresolved dependency, and
   --  raise the appropriate exception.

   -------------------
   -- Lookup_Module --
   -------------------

   function Lookup_Module
     (Name : String)
     return Module_Access is
   begin
      return World_Dict.Lookup (Name, Default => null);
   end Lookup_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Info : Module_Info) is
      M : Module (Virtual => False);
   begin
      if Initialized then
         pragma Debug (O ("Initialization already done, cannot register "
                          & Info.Name.all));
         raise Program_Error;
         --  If we call Register_Module after Initialization is done,
         --  then there is a deep problem.
      end if;

      M.Info := Info;
      declare
         New_M : constant Module_Access := new Module'(M);
      begin
         Append (World, New_M);
         if Info.Implicit then
            Append (Implicit_Dependencies, New_M);
         end if;
      end;
   end Register_Module;

   -----------------------
   -- Enter_Module_Name --
   -----------------------

   procedure Enter_Module_Name (Name : String; Module : Module_Access) is
      Duplicate : constant Module_Access := Lookup_Module (Name);
   begin
      pragma Debug (O ("Registering " & Name));

      if Duplicate /= null then
         O (Name & " already registered.", Critical);
         raise Conflict;
      end if;

      pragma Assert (Module_Name (Module).all = Name);
      --  XXX at some point, remove the Name argument!

      World_Dict.Register (Name, Module);
   end Enter_Module_Name;

   ---------------------
   -- Check_Conflicts --
   ---------------------

   procedure Check_Conflicts
   is
      MI : Dep_Lists.Iterator := First (World);
      SI : String_Lists.Iterator;

      Current     : Module_Access;
      Conflicting : Module_Access;
   begin

      --  Register all modules and aliases

      while not Last (MI) loop
         Current := Value (MI).all;

         pragma Assert (not Current.Virtual);
         Enter_Module_Name (Current.Info.Name.all, Current);

         SI := First (Current.Info.Provides);
         while not Last (SI) loop

            declare
               Name    : String renames Value (SI).all;
               Virtual : Module_Access := Lookup_Module (Name);
            begin
               if Virtual = null then
                  Virtual := new Module (Virtual => True);
                  Virtual.Name := Value (SI);
                  Enter_Module_Name (Name, Virtual);
               end if;
               Prepend (Virtual.Deps, Current);
            end;

            Next (SI);
         end loop;
         Next (MI);
      end loop;

      --  Walk each conflict list, looking for a conflicting
      --  registered module.

      MI := First (World);
      while not Last (MI) loop
         Current := Value (MI).all;

         SI := First (Current.Info.Conflicts);

         while not Last (SI) loop
            Conflicting := Lookup_Module (Value (SI).all);

            if Conflicting /= null then
               O ("Conflict between " & Module_Name (Current).all
                  & " and " & Module_Name (Conflicting).all, Critical);
               raise Conflict;
            end if;

            Next (SI);
         end loop;

         Next (MI);
      end loop;
   end Check_Conflicts;

   --------------------------
   -- Resolve_Dependencies --
   --------------------------

   procedure Resolve_Dependencies
   is
      MI : Dep_Lists.Iterator := First (World);
      IDI : Dep_Lists.Iterator;
      SI : String_Lists.Iterator;
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
                     Prepend (Current.Deps, Dep_Module);

                  elsif not Optional then
                     Raise_Unresolved_Dependency
                       (From => Current.Info.Name.all, Upon => Dep_Name);
                  end if;
               end;

               if not Current.Info.Implicit then
                  IDI := First (Implicit_Dependencies);
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
      Dep : Module_Access;
   begin
      if M.In_Progress then
         O (M.Info.Name.all & " is part of a cycle:", Critical);
         Circular_Dependency_Detected := True;
         return;
      end if;

      Circular_Dependency_Detected := False;

      if not Parameters.Get_Conf ("modules", Module_Name (M).all, True) then

         --  This module is not enabled.

         return;
      end if;

      M.In_Progress := True;
      MI := First (M.Deps);

      while not Last (MI) loop
         Dep := Value (MI).all;

         pragma Debug (O ("DEP: """
                          & Module_Name (M).all & """ -> """
                          & Module_Name (Dep).all & """;"));

         if not Dep.Visited then
            begin
               Visit (Dep, Circular_Dependency_Detected);
               if Circular_Dependency_Detected then
                  O ("... depended upon by " & Dep.Info.Name.all, Critical);
                  return;
               end if;

               if not Dep.Visited then

                  --  Case of a dependency that is disabled.

                  Raise_Unresolved_Dependency
                    (From => M.Name.all, Upon => Dep.Name.all);
                  return;
               end if;
            end;
         end if;

         Next (MI);
      end loop;

      pragma Debug (O ("Processed dependencies of " & Module_Name (M).all));

      if not M.Virtual then
         begin
            M.Info.Init.all;
         exception
            when E : others =>
               O ("Initialization of " & Module_Name (M).all & " failed: "
                  & Ada.Exceptions.Exception_Information (E), Warning);
               raise;
         end;
      end if;

      M.Visited := True;
      M.In_Progress := False;
   end Visit;

   ----------------------
   -- Run_Initializers --
   ----------------------

   procedure Run_Initializers
   is
      MI : Dep_Lists.Iterator := First (World);
      M  : Module_Access;

      Circular_Dependency_Detected : Boolean;
   begin
      while not Last (MI) loop
         M := Value (MI).all;

         if not M.Visited then
            Visit (M, Circular_Dependency_Detected);

            if Circular_Dependency_Detected then
               raise Circular_Dependency;
            end if;
         end if;

         Next (MI);
      end loop;
   end Run_Initializers;

   ----------------------
   -- Initialize_World --
   ----------------------

   procedure Initialize_World
   is
      use PolyORB.Parameters;

   begin
      if Initialized then
         raise Already_Initialized;
      end if;

      --  Initialize Configuration subsystem

      PolyORB.Parameters.Set_Hooks;

      --  Initialize registered packages:
      --  Recursive traversal of the dependency graph then initialize
      --  each module in reverse topological order.

      Check_Conflicts;
      Resolve_Dependencies;
      Run_Initializers;

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

   ------------------------------------
   -- Diagnose_Unresolved_Dependency --
   ------------------------------------

   procedure Raise_Unresolved_Dependency
     (From, Upon : String) is
   begin
      O ("Unresolved dependency: " & From & " -> " & Upon, Critical);
      raise Unresolved_Dependency;
   end Raise_Unresolved_Dependency;

end PolyORB.Initialization;
