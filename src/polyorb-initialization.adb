------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . I N I T I A L I Z A T I O N                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Automatic initialization of PolyORB subsystems.

--  $Id$

with PolyORB.Configuration;
with PolyORB.Dynamic_Dict;
with PolyORB.Log;
with PolyORB.Utils.Chained_Lists;

package body PolyORB.Initialization is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.initialization");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------------
   -- Private data structures --
   -----------------------------

   type Module;
   type Module_Access is access all Module;

   package Dep_Lists is new PolyORB.Utils.Chained_Lists
     (Module_Access);
   use Dep_Lists;

   use String_Lists;

   type Module is record
      Info    : Module_Info;
      Deps    : Dep_Lists.List;
      Visited : Boolean := False;
      In_Progress : Boolean := False;
   end record;

   -----------------
   -- Global data --
   -----------------

   Initialized : Boolean := False;
   World : Dep_Lists.List;
   package World_Dict is new PolyORB.Dynamic_Dict
     (Value => Module_Access, No_Value => null);

   function Lookup_Module (Name : String) return Module_Access;

   function Lookup_Module (Name : String) return Module_Access is
   begin
      return World_Dict.Lookup (Name, Default => null);
   end Lookup_Module;

   procedure Register_Module (Info : Module_Info)
   is
      M : Module;
   begin
      if Initialized then
         pragma Debug (O ("Initialization already done, cannot register "
                          & Info.Name.all));
         raise Program_Error;
         --  If we call Register_Module after Initialization is done,
         --  then there is a deep problem.
      end if;

      if not Configuration.Get_Conf ("modules", Info.Name.all, True) then
         pragma Debug (O (Info.Name.all & " is disabled."));
         return;
      end if;

      M.Info := Info;
      Append (World, new Module'(M));
   end Register_Module;

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

   procedure Check_Conflicts is
      use String_Lists;

      MI : Dep_Lists.Iterator := First (World);
      SI : String_Lists.Iterator;
      Current : Module_Access;

      procedure Register_Module
        (Name : String; Module : Module_Access);

      procedure Register_Module
        (Name : String; Module : Module_Access)
      is
         Duplicate : constant Module_Access
           := Lookup_Module (Name);
      begin
         pragma Debug (O ("Registering " & Name));
         if Duplicate /= null then
            O (Name & " already registered by "
               & Duplicate.Info.Name.all, Critical);
            raise Conflict;
         end if;
         World_Dict.Register (Name, Module);
      end Register_Module;

      Conflicting : Module_Access;
   begin

      --  Register all modules and aliases

      while not Last (MI) loop
         Current := Value (MI).all;

         Register_Module (Current.Info.Name.all, Current);
         SI := First (Current.Info.Provides);
         while not Last (SI) loop
            Register_Module (Value (SI).all, Current);
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
               O ("Conflict between " & Current.Info.Name.all
                  & " and " & Conflicting.Info.Name.all, Critical);
               raise Conflict;
            end if;
            Next (SI);
         end loop;
         Next (MI);
      end loop;
   end Check_Conflicts;

   procedure Resolve_Dependencies is
      MI : Dep_Lists.Iterator := First (World);
      SI : String_Lists.Iterator;
      Current : Module_Access;
   begin
      while not Last (MI) loop
         Current := Value (MI).all;
         SI := First (Current.Info.Depends);
         while not Last (SI) loop
            declare
               Dep_Name : String renames Value (SI).all;
               Dep_Module : Module_Access;
               Last : Integer := Dep_Name'Last;
               Optional : Boolean := False;
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
                  O ("Unresolved dependency: "
                       & Current.Info.Name.all & " -> "
                       & Dep_Name, Critical);
                  raise Unresolved_Dependency;
               end if;
            end;
            Next (SI);
         end loop;

         Next (MI);
      end loop;
   end Resolve_Dependencies;

   -----------------------------------------------------------
   -- Recursive traversal of the dependency graph           --
   -- (initialize each module in reverse topological order) --
   -----------------------------------------------------------

   procedure Visit (M : Module_Access);

   procedure Visit (M : Module_Access) is
      MI : Dep_Lists.Iterator;
      Dep : Module_Access;
   begin
      if M.In_Progress then
         O (M.Info.Name.all & " is part of a cycle.", Critical);
         raise Circular_Dependency;
      end if;

      M.In_Progress := True;
      MI := First (M.Deps);
      while not Last (MI) loop
         Dep := Value (MI).all;
         if not Dep.Visited then
            Visit (Dep);
         end if;
         Next (MI);
      end loop;
      pragma Debug (O ("Initializing " & M.Info.Name.all));
      M.Info.Init.all;
      M.Visited := True;
      M.In_Progress := False;
   end Visit;

   procedure Run_Initializers is
      MI : Dep_Lists.Iterator := First (World);
      M  : Module_Access;
   begin
      while not Last (MI) loop
         M := Value (MI).all;
         if not M.Visited then
            Visit (M);
         end if;
         Next (MI);
      end loop;
   end Run_Initializers;

   procedure Initialize_World is
   begin
      if Initialized then
         raise Already_Initialized;
      end if;

      Check_Conflicts;
      Resolve_Dependencies;
      Run_Initializers;

      Initialized := True;
   end Initialize_World;

   function Is_Initialized return Boolean is
   begin
      return Initialized;
   end Is_Initialized;

end PolyORB.Initialization;
