------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . C O R B A _ P . I N I T I A L _ R E F E R E N C E S    --
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

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Utils.HFunctions.Mul;
with PolyORB.Utils.HTables.Perfect;

package body PolyORB.CORBA_P.Initial_References is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.corba_p.initial_references");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --  Management of Initial references.

   --  Resolve_Initial_References may either return a reference to a
   --  pre allocated object (e.g. RootPOA), or return a newly created
   --  object (e.g. POACurrent). We build to hash tables to store each
   --  information.

   package Referenced_Objects_HTables is new PolyORB.Utils.HTables.Perfect
     (CORBA.Object.Ref,
      PolyORB.Utils.HFunctions.Mul.Hash_Mul_Parameters,
      PolyORB.Utils.HFunctions.Mul.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Mul.Hash,
      PolyORB.Utils.HFunctions.Mul.Next_Hash_Parameters);

   Referenced_Objects : Referenced_Objects_HTables.Table_Instance;
   --  Hash table of referenced objects

   package Referenced_Allocators_HTables is new PolyORB.Utils.HTables.Perfect
     (Create_Ptr,
      PolyORB.Utils.HFunctions.Mul.Hash_Mul_Parameters,
      PolyORB.Utils.HFunctions.Mul.Default_Hash_Parameters,
      PolyORB.Utils.HFunctions.Mul.Hash,
      PolyORB.Utils.HFunctions.Mul.Next_Hash_Parameters);

   Referenced_Allocators : Referenced_Allocators_HTables.Table_Instance;
   --  Hash table of referenced allocators

   --------------------------------
   -- Register_Initial_Reference --
   --------------------------------

   procedure Register_Initial_Reference
     (Id  : in Standard.String;
      Ref : in CORBA.Object.Ref) is
   begin
      pragma Debug (O ("Register_Initial_Reference: id " & Id));

      Referenced_Objects_HTables.Insert
        (Referenced_Objects, Id, Ref);
   end Register_Initial_Reference;

   procedure Register_Initial_Reference
     (Id        : in Standard.String;
      Allocator : in Create_Ptr) is
   begin
      pragma Debug (O ("Register_Initial_Reference: id " & Id));

      Referenced_Allocators_HTables.Insert
        (Referenced_Allocators, Id, Allocator);
   end Register_Initial_Reference;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Id : in Standard.String)
     return CORBA.Object.Ref
   is
      Nil_Ref : CORBA.Object.Ref;

   begin
      pragma Debug (O ("Resolve_Initial_Reference: id " & Id));

      --  Test if Id is in Referenced_Objects

      declare
         use Referenced_Objects_HTables;

         Result : CORBA.Object.Ref;

      begin
         Result := Lookup (Referenced_Objects, Id, Nil_Ref);

         if not CORBA.Object.Is_Nil (Result) then
            return Result;
         end if;
      end;

      --  Else test if Id is in Referenced_Allocators

      declare
         use Referenced_Allocators_HTables;

         Allocator : constant Create_Ptr :=
           Lookup (Referenced_Allocators, Id, null);

      begin
         if Allocator /= null then
            return Allocator.all;
         end if;
      end;

      --  Otherwise, return Nil_Ref

      pragma Debug (O ("Id not found !"));
      return Nil_Ref;
   end Resolve_Initial_References;

   ---------------------------
   -- List_Initial_Services --
   ---------------------------

   function List_Initial_Services
     return PolyORB.Utils.Strings.Lists.List
   is
      Result : PolyORB.Utils.Strings.Lists.List;

   begin

      --  Add all elements in Referenced_Objects

      declare
         use Referenced_Objects_HTables;

         It : Iterator := First (Referenced_Objects);

      begin
         while not Last (It) loop
            PolyORB.Utils.Strings.Lists.Append (Result, Key (It));

            Next (It);
         end loop;
      end;

      --  Add all elements in Referenced_Allocators

      declare
         use Referenced_Allocators_HTables;

         It : Iterator := First (Referenced_Allocators);

      begin
         while not Last (It) loop
            PolyORB.Utils.Strings.Lists.Append (Result, Key (It));

            Next (It);
         end loop;
      end;

      return Result;
   end List_Initial_Services;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin

      --  Initialize hash tables

      Referenced_Objects_HTables.Initialize (Referenced_Objects);
      Referenced_Allocators_HTables.Initialize (Referenced_Allocators);

   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.corba_p.initial_references",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"corba.initial_references",
       Init      => Initialize'Access));

end PolyORB.CORBA_P.Initial_References;
