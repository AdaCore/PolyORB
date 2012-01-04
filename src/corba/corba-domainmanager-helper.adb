------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           C O R B A . D O M A I N M A N A G E R . H E L P E R            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

--  XXX This file should be generated using idlac

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with CORBA.Object.Helper;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);

package body CORBA.DomainManager.Helper is

   TC_DomainManager_Cache              : TypeCode.Object;
   TC_IDL_SEQUENCE_DomainManager_Cache : TypeCode.Object;
   TC_DomainManagersList_Cache         : TypeCode.Object;

   function Element_Wrap (X : access Ref) return PolyORB.Any.Content'Class;

   function Element_Wrap (X : access Ref) return PolyORB.Any.Content'Class is
   begin
      return CORBA.Object.Helper.Wrap
        (CORBA.Object.Ref (X.all)'Unrestricted_Access);
   end Element_Wrap;

   package IDL_SEQUENCE_DomainManager_Helper is
     new IDL_SEQUENCE_DomainManager.CORBA_Helper
       (Element_To_Any   => To_Any,
        Element_From_Any => From_Any,
        Element_Wrap     => Element_Wrap);

   procedure Deferred_Initialization;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      TC_DomainManager_Cache :=
        TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);
      Internals.Add_Parameter
        (TC_DomainManager_Cache, To_Any (To_CORBA_String ("DomainManager")));
      Internals.Add_Parameter
        (TC_DomainManager_Cache, To_Any (To_CORBA_String (Repository_Id)));

      TC_IDL_SEQUENCE_DomainManager_Cache :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (TC_DomainManager_Cache, 0);
      IDL_SEQUENCE_DomainManager_Helper.Initialize
        (Element_TC  => TC_DomainManager_Cache,
         Sequence_TC => TC_IDL_SEQUENCE_DomainManager_Cache);

      TC_DomainManagersList_Cache :=
        CORBA.TypeCode.Internals.Build_Alias_TC
          (Name   => To_CORBA_String ("DomainManagersList"),
           Id     => To_CORBA_String ("IDL:CORBA_A/DomainManagersList:1.0"),
           Parent => TC_IDL_SEQUENCE_DomainManager);
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any)
     return IDL_SEQUENCE_DomainManager.Sequence
     renames IDL_SEQUENCE_DomainManager_Helper.From_Any;

   function From_Any (Item : Any) return Ref is
   begin
      return To_Ref (Object.Helper.From_Any (Item));
   end From_Any;

   ----------------------
   -- TC_DomainManager --
   ----------------------

   function TC_DomainManager return TypeCode.Object is
   begin
      return TC_DomainManager_Cache;
   end TC_DomainManager;

   ---------------------------
   -- TC_DomainManagersList --
   ---------------------------

   function TC_DomainManagersList return TypeCode.Object is
   begin
      return TC_DomainManagersList_Cache;
   end TC_DomainManagersList;

   -----------------------------------
   -- TC_IDL_SEQUENCE_DomainManager --
   -----------------------------------

   function TC_IDL_SEQUENCE_DomainManager return TypeCode.Object is
   begin
      return TC_IDL_SEQUENCE_DomainManager_Cache;
   end TC_IDL_SEQUENCE_DomainManager;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : IDL_SEQUENCE_DomainManager.Sequence)
      return Any
      renames IDL_SEQUENCE_DomainManager_Helper.To_Any;

   function To_Any (Item : Ref) return Any is
      Result : Any := Object.Helper.To_Any (Object.Ref (Item));
   begin
      Internals.Set_Type (Result, TC_DomainManager);
      return Result;
   end To_Any;

   ------------
   -- To_Ref --
   ------------

   function To_Ref (The_Ref : Object.Ref'Class) return Ref is
   begin
      if Object.Is_Nil (The_Ref)
        or else Object.Is_A (The_Ref, Repository_Id)
      then
         return Unchecked_To_Ref (The_Ref);
      end if;

      Raise_Bad_Param (Default_Sys_Member);
   end To_Ref;

   ----------------------
   -- Unchecked_To_Ref --
   ----------------------

   function Unchecked_To_Ref (The_Ref : Object.Ref'Class) return Ref is
      Result : Ref;
   begin
      Set (Result, Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"corba.domainmanager.helper",
          Conflicts => Empty,
          Depends   => +"corba" & "any",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end CORBA.DomainManager.Helper;
