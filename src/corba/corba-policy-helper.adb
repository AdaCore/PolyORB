------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y . H E L P E R                   --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with CORBA.Object.Helper;

package body CORBA.Policy.Helper is

   Repository_Id : constant Standard.String := "IDL:omg.org/CORBA/Policy:1.0";

   procedure Deferred_Initialization;

   TC_Policy_Cache : TypeCode.Object;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      TC_Policy_Cache := CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Object);
      Internals.Add_Parameter
        (TC_Policy_Cache, To_Any (To_CORBA_String ("Policy")));
      Internals.Add_Parameter
        (TC_Policy_Cache, To_Any (To_CORBA_String (Repository_Id)));
   end Deferred_Initialization;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : Any) return Ref is
   begin
      return To_Ref (CORBA.Object.Helper.From_Any (Item));
   end From_Any;

   ---------------
   -- TC_Policy --
   ---------------

   function TC_Policy return TypeCode.Object is
   begin
      return TC_Policy_Cache;
   end TC_Policy;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : Ref) return Any is
      Result : Any := Object.Helper.To_Any (Object.Ref (Item));
   begin
      Internals.Set_Type (Result, TC_Policy);
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
         (Name      => +"corba.policy.helper",
          Conflicts => Empty,
          Depends   => +"corba" & "any",
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access,
          Shutdown  => null));
   end;
end CORBA.Policy.Helper;
