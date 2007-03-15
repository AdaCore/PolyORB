------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y . H E L P E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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
      TC_Policy_Cache :=
        TypeCode.Internals.To_CORBA_Object (PolyORB.Any.TypeCode.TC_Object);
      TypeCode.Internals.Add_Parameter
        (TC_Policy_Cache, To_Any (To_CORBA_String ("Policy")));
      TypeCode.Internals.Add_Parameter
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
