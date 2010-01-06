------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y M A N A G E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with PolyORB.Annotations;
with PolyORB.CORBA_P.Initial_References;
with PolyORB.CORBA_P.Policy_Management;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body CORBA.PolicyManager is

   use PolyORB.Annotations;
   use PolyORB.CORBA_P.Policy_Management;
   use PolyORB.Setup;
   use PolyORB.Tasking.Mutexes;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
     return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         "IDL:omg.org/CORBA/PolicyManager:1.0")
        or else CORBA.Is_Equivalent
        (Logical_Type_Id,
         "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   --------------------------
   -- Get_Policy_Overrides --
   --------------------------

   function Get_Policy_Overrides
     (Self : Local_Ref;
      TS   : CORBA.Policy.PolicyTypeSeq) return CORBA.Policy.PolicyList is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Get_Policy_Overrides (Object_Ptr (Entity_Of (Self)), TS);
   end Get_Policy_Overrides;

   function Get_Policy_Overrides
     (Self : access Object;
      TS   : CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList
   is
      Npad   : Notepad_Access;
      Note   : Policy_Manager_Note;
      Result : CORBA.Policy.PolicyList;

   begin
      Enter (Self.Lock);

      Npad := PolyORB.ORB.Notepad_Of (The_ORB);
      Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);

      Result := Get_Policy_Overrides (Note.Overrides, TS);

      Leave (Self.Lock);

      return Result;
   end Get_Policy_Overrides;

   --------------------------
   -- Set_Policy_Overrides --
   --------------------------

   procedure Set_Policy_Overrides
     (Self     : Local_Ref;
      Policies : CORBA.Policy.PolicyList;
      Set_Add  : SetOverrideType)
   is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Set_Policy_Overrides (Object_Ptr (Entity_Of (Self)), Policies, Set_Add);
   end Set_Policy_Overrides;

   procedure Set_Policy_Overrides
     (Self     : access Object;
      Policies : CORBA.Policy.PolicyList;
      Set_Add  : CORBA.SetOverrideType)
   is
      Npad    : Notepad_Access;
      Note    : Policy_Manager_Note;
      Indexes : CORBA.Unsigned_Short;

   begin
      Enter (Self.Lock);

      Npad := PolyORB.ORB.Notepad_Of (The_ORB);

      if Set_Add = ADD_OVERRIDE then
         Get_Note (Npad.all, Note, Empty_Policy_Manager_Note);
      end if;

      Add_Policy_Overrides (Note.Overrides, Policies, ORB_Level);

      Check_Compatibility (Note.Overrides, Indexes);

      if Indexes /= 0 then
         raise Program_Error;
         --  XXX should raise the CORBA.InvalidPolicies exception
      end if;

      Set_Note (Npad.all, Note);
      Leave (Self.Lock);

   exception
      when others =>
         Leave (Self.Lock);
         raise;
   end Set_Policy_Overrides;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
      PM_Object : constant Object_Ptr := new Object;
      Ref : CORBA.Object.Ref;
   begin
      Create (PM_Object.Lock);

      CORBA.Object.Set (Ref, PolyORB.Smart_Pointers.Entity_Ptr (PM_Object));

      PolyORB.CORBA_P.Initial_References.Register_Initial_Reference
        ("ORBPolicyManager",
         Ref);
      --  There is at most one ORBPolicyManager per partition
   end Deferred_Initialization;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba.policymanager",
       Conflicts => Empty,
       Depends   => +"corba.initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Deferred_Initialization'Access,
       Shutdown  => null));
end CORBA.PolicyManager;
