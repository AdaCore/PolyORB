------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y C U R R E N T                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

with PolyORB.Annotations;
with PolyORB.Initial_References;
with PolyORB.CORBA_P.Policy_Management;
with PolyORB.Initialization;
with PolyORB.References;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Strings;

package body CORBA.PolicyCurrent is

   use PolyORB.Annotations;
   use PolyORB.CORBA_P.Policy_Management;
   use PolyORB.Tasking.Threads.Annotations;

   ----------
   -- Is_A --
   ----------

   overriding function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/PolicyCurrent:1.0")
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/PolicyManager:1.0")
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Current:1.0")
          or else
        CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   --------------------------
   -- Get_Policy_Overrides --
   --------------------------

   overriding function Get_Policy_Overrides
     (Self : Local_Ref;
      TS   : CORBA.Policy.PolicyTypeSeq) return CORBA.Policy.PolicyList
   is
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
      pragma Unreferenced (Self);

      Notepad : Notepad_Access;
      Note    : Policy_Manager_Note;

   begin
      Notepad := Get_Current_Thread_Notepad;

      Get_Note (Notepad.all, Note, Empty_Policy_Manager_Note);

      return Get_Policy_Overrides (Note.Overrides, TS);
   end Get_Policy_Overrides;

   --------------------------
   -- Set_Policy_Overrides --
   --------------------------

   overriding procedure Set_Policy_Overrides
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
      pragma Unreferenced (Self);

      Notepad : Notepad_Access;
      Note    : Policy_Manager_Note;
      Indexes : CORBA.Unsigned_Short;

   begin
      Notepad := Get_Current_Thread_Notepad;

      if Set_Add = ADD_OVERRIDE then
         Get_Note (Notepad.all, Note, Empty_Policy_Manager_Note);
      end if;

      Add_Policy_Overrides (Note.Overrides, Policies, Thread_Level);

      Check_Compatibility (Note.Overrides, Indexes);

      if Indexes /= 0 then
         raise Program_Error;
         --  XXX We must raise CORBA.InvalidPolicies exception, but it
         --  is not (yet) defined in CORBA package.
      end if;

      Set_Note (Notepad.all, Note);
   end Set_Policy_Overrides;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
      Ref : PolyORB.References.Ref;
   begin
      Ref.Set (new Object);
      PolyORB.Initial_References.Register_Initial_Reference
        ("PolicyCurrent", Ref);
   end Deferred_Initialization;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"corba.policycurrent",
       Conflicts => Empty,
       Depends   => +"initial_references",
       Provides  => Empty,
       Implicit  => False,
       Init      => Deferred_Initialization'Access,
       Shutdown  => null));
end CORBA.PolicyCurrent;
