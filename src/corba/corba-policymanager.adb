------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . P O L I C Y M A N A G E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

--  $Id$

with PolyORB.CORBA_P.Initial_References;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body CORBA.PolicyManager is

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : in     Standard.String)
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
     (Self : in Local_Ref;
      TS   : in CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Get_Policy_Overrides (Object_Ptr (Entity_Of (Self)), TS);
   end Get_Policy_Overrides;

   function Get_Policy_Overrides
     (Self : access Object;
      TS   : in     CORBA.Policy.PolicyTypeSeq)
     return CORBA.Policy.PolicyList
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (TS);

      Result : CORBA.Policy.PolicyList;
   begin

      --  Insert implementation of get_policy_overrides

      return Result;
   end Get_Policy_Overrides;

   --------------------------
   -- Set_Policy_Overrides --
   --------------------------

   procedure Set_Policy_Overrides
     (Self     : in Local_Ref;
      Policies : in CORBA.Policy.PolicyList;
      Set_Add  : in SetOverrideType)
   is
      Self_Ref : CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      Set_Policy_Overrides (Object_Ptr (Entity_Of (Self)), Policies, Set_Add);
   end Set_Policy_Overrides;

   procedure Set_Policy_Overrides
     (Self     : access Object;
      Policies : in     CORBA.Policy.PolicyList;
      Set_Add  : in     CORBA.SetOverrideType)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Policies);
      pragma Unreferenced (Set_Add);

   begin

      --  Insert implementation of set_policy_overrides

      null;
   end Set_Policy_Overrides;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
      Ptr : constant Object_Ptr := new Object;
      Ref : CORBA.Object.Ref;
   begin
      CORBA.Object.Set (Ref, PolyORB.Smart_Pointers.Entity_Ptr (Ptr));

      PolyORB.CORBA_P.Initial_References.Register_Initial_Reference
        ("ORBPolicyManager",
         Ref);
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
       Init      => Deferred_Initialization'Access));
end CORBA.PolicyManager;
