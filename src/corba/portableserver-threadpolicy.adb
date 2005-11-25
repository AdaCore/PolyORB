------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O R T A B L E S E R V E R . T H R E A D P O L I C Y           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.CORBA_P.Policy;
with PolyORB.CORBA_P.Policy_Management;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PortableServer.ThreadPolicy is

   use CORBA;
   use CORBA.Policy;
   use CORBA.TypeCode;
   use PolyORB.CORBA_P.Policy;
   use PolyORB.CORBA_P.Policy_Management;

   function Create_ThreadPolicy
     (The_Type : in CORBA.PolicyType;
      Value    : in CORBA.Any)
     return CORBA.Policy.Ref;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Ref
   is
      use type CORBA.PolicyType;

   begin
      if The_Ref not in CORBA.Policy.Ref'Class
        or else Get_Policy_Type (CORBA.Policy.Ref (The_Ref)) /=
        THREAD_POLICY_ID
      then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         Entity : constant PolyORB.Smart_Pointers.Entity_Ptr :=
           new Policy_Object_Type;

         Result : Ref;
      begin
         Set_Policy_Type (Policy_Object_Type (Entity.all),
                          THREAD_POLICY_ID);

         Set_Policy_Value (Policy_Object_Type (Entity.all),
                           Get_Policy_Value
                           (Policy_Object_Type
                            (Entity_Of
                             (CORBA.Policy.Ref (The_Ref)).all)));

         CORBA.Policy.Set (CORBA.Policy.Ref (Result), Entity);

         return Result;
      end;

   end To_Ref;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : Ref)
     return PortableServer.ThreadPolicyValue is
   begin
      return From_Any (Get_Policy_Value
                       (Policy_Object_Type
                        (Entity_Of
                         (CORBA.Policy.Ref (Self)).all)));
   end Get_Value;

   -------------------------
   -- Create_ThreadPolicy --
   -------------------------

   function Create_ThreadPolicy
     (The_Type : in CORBA.PolicyType;
      Value    : in CORBA.Any)
     return CORBA.Policy.Ref
   is
   begin
      pragma Assert (The_Type = THREAD_POLICY_ID);

      if Get_Type (Value) /= TC_ThreadPolicyValue then
         Raise_PolicyError ((Reason => BAD_POLICY_TYPE));
      end if;

      declare
         Index : CORBA.Any
           := CORBA.Internals.Get_Aggregate_Element (Value,
                                                     CORBA.TC_Unsigned_Long,
                                                     CORBA.Unsigned_Long (0));
         Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
      begin
         if Position not in
           ThreadPolicyValue'Pos (ThreadPolicyValue'First) ..
           ThreadPolicyValue'Pos (ThreadPolicyValue'Last)
         then
            Raise_PolicyError ((Reason => BAD_POLICY_VALUE));
         end if;
      end;

      declare
         Result : CORBA.Policy.Ref;
         Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
           := new Policy_Object_Type;
      begin
         Set_Policy_Type (Policy_Object_Type (Entity.all), The_Type);
         Set_Policy_Value (Policy_Object_Type (Entity.all), Value);

         CORBA.Policy.Set (Result, Entity);

         return Result;
      end;
   end Create_ThreadPolicy;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
   begin
      Register
        (The_Type       => THREAD_POLICY_ID,
         POA_Level      => True,
         Factory        => Create_ThreadPolicy'Access,
         System_Default => Create_ThreadPolicy (THREAD_POLICY_ID,
                                                To_Any (ORB_CTRL_MODEL)));
   end Deferred_Initialization;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"portableserver.threadpolicy",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Implicit  => False,
       Init      => Deferred_Initialization'Access));
end PortableServer.ThreadPolicy;
