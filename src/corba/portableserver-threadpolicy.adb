------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O R T A B L E S E R V E R . T H R E A D P O L I C Y           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.Policy;
with PolyORB.CORBA_P.Policy_Management;
with PortableServer.Helper;
with PolyORB.Initialization;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;

package body PortableServer.ThreadPolicy is

   use CORBA;
   use CORBA.Policy;
   use CORBA.TypeCode;
   use PortableServer.Helper;
   use PolyORB.CORBA_P.Policy;
   use PolyORB.CORBA_P.Policy_Management;

   function Create_ThreadPolicy
     (The_Type : CORBA.PolicyType;
      Value    : CORBA.Any)
     return CORBA.Policy.Ref;

   ------------
   -- To_Ref --
   ------------

   function To_Ref (The_Ref : CORBA.Object.Ref'Class) return Ref is
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
     (The_Type : CORBA.PolicyType;
      Value    : CORBA.Any)
     return CORBA.Policy.Ref
   is
   begin
      pragma Assert (The_Type = THREAD_POLICY_ID);

      if Get_Type (Value) /= TC_ThreadPolicyValue then
         Raise_PolicyError ((Reason => BAD_POLICY_TYPE));
      end if;

      declare
         Position : constant CORBA.Unsigned_Long :=
           CORBA.From_Any
             (CORBA.Internals.Get_Aggregate_Element
                (Value,
                 CORBA.TC_Unsigned_Long,
                 CORBA.Unsigned_Long (0)));
      begin
         if Position > ThreadPolicyValue'Pos (ThreadPolicyValue'Last) then
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
       Depends   => +"PortableServer.Helper",
       Provides  => Empty,
       Implicit  => False,
       Init      => Deferred_Initialization'Access,
       Shutdown  => null));
end PortableServer.ThreadPolicy;
