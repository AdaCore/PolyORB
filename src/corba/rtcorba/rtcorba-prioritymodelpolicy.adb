------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          R T C O R B A . P R I O R I T Y M O D E L P O L I C Y           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with PolyORB.CORBA_P.POA_Config;
with PolyORB.CORBA_P.Policy;
with PolyORB.CORBA_P.Policy_Management;

with PolyORB.Initialization;

with PolyORB.POA_Policies;
with PolyORB.Smart_Pointers;
with PolyORB.RTCORBA_P.PriorityModelPolicy;
with PolyORB.RTCORBA_P.Setup;
with PolyORB.Tasking.Priorities;
with PolyORB.Utils.Strings;

with RTCORBA.PriorityMapping;
with RTCORBA.PriorityModelPolicy.Helper;

package body RTCORBA.PriorityModelPolicy is

   use CORBA;
   use CORBA.TypeCode;

   use PolyORB.CORBA_P.Policy;
   use PolyORB.CORBA_P.Policy_Management;

   use PolyORB.RTCORBA_P.PriorityModelPolicy;

   function Priority_Model_Policy_Allocator
     (Self : CORBA.Policy.Ref)
     return PolyORB.POA_Policies.Policy_Access;

   function Create_PriorityModelPolicy
     (The_Type : CORBA.PolicyType;
      Value    : CORBA.Any)
     return CORBA.Policy.Ref;

   -------------------------------------
   -- Priority_Model_Policy_Allocator --
   -------------------------------------

   function Priority_Model_Policy_Allocator
     (Self : CORBA.Policy.Ref)
     return PolyORB.POA_Policies.Policy_Access
   is
      use type PolyORB.RTCORBA_P.Setup.PriorityMapping_Access;
      use PolyORB.Tasking.Priorities;
      use RTCORBA.PriorityModelPolicy.Helper;

      Priority_Mapping : constant
        PolyORB.RTCORBA_P.Setup.PriorityMapping_Access
        := PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping;

      Success : CORBA.Boolean;
      New_Priority : RTCORBA.NativePriority;

   begin
      --  Compute new priority

      if Priority_Mapping = null then
         CORBA.Raise_Internal (CORBA.Default_Sys_Member);
      end if;

      RTCORBA.PriorityMapping.To_Native
        (Priority_Mapping.all,
         Get_Server_Priority (To_Local_Ref (Self)),
         New_Priority,
         Success);

      if not Success then
         CORBA.Raise_Data_Conversion
           (CORBA.System_Exception_Members'(Minor     => 2,
                                            Completed => CORBA.Completed_No));
      end if;

      return Create (Get_Priority_Model (To_Local_Ref (Self)),
                     ORB_Priority (New_Priority),
                     External_Priority (Get_Server_Priority
                                        (To_Local_Ref (Self))));
   end Priority_Model_Policy_Allocator;

   ---------------------------------
   -- Create_PriorityModelPolicy --
   ---------------------------------

   function Create_PriorityModelPolicy
     (The_Type : CORBA.PolicyType;
      Value    : CORBA.Any)
     return CORBA.Policy.Ref
   is
   begin
      pragma Assert (The_Type = PRIORITY_MODEL_POLICY_TYPE);

      if Get_Type (Value) /= TC_Unsigned_Long then
         Raise_PolicyError ((Reason => BAD_POLICY_TYPE));
      end if;

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
   end Create_PriorityModelPolicy;

   ------------------------
   -- Get_Priority_Model --
   ------------------------

   function Get_Priority_Model
     (Self : Local_Ref)
     return RTCORBA.PriorityModel
   is
   begin
      return Get_Priority_Model
        (PriorityModelPolicy_Type (Entity_Of (Self).all));
   end Get_Priority_Model;

   -------------------------
   -- Get_Server_Priority --
   -------------------------

   function Get_Server_Priority
     (Self : Local_Ref)
     return RTCORBA.Priority
   is
   begin
      return Get_Server_Priority
        (PriorityModelPolicy_Type (Entity_Of (Self).all));
   end Get_Server_Priority;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.POA_Config.Register
        (PRIORITY_MODEL_POLICY_TYPE,
         Priority_Model_Policy_Allocator'Access);

      Register
        (The_Type       => PRIORITY_MODEL_POLICY_TYPE,
         POA_Level      => True,
         Factory        => Create_PriorityModelPolicy'Access,
         System_Default =>
           Create_PriorityModelPolicy (PRIORITY_MODEL_POLICY_TYPE,
                                       To_Any (CORBA.Unsigned_Long (0))));
      --  XXX Is this correct? If policy can't be created with
      --  CORBA::create_policy then we must not register factory procedure.
      --  Also, created system default value is not compatible with policy
      --  implementation.
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"rtcorba-prioritymodelpolicy",
       Conflicts => Empty,
       Depends   => +"rt_poa",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end RTCORBA.PriorityModelPolicy;
