------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     POLYORB.GIOP_P.TAGGED_COMPONENTS.POLICIES.PRIORITY_MODEL_POLICY      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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
with PolyORB.Errors;
with PolyORB.ORB;
with PolyORB.Setup;
with PolyORB.RT_POA;
with PolyORB.RT_POA_Policies.Priority_Model_Policy;
with PolyORB.Tasking.Priorities;

package body PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy is

   Priority_Model_Policy_Type : constant := 40;
   --  Defined in RT-CORA specifications

   ---------------------
   -- Fetch_Component --
   ---------------------

   function Fetch_Component
     (Oid : access PolyORB.Objects.Object_Id)
     return Policy_Value;

   function Fetch_Component
     (Oid : access PolyORB.Objects.Object_Id)
     return Policy_Value
   is
      use PolyORB.Errors;
      use PolyORB.ORB;
      use PolyORB.RT_POA;
      use PolyORB.RT_POA_Policies.Priority_Model_Policy;
      use PolyORB.Tasking.Priorities;
      use Policy_Value_Seq;
      use PolyORB.Representations.CDR.Common;
      use type PolyORB.Types.Unsigned_Long;

      Result : Policy_Value;
      Buffer : Buffer_Access;

      Model           : Priority_Model;
      Server_ORB_Priority : ORB_Priority;
      Server_External_Priority : External_Priority;

      Error           : PolyORB.Errors.Error_Container;

   begin
      if Object_Adapter (PolyORB.Setup.The_ORB).all
        not in PolyORB.RT_POA.RT_Obj_Adapter'Class then
         return Policy_Value'(P_Type => Invalid_Policy_Type, P_Value => null);
      end if;

      Buffer := new Buffer_Type;

      Get_Scheduling_Parameters
        (RT_Obj_Adapter_Access (Object_Adapter (PolyORB.Setup.The_ORB)),
         PolyORB.Objects.Object_Id_Access (Oid),
         Model,
         Server_ORB_Priority,
         Server_External_Priority,
         Error);

      if Found (Error) then
         Catch (Error);
         return Policy_Value'(P_Type => Invalid_Policy_Type, P_Value => null);
      end if;

      Start_Encapsulation (Buffer);
      Marshall (Buffer,
                PolyORB.Types.Unsigned_Long
                (Priority_Model'Pos (Model)));
      Marshall (Buffer, PolyORB.Types.Short (Server_External_Priority));

      Result := Policy_Value'(P_Type => Priority_Model_Policy_Type,
                              P_Value =>
                                new Encapsulation'(Encapsulate (Buffer)));

      Release (Buffer);

      return Result;
   end Fetch_Component;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register (Fetch_Component'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tagged_components.policies.priority_model_policy",
       Conflicts => PolyORB.Initialization.String_Lists.Empty,
       Depends   => PolyORB.Initialization.String_Lists.Empty,
       Provides  => PolyORB.Initialization.String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.GIOP_P.Tagged_Components.Policies.Priority_Model_Policy;
