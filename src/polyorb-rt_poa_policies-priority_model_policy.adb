------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.RT_POA_POLICIES.PRIORITY_MODEL_POLICY               --
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

with PolyORB.Annotations;

package body PolyORB.RT_POA_Policies.Priority_Model_Policy is

   type Priority_Model_Policy_Note is new PolyORB.Annotations.Note with record
      Model                    : Priority_Model;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority;
   end record;

   Default_PMP : constant Priority_Model_Policy_Note
     := (PolyORB.Annotations.Note
         with Model => SERVER_DECLARED,
         Server_ORB_Priority => ORB_Priority'Last,
         Server_External_Priority => Invalid_Priority);

   ------------
   -- Create --
   ------------

   function Create
     (Model                    : Priority_Model;
      Server_ORB_Priority      : ORB_Priority;
      Server_External_Priority : External_Priority)
     return Policy_Access
   is
      Result : constant Policy_Access
        := new PriorityModelPolicy (Model => Model);

      TResult : PriorityModelPolicy renames PriorityModelPolicy (Result.all);

   begin
      TResult.Server_ORB_Priority := Server_ORB_Priority;
      TResult.Server_External_Priority := Server_External_Priority;

      return Result;
   end Create;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id (Self : PriorityModelPolicy) return String is
   begin
      return "PRIORITY_MODEL_POLICY_" & Priority_Model'Image (Self.Model);
   end Policy_Id;

   ------------------------
   -- Check_Compatiblity --
   ------------------------

   procedure Check_Compatibility
     (Self           :        PriorityModelPolicy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);
      pragma Warnings (On);

   begin
      --  No rule to test

      null;
   end Check_Compatibility;

   --------------------------------------
   -- Get_Servant_Priority_Information --
   --------------------------------------

   procedure Get_Servant_Priority_Information
     (Servant                  :        Servants.Servant_Access;
      Model                    :    out Priority_Model;
      Server_ORB_Priority      :    out ORB_Priority;
      Server_External_Priority :    out External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (Servant);

      Note : Priority_Model_Policy_Note;

   begin
      PolyORB.Annotations.Get_Note (Notepad.all, Note, Default_PMP);

      if Note /= Default_PMP then
         Model := Note.Model;
         Server_ORB_Priority := Note.Server_ORB_Priority;
         Server_External_Priority := Note.Server_External_Priority;
      else
         Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
         return;
      end if;
   end Get_Servant_Priority_Information;

   --------------------------------------
   -- Set_Servant_Priority_Information --
   --------------------------------------

   procedure Set_Servant_Priority_Information
     (Self    : PriorityModelPolicy;
      Servant : PolyORB.Servants.Servant_Access)
   is
      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (Servant);

      Note : Priority_Model_Policy_Note;

   begin
      PolyORB.Annotations.Get_Note (Notepad.all, Note, Default_PMP);

      if Note = Default_PMP then
         Note.Model := Self.Model;
         Note.Server_ORB_Priority := Self.Server_ORB_Priority;
         Note.Server_External_Priority := Self.Server_External_Priority;

         PolyORB.Annotations.Set_Note (Notepad.all, Note);
      end if;
   end Set_Servant_Priority_Information;

   procedure Set_Servant_Priority_Information
     (Self                     :        PriorityModelPolicy;
      Servant                  :        Servants.Servant_Access;
      Server_ORB_Priority      :        ORB_Priority;
      Server_External_Priority :        External_Priority;
      Error                    : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.Errors;

      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (Servant);

      Note : Priority_Model_Policy_Note;

   begin
      if Self.Model /= SERVER_DECLARED then
         Throw (Error, WrongPolicy_E, Null_Members'(Null_Member));
         return;
      end if;

      Note.Model := SERVER_DECLARED;
      Note.Server_ORB_Priority := Server_ORB_Priority;
      Note.Server_External_Priority := Server_External_Priority;

      PolyORB.Annotations.Set_Note (Notepad.all, Note);
   end Set_Servant_Priority_Information;

end PolyORB.RT_POA_Policies.Priority_Model_Policy;
