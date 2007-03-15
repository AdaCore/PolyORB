------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.RT_POA_POLICIES.THREAD_POOL_POLICY                 --
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

package body PolyORB.RT_POA_Policies.Thread_Pool_Policy is

   type Thread_Pool_Policy_Note is new PolyORB.Annotations.Note with record
      Lanes : PolyORB.Lanes.Lane_Root_Access;
   end record;

   Default_TPP : constant Thread_Pool_Policy_Note
     := (PolyORB.Annotations.Note with Lanes => null);

   ------------
   -- Create --
   ------------

   function Create (Lanes : Lane_Root_Access) return Policy_Access is
      Result : constant Policy_Access := new ThreadPoolPolicy;

      TResult : ThreadPoolPolicy renames ThreadPoolPolicy (Result.all);
   begin
      TResult.Lanes := Lanes;

      return Result;
   end Create;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id (Self : ThreadPoolPolicy) return String is
      pragma Unreferenced (Self);

   begin
      return "THREAD_POOL_POLICY";
   end Policy_Id;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self           :        ThreadPoolPolicy;
      Other_Policies :        AllPolicies;
      Error          : in out PolyORB.Errors.Error_Container)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);

   begin
      null;
   end Check_Compatibility;

   ----------------------
   -- Get_Servant_Lane --
   ----------------------

   function Get_Servant_Lane
     (Servant : PolyORB.Servants.Servant_Access)
     return Lane_Root_Access
   is
      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (Servant);

      Note : Thread_Pool_Policy_Note;

   begin
      PolyORB.Annotations.Get_Note (Notepad.all, Note, Default_TPP);

      if Note /= Default_TPP then
         return Note.Lanes;
      else
         return null;
      end if;
   end Get_Servant_Lane;

   ----------------------
   -- Set_Servant_Lane --
   ----------------------

   procedure Set_Servant_Lane
     (Self    : ThreadPoolPolicy;
      Servant : PolyORB.Servants.Servant_Access)
   is
      Notepad : constant PolyORB.Annotations.Notepad_Access
        := PolyORB.Servants.Notepad_Of (Servant);

      Note : Thread_Pool_Policy_Note;

   begin
      PolyORB.Annotations.Get_Note (Notepad.all, Note, Default_TPP);

      if Note = Default_TPP then
         Note.Lanes := Self.Lanes;

         PolyORB.Annotations.Set_Note (Notepad.all, Note);
      end if;
   end Set_Servant_Lane;

   -----------------------
   -- Is_Valid_Priority --
   -----------------------

   function Is_Valid_Priority
     (Self     : ThreadPoolPolicy;
      Priority : External_Priority)
     return Boolean
   is
   begin
      return Is_Valid_Priority (Self.Lanes, Priority);
   end Is_Valid_Priority;

end PolyORB.RT_POA_Policies.Thread_Pool_Policy;
