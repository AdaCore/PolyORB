------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   R T C O R B A . P R I O R I T Y M O D E L P O L I C Y . H E L P E R    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

package body RTCORBA.PriorityModelPolicy.Helper is

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.PriorityModelPolicy.Local_Ref
   is
      Result : RTCORBA.PriorityModelPolicy.Local_Ref;

   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));

      return Result;
   end Unchecked_To_Local_Ref;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.PriorityModelPolicy.Local_Ref is
   begin
      --        if CORBA.Object.Is_Nil (The_Ref)
      --          or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
      --           return Unchecked_To_Local_Ref (The_Ref);
      --        end if;
      --        CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);

      if The_Ref not in CORBA.Policy.Ref'Class
        or else CORBA.Policy.Get_Policy_Type (CORBA.Policy.Ref (The_Ref))
        /= PRIORITY_MODEL_POLICY_TYPE
      then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         Result : Local_Ref;

      begin
         CORBA.Policy.Set (CORBA.Policy.Ref (Result),
                           CORBA.Object.Entity_Of (The_Ref));

         return Result;
      end;

   end To_Local_Ref;

end RTCORBA.PriorityModelPolicy.Helper;
