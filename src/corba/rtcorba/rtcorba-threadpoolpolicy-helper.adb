------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      R T C O R B A . T H R E A D P O O L P O L I C Y . H E L P E R       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2017, Free Software Foundation, Inc.          --
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

with CORBA.Policy;

with PolyORB.CORBA_P.Policy;
with PolyORB.Smart_Pointers;

package body RTCORBA.ThreadpoolPolicy.Helper is

   use type CORBA.PolicyType;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.ThreadpoolPolicy.Local_Ref
   is
      Result : RTCORBA.ThreadpoolPolicy.Local_Ref;

   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));

      return Result;
   end Unchecked_To_Local_Ref;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : CORBA.Object.Ref'Class)
     return RTCORBA.ThreadpoolPolicy.Local_Ref
   is
      use PolyORB.CORBA_P.Policy;

   begin
      --        if CORBA.Object.Is_Nil (The_Ref)
      --          or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
      --           return Unchecked_To_Local_Ref (The_Ref);
      --        end if;
      --        CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);

      if The_Ref not in CORBA.Policy.Ref'Class
        or else CORBA.Policy.Get_Policy_Type (CORBA.Policy.Ref (The_Ref))
        /= THREADPOOL_POLICY_TYPE
      then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         Entity : constant PolyORB.Smart_Pointers.Entity_Ptr
           := new Policy_Object_Type;

         Result : Local_Ref;
      begin
         Set_Policy_Type (Policy_Object_Type (Entity.all),
                          THREADPOOL_POLICY_TYPE);

         Set_Policy_Value (Policy_Object_Type (Entity.all),
                           Get_Policy_Value
                           (Policy_Object_Type
                            (CORBA.Policy.Entity_Of
                             (CORBA.Policy.Ref (The_Ref)).all)));

         CORBA.Policy.Set (CORBA.Policy.Ref (Result), Entity);

         return Result;
      end;
   end To_Local_Ref;

end RTCORBA.ThreadpoolPolicy.Helper;
