------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.LIFESPAN_POLICY.PERSISTENT              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.POA;
with PolyORB.Types;

package body PolyORB.POA_Policies.Lifespan_Policy.Persistent is

   use PolyORB.Types;

   ------------
   -- Create --
   ------------

   function Create return Persistent_Policy_Access is
   begin
      return new Persistent_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Persistent_Policy;
      Other_Policies   : AllPolicies)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Warnings (On);
      null;
      --  XXX Is this OK for policy PERSISTENT??
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Persistent_Policy)
     return String is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
      return "LIFESPAN_POLICY.PERSISTENT";
   end Policy_Id;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Lifespan_Cookie
     (Self : Persistent_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
     return Time_Stamp is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA);
      pragma Warnings (On);
      return 0;
   end Get_Lifespan_Cookie;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   procedure Ensure_Lifespan
     (Self  : Persistent_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA);
      pragma Warnings (On);
      if U_Oid.Persistency_Flag /= 0 then
         raise PolyORB.POA.Object_Not_Exist;
      end if;
   end Ensure_Lifespan;

end PolyORB.POA_Policies.Lifespan_Policy.Persistent;
