------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.LIFESPAN_POLICY.PERSISTENT              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
     (Self           : Persistent_Policy;
      Other_Policies : AllPolicies;
      Error          : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Unreferenced (Other_Policies);
      pragma Unreferenced (Error);
      pragma Warnings (On);

   begin
      null;
      --  No rule to test.
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Persistent_Policy)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      return "LIFESPAN_POLICY.PERSISTENT";
   end Policy_Id;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Lifespan_Cookie
     (Self : Persistent_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
     return Time_Stamp
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA);
      pragma Warnings (On);

   begin
      return Null_Time_Stamp;
   end Get_Lifespan_Cookie;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   procedure Ensure_Lifespan
     (Self  :        Persistent_Policy;
      OA    :        PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid :        Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, OA);
      pragma Warnings (On);

      use PolyORB.Exceptions;

   begin
      if U_Oid.Persistency_Flag /= Null_Time_Stamp then
         Throw (Error,
                Object_Not_Exist_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
      end if;
   end Ensure_Lifespan;

end PolyORB.POA_Policies.Lifespan_Policy.Persistent;
