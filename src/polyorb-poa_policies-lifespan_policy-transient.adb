------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.POA_POLICIES.LIFESPAN_POLICY.TRANSIENT               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

with PolyORB.CORBA_P.Exceptions; use PolyORB.CORBA_P.Exceptions;
--  XXX remove dep on CORBA_P!

with PolyORB.POA;
with PolyORB.Types;

package body PolyORB.POA_Policies.Lifespan_Policy.Transient is

   use PolyORB.Types;

   ------------
   -- Create --
   ------------

   function Create return Transient_Policy_Access is
   begin
      return new Transient_Policy;
   end Create;

   -------------------------
   -- Check_Compatibility --
   -------------------------

   procedure Check_Compatibility
     (Self : Transient_Policy;
      OA   : PolyORB.POA_Types.Obj_Adapter_Access)
   is
   begin
      null;
   end Check_Compatibility;

   ---------------
   -- Policy_Id --
   ---------------

   function Policy_Id
     (Self : Transient_Policy)
     return String is
   begin
      return "LIFESPAN_POLICY.TRANSIENT";
   end Policy_Id;

   --------------------
   -- Get_Time_Stamp --
   --------------------

   function Get_Time_Stamp
     (P  : Transient_Policy;
      OA : PolyORB.POA_Types.Obj_Adapter_Access)
     return Time_Stamp is
   begin
      return PolyORB.POA.Obj_Adapter_Access (OA).Boot_Time;
   end Get_Time_Stamp;

   ---------------------
   -- Ensure_Lifespan --
   ---------------------

   procedure Ensure_Lifespan
     (P     : Transient_Policy;
      OA    : PolyORB.POA_Types.Obj_Adapter_Access;
      U_Oid : Unmarshalled_Oid_Access) is
   begin
      if U_Oid.Persistency_Flag
        /= PolyORB.POA.Obj_Adapter_Access (OA).Boot_Time
      then
         Raise_Bad_Param;
      end if;
   end Ensure_Lifespan;

   ----------
   -- Free --
   ----------

   procedure Free
     (P   : in     Transient_Policy;
      Ptr : in out Policy_Access)
   is
   begin
      Free (Transient_Policy_Access (Ptr));
   end Free;

end PolyORB.POA_Policies.Lifespan_Policy.Transient;
