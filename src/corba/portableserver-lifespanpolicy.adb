------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O R T A B L E S E R V E R . L I F E S P A N P O L I C Y         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  $Id$

package body PortableServer.LifespanPolicy is

   ----------------------------
   -- Create_Lifespan_Policy --
   ----------------------------

   function Create_Lifespan_Policy
     (Value : in PortableServer.LifespanPolicyValue)
     return CORBA.Policy.Ref'Class
   is
      Result : PortableServer.LifespanPolicy.Ref;
   begin
      Result.Type_Of_Ref := LIFESPAN_POLICY_ID;
      Result.LifespanPolicy := Value;

      return Result;
   end Create_Lifespan_Policy;

   -------------------
   -- Create_Policy --
   -------------------

   function Create_Policy
     (The_Type : in CORBA.PolicyType;
      Val      : CORBA.Any)
     return PortableServer.LifespanPolicy.Ref
   is
      use CORBA;

   begin
      if The_Type /= LIFESPAN_POLICY_ID then
         raise Program_Error;
      end if;

      return PortableServer.LifespanPolicy.Ref
        (Create_Lifespan_Policy (From_Any (Val)));
   end Create_Policy;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : Ref)
     return PortableServer.LifespanPolicyValue is
   begin
      return Self.LifespanPolicy;
   end Get_Value;

end PortableServer.LifespanPolicy;
