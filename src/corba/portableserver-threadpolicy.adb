------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O R T A B L E S E R V E R . T H R E A D P O L I C Y           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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

--  $Id$

package body PortableServer.ThreadPolicy is

   --------------------------
   -- Create_Thread_Policy --
   --------------------------

   function Create_Thread_Policy
     (Value : in PortableServer.ThreadPolicyValue)
     return CORBA.Policy.Ref'Class
   is
      Result : PortableServer.ThreadPolicy.Ref;
   begin
      Result.Type_Of_Ref := THREAD_POLICY_ID;
      Result.ThreadPolicy := Value;

      return Result;
   end Create_Thread_Policy;

   -------------------
   -- Create_Policy --
   -------------------

   function Create_Policy
     (The_Type : in CORBA.PolicyType;
      Val      : CORBA.Any)
     return PortableServer.ThreadPolicy.Ref
   is
      use CORBA;

   begin
      if The_Type /= THREAD_POLICY_ID then
         raise Program_Error;
      end if;

      return PortableServer.ThreadPolicy.Ref
        (Create_Thread_Policy (From_Any (Val)));
   end Create_Policy;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : Ref)
     return PortableServer.ThreadPolicyValue is
   begin
      return Self.ThreadPolicy;
   end Get_Value;

end PortableServer.ThreadPolicy;
